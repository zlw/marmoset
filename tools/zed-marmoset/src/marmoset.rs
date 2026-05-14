use std::path::{Path, PathBuf};

use zed_extension_api::{self as zed, LanguageServerId, Result};

struct MarmosetExtension;

#[derive(Clone, Debug, PartialEq, Eq)]
struct RepoBinaryLaunch {
    path: String,
    marmoset_root: String,
}

fn repo_binary_launch(repo_root: &Path) -> RepoBinaryLaunch {
    RepoBinaryLaunch {
        path: path_to_string(repo_root.join("marmoset")),
        marmoset_root: path_to_string(repo_root.to_path_buf()),
    }
}

fn env_with_marmoset_root(env: &[(String, String)], marmoset_root: &str) -> Vec<(String, String)> {
    let mut merged = Vec::with_capacity(env.len() + 1);
    let mut replaced = false;

    for (name, value) in env {
        if name == "MARMOSET_ROOT" {
            if !replaced {
                merged.push((name.clone(), marmoset_root.to_string()));
                replaced = true;
            }
        } else {
            merged.push((name.clone(), value.clone()));
        }
    }

    if !replaced {
        merged.push(("MARMOSET_ROOT".to_string(), marmoset_root.to_string()));
    }

    merged
}

fn env_value<'a>(env: &'a [(String, String)], name: &str) -> Option<&'a str> {
    env.iter().find_map(|(env_name, value)| {
        if env_name == name {
            Some(value.as_str())
        } else {
            None
        }
    })
}

fn marmoset_root_from_env(env: &[(String, String)]) -> Option<PathBuf> {
    env_value(env, "MARMOSET_ROOT")
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
}

fn marmoset_root_from_worktree_root(worktree_root: &str) -> Option<PathBuf> {
    if worktree_root.is_empty() {
        None
    } else {
        Some(PathBuf::from(worktree_root))
    }
}

fn marmoset_root_launch(
    shell_env: &[(String, String)],
    worktree_root: &str,
) -> Option<RepoBinaryLaunch> {
    let repo_root = marmoset_root_from_env(shell_env)
        .or_else(|| marmoset_root_from_worktree_root(worktree_root))?;
    Some(repo_binary_launch(repo_root.as_path()))
}

fn path_to_string(path: PathBuf) -> String {
    path.to_string_lossy().into_owned()
}

impl zed::Extension for MarmosetExtension {
    fn new() -> Self {
        MarmosetExtension
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let shell_env = worktree.shell_env();
        let worktree_root = worktree.root_path();
        let launch = match marmoset_root_launch(&shell_env, &worktree_root) {
            Some(launch) => launch,
            None => {
                return Err(
                    "Could not resolve Marmoset repo root; set MARMOSET_ROOT or open the Marmoset repo root in Zed"
                        .to_string(),
                )
            }
        };
        let env = env_with_marmoset_root(&shell_env, &launch.marmoset_root);

        Ok(zed::Command {
            command: launch.path,
            args: vec!["lsp".into()],
            env,
        })
    }
}

zed::register_extension!(MarmosetExtension);

#[cfg(test)]
mod tests {
    use super::{
        env_with_marmoset_root, marmoset_root_launch, repo_binary_launch, RepoBinaryLaunch,
    };
    use std::path::PathBuf;

    #[test]
    fn env_with_marmoset_root_appends_when_missing() {
        let env = vec![("PATH".to_string(), "/usr/bin".to_string())];

        assert_eq!(
            env_with_marmoset_root(&env, "/tmp/marmoset-dev"),
            vec![
                ("PATH".to_string(), "/usr/bin".to_string()),
                ("MARMOSET_ROOT".to_string(), "/tmp/marmoset-dev".to_string()),
            ]
        );
    }

    #[test]
    fn env_with_marmoset_root_replaces_existing_value() {
        let env = vec![
            ("PATH".to_string(), "/usr/bin".to_string()),
            ("MARMOSET_ROOT".to_string(), "/tmp/old-root".to_string()),
        ];

        assert_eq!(
            env_with_marmoset_root(&env, "/tmp/marmoset-dev"),
            vec![
                ("PATH".to_string(), "/usr/bin".to_string()),
                ("MARMOSET_ROOT".to_string(), "/tmp/marmoset-dev".to_string()),
            ]
        );
    }

    #[test]
    fn marmoset_root_env_uses_exact_configured_repo_binary() {
        let shell_env = vec![("MARMOSET_ROOT".to_string(), "/tmp/marmoset-dev".to_string())];
        let repo_binary = "/tmp/marmoset-dev/marmoset".to_string();
        let selected = marmoset_root_launch(&shell_env, "/tmp/marmoset-worktree");

        assert_eq!(
            selected,
            Some(RepoBinaryLaunch {
                path: repo_binary.clone(),
                marmoset_root: "/tmp/marmoset-dev".to_string(),
            })
        );
    }

    #[test]
    fn missing_marmoset_root_uses_worktree_repo_binary() {
        let shell_env = vec![("PATH".to_string(), "/tmp/marmoset-dev/marmoset".to_string())];

        let selected = marmoset_root_launch(&shell_env, "/tmp/marmoset-worktree");

        assert_eq!(
            selected,
            Some(RepoBinaryLaunch {
                path: "/tmp/marmoset-worktree/marmoset".to_string(),
                marmoset_root: "/tmp/marmoset-worktree".to_string(),
            })
        );
    }

    #[test]
    fn empty_worktree_root_has_no_launcher_candidate() {
        let shell_env = vec![("PATH".to_string(), "/tmp/marmoset-dev/marmoset".to_string())];

        let selected = marmoset_root_launch(&shell_env, "");

        assert_eq!(selected, None);
    }

    #[test]
    fn repo_binary_launch_uses_repo_root_marmoset_path() {
        assert_eq!(
            repo_binary_launch(PathBuf::from("/tmp/marmoset-dev").as_path()),
            RepoBinaryLaunch {
                path: "/tmp/marmoset-dev/marmoset".to_string(),
                marmoset_root: "/tmp/marmoset-dev".to_string(),
            }
        );
    }
}
