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

fn marmoset_root_launch(
    shell_env: &[(String, String)],
    mut is_launchable: impl FnMut(&str, &[(String, String)]) -> bool,
) -> Option<std::result::Result<RepoBinaryLaunch, String>> {
    let repo_root = marmoset_root_from_env(shell_env)?;
    let launch = repo_binary_launch(repo_root.as_path());
    let env = env_with_marmoset_root(shell_env, &launch.marmoset_root);

    if is_launchable(&launch.path, &env) {
        Some(Ok(launch))
    } else {
        Some(Err(format!(
            "MARMOSET_ROOT is set to {}, but {} is not launchable",
            launch.marmoset_root, launch.path
        )))
    }
}

// Zed runs this extension as wasm, so std::fs metadata checks do not reliably
// reflect which host binaries are actually launchable.
fn launchable_binary(path: &str, env: &[(String, String)]) -> bool {
    let mut command = zed::process::Command::new(path)
        .arg("--version")
        .envs(env.iter().cloned());

    match command.output() {
        Ok(output) => output.status == Some(0),
        Err(_) => false,
    }
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
        let launch = match marmoset_root_launch(&shell_env, |path, env| {
            launchable_binary(path, env)
        }) {
            Some(Ok(launch)) => launch,
            Some(Err(message)) => return Err(message),
            None => {
                return Err("MARMOSET_ROOT is not set; set it to the Marmoset repo root".to_string())
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
        let mut probed = Vec::new();

        let selected = marmoset_root_launch(&shell_env, |path, env| {
            probed.push((path.to_string(), env.to_vec()));
            path == repo_binary
                && env
                    .iter()
                    .any(|(name, value)| name == "MARMOSET_ROOT" && value == "/tmp/marmoset-dev")
        });

        assert_eq!(
            selected,
            Some(Ok(RepoBinaryLaunch {
                path: repo_binary.clone(),
                marmoset_root: "/tmp/marmoset-dev".to_string(),
            }))
        );
        assert_eq!(probed.first().map(|(path, _env)| path), Some(&repo_binary));
        assert_eq!(probed.len(), 1);
    }

    #[test]
    fn marmoset_root_env_failure_does_not_probe_nested_or_fallback_roots() {
        let shell_env = vec![("MARMOSET_ROOT".to_string(), "/tmp/marmoset-dev".to_string())];
        let mut probed = Vec::new();

        let selected = marmoset_root_launch(&shell_env, |path, _env| {
            probed.push(path.to_string());
            false
        });

        assert_eq!(
            selected,
            Some(Err(
                "MARMOSET_ROOT is set to /tmp/marmoset-dev, but /tmp/marmoset-dev/marmoset is not launchable"
                    .to_string()
            ))
        );
        assert_eq!(probed, vec!["/tmp/marmoset-dev/marmoset".to_string()]);
    }

    #[test]
    fn missing_marmoset_root_has_no_launcher_candidate() {
        let shell_env = vec![("PATH".to_string(), "/tmp/marmoset-dev/marmoset".to_string())];

        let selected = marmoset_root_launch(&shell_env, |_path, _env| {
            panic!("PATH must not be probed without MARMOSET_ROOT");
        });

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
