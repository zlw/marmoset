use std::path::{Path, PathBuf};

use zed_extension_api::{self as zed, LanguageServerId, Result};

struct MarmosetExtension;

#[derive(Clone, Debug, PartialEq, Eq)]
struct RepoBinaryLaunch {
    path: String,
    marmoset_root: String,
}

fn parent_dir(path: &Path) -> Option<&Path> {
    path.parent().filter(|parent| *parent != path)
}

fn ancestor_dirs(start_dir: &Path) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    let mut current = Some(start_dir);

    while let Some(dir) = current {
        dirs.push(dir.to_path_buf());
        current = parent_dir(dir);
    }

    dirs
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

fn pwd_from_env(env: &[(String, String)]) -> Option<PathBuf> {
    env_value(env, "PWD")
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
}

fn marmoset_roots_from_path_binary_entries(env: &[(String, String)]) -> Vec<PathBuf> {
    env_value(env, "PATH")
        .map(|path_value| {
            path_value
                .split(':')
                .filter(|entry| !entry.is_empty())
                .map(PathBuf::from)
                .filter_map(|entry| {
                    if entry.file_name().and_then(|name| name.to_str()) == Some("marmoset") {
                        entry.parent().map(Path::to_path_buf)
                    } else {
                        None
                    }
                })
                .collect()
        })
        .unwrap_or_default()
}

fn push_candidate_root(candidate_roots: &mut Vec<PathBuf>, repo_root: PathBuf) {
    if repo_root.as_os_str().is_empty() {
        return;
    }

    if !candidate_roots
        .iter()
        .any(|candidate_root| candidate_root == &repo_root)
    {
        candidate_roots.push(repo_root);
    }
}

fn push_nested_repo_candidates(candidate_roots: &mut Vec<PathBuf>, root: &Path) {
    if root.as_os_str().is_empty() {
        return;
    }

    push_candidate_root(candidate_roots, root.join("marmoset"));
}

fn repo_dev_binary(
    worktree_root: &Path,
    shell_env: &[(String, String)],
    mut is_launchable: impl FnMut(&str, &[(String, String)]) -> bool,
) -> Option<RepoBinaryLaunch> {
    let mut candidate_roots = Vec::new();

    if let Some(repo_root) = marmoset_root_from_env(shell_env) {
        push_candidate_root(&mut candidate_roots, repo_root);
    }

    if let Some(repo_root) = pwd_from_env(shell_env) {
        push_candidate_root(&mut candidate_roots, repo_root);
    }

    if let Some(repo_root) = marmoset_root_from_env(shell_env) {
        push_nested_repo_candidates(&mut candidate_roots, repo_root.as_path());
    }

    if let Some(repo_root) = pwd_from_env(shell_env) {
        push_nested_repo_candidates(&mut candidate_roots, repo_root.as_path());
    }

    for repo_root in ancestor_dirs(worktree_root) {
        push_candidate_root(&mut candidate_roots, repo_root.clone());
        push_nested_repo_candidates(&mut candidate_roots, repo_root.as_path());
    }

    for repo_root in marmoset_roots_from_path_binary_entries(shell_env) {
        push_candidate_root(&mut candidate_roots, repo_root);
    }

    for repo_root in candidate_roots {
        let candidate = repo_binary_launch(&repo_root);
        let env = env_with_marmoset_root(shell_env, &candidate.marmoset_root);
        if is_launchable(&candidate.path, &env) {
            return Some(candidate);
        }
    }

    None
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
        let worktree_root = worktree.root_path();
        let launch = repo_dev_binary(Path::new(&worktree_root), &shell_env, |path, env| {
            launchable_binary(path, env)
        });

        let (command, env) = match launch {
            Some(launch) => {
                let env = env_with_marmoset_root(&shell_env, &launch.marmoset_root);
                (launch.path, env)
            }
            None => {
                let command = worktree.which("marmoset").ok_or_else(|| {
                    "marmoset not found in PATH. Install it with: dune install".to_string()
                })?;
                (command, shell_env)
            }
        };

        Ok(zed::Command {
            command,
            args: vec!["lsp".into()],
            env,
        })
    }
}

zed::register_extension!(MarmosetExtension);

#[cfg(test)]
mod tests {
    use super::{env_with_marmoset_root, repo_binary_launch, repo_dev_binary, RepoBinaryLaunch};
    use std::path::PathBuf;

    fn worktree_root() -> PathBuf {
        PathBuf::from("/tmp/marmoset-dev/test/fixtures/modules")
    }

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
    fn uses_marmoset_root_env_before_worktree_ancestors() {
        let worktree_root = PathBuf::from("/tmp/unrelated/project");
        let shell_env = vec![("MARMOSET_ROOT".to_string(), "/tmp/marmoset-dev".to_string())];
        let repo_binary = "/tmp/marmoset-dev/marmoset".to_string();
        let mut probed = Vec::new();

        let selected = repo_dev_binary(worktree_root.as_path(), &shell_env, |path, env| {
            probed.push((path.to_string(), env.to_vec()));
            path == repo_binary
                && env
                    .iter()
                    .any(|(name, value)| name == "MARMOSET_ROOT" && value == "/tmp/marmoset-dev")
        });

        assert_eq!(
            selected,
            Some(RepoBinaryLaunch {
                path: repo_binary.clone(),
                marmoset_root: "/tmp/marmoset-dev".to_string(),
            })
        );
        assert_eq!(probed.first().map(|(path, _env)| path), Some(&repo_binary));
    }

    #[test]
    fn does_not_probe_plain_path_marmoset_for_empty_worktree_root() {
        let selected = repo_dev_binary(PathBuf::from("").as_path(), &[], |path, _env| {
            path == "marmoset"
        });

        assert_eq!(selected, None);
    }

    #[test]
    fn uses_marmoset_binary_path_entries_from_shell_path() {
        let worktree_root = PathBuf::from("");
        let shell_env = vec![(
            "PATH".to_string(),
            "/tmp/marmoset-dev/marmoset:/usr/bin".to_string(),
        )];
        let repo_binary = "/tmp/marmoset-dev/marmoset".to_string();
        let mut probed = Vec::new();

        let selected = repo_dev_binary(worktree_root.as_path(), &shell_env, |path, env| {
            probed.push((path.to_string(), env.to_vec()));
            path == repo_binary
                && env
                    .iter()
                    .any(|(name, value)| name == "MARMOSET_ROOT" && value == "/tmp/marmoset-dev")
        });

        assert_eq!(
            selected,
            Some(RepoBinaryLaunch {
                path: repo_binary.clone(),
                marmoset_root: "/tmp/marmoset-dev".to_string(),
            })
        );
        assert_eq!(probed.first().map(|(path, _env)| path), Some(&repo_binary));
    }

    #[test]
    fn uses_pwd_env_when_worktree_root_is_empty() {
        let worktree_root = PathBuf::from("");
        let shell_env = vec![
            ("PWD".to_string(), "/tmp/marmoset-dev".to_string()),
            (
                "PATH".to_string(),
                "/Users/zlw/.opam/default/bin:/usr/bin".to_string(),
            ),
        ];
        let repo_binary = "/tmp/marmoset-dev/marmoset".to_string();
        let mut probed = Vec::new();

        let selected = repo_dev_binary(worktree_root.as_path(), &shell_env, |path, env| {
            probed.push((path.to_string(), env.to_vec()));
            path == repo_binary
                && env
                    .iter()
                    .any(|(name, value)| name == "MARMOSET_ROOT" && value == "/tmp/marmoset-dev")
        });

        assert_eq!(
            selected,
            Some(RepoBinaryLaunch {
                path: repo_binary.clone(),
                marmoset_root: "/tmp/marmoset-dev".to_string(),
            })
        );
        assert_eq!(probed.first().map(|(path, _env)| path), Some(&repo_binary));
    }

    #[test]
    fn finds_repo_binary_when_parent_directory_is_opened_as_worktree() {
        let worktree_root = PathBuf::from("/tmp/marmoset-dev");
        let repo_binary = "/tmp/marmoset-dev/marmoset/marmoset".to_string();
        let mut probed = Vec::new();

        let selected = repo_dev_binary(worktree_root.as_path(), &[], |path, env| {
            probed.push((path.to_string(), env.to_vec()));
            path == repo_binary
                && env.iter().any(|(name, value)| {
                    name == "MARMOSET_ROOT" && value == "/tmp/marmoset-dev/marmoset"
                })
        });

        assert_eq!(
            selected,
            Some(RepoBinaryLaunch {
                path: repo_binary.clone(),
                marmoset_root: "/tmp/marmoset-dev/marmoset".to_string(),
            })
        );
        assert!(probed.iter().any(|(path, env)| path == &repo_binary
            && env.iter().any(|(name, value)| {
                name == "MARMOSET_ROOT" && value == "/tmp/marmoset-dev/marmoset"
            })));
    }

    #[test]
    fn finds_repo_root_binary_from_nested_worktree_root() {
        let worktree_root = worktree_root();
        let repo_binary = "/tmp/marmoset-dev/marmoset".to_string();
        let mut probed = Vec::new();

        let selected = repo_dev_binary(worktree_root.as_path(), &[], |path, env| {
            probed.push((path.to_string(), env.to_vec()));
            path == repo_binary
                && env
                    .iter()
                    .any(|(name, value)| name == "MARMOSET_ROOT" && value == "/tmp/marmoset-dev")
        });

        assert_eq!(
            selected,
            Some(RepoBinaryLaunch {
                path: repo_binary.clone(),
                marmoset_root: "/tmp/marmoset-dev".to_string(),
            })
        );
        assert!(probed.iter().any(|(path, env)| path == &repo_binary
            && env
                .iter()
                .any(|(name, value)| name == "MARMOSET_ROOT" && value == "/tmp/marmoset-dev")));
    }

    #[test]
    fn prefers_nearest_ancestor_repo_binary() {
        let worktree_root = worktree_root();
        let mut probed = Vec::new();

        let selected = repo_dev_binary(worktree_root.as_path(), &[], |path, _env| {
            probed.push(path.to_string());
            path == "/tmp/marmoset-dev/test/marmoset"
        });

        assert_eq!(
            selected,
            Some(RepoBinaryLaunch {
                path: "/tmp/marmoset-dev/test/marmoset".to_string(),
                marmoset_root: "/tmp/marmoset-dev/test".to_string(),
            })
        );
        assert_eq!(
            probed,
            vec![
                "/tmp/marmoset-dev/test/fixtures/modules/marmoset".to_string(),
                "/tmp/marmoset-dev/test/fixtures/modules/marmoset/marmoset".to_string(),
                "/tmp/marmoset-dev/test/fixtures/marmoset".to_string(),
                "/tmp/marmoset-dev/test/fixtures/marmoset/marmoset".to_string(),
                "/tmp/marmoset-dev/test/marmoset".to_string()
            ]
        );
    }

    #[test]
    fn returns_none_when_repo_binary_probe_rejects_all_ancestors() {
        let worktree_root = worktree_root();
        let mut probed = Vec::new();

        let selected = repo_dev_binary(worktree_root.as_path(), &[], |path, _env| {
            probed.push(path.to_string());
            false
        });

        assert_eq!(selected, None);
        assert_eq!(
            probed,
            vec![
                "/tmp/marmoset-dev/test/fixtures/modules/marmoset".to_string(),
                "/tmp/marmoset-dev/test/fixtures/modules/marmoset/marmoset".to_string(),
                "/tmp/marmoset-dev/test/fixtures/marmoset".to_string(),
                "/tmp/marmoset-dev/test/fixtures/marmoset/marmoset".to_string(),
                "/tmp/marmoset-dev/test/marmoset".to_string(),
                "/tmp/marmoset-dev/test/marmoset/marmoset".to_string(),
                "/tmp/marmoset-dev/marmoset".to_string(),
                "/tmp/marmoset-dev/marmoset/marmoset".to_string(),
                "/tmp/marmoset".to_string(),
                "/tmp/marmoset/marmoset".to_string(),
                "/marmoset".to_string(),
                "/marmoset/marmoset".to_string(),
            ]
        );
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

    #[test]
    fn does_not_depend_on_local_fs_visibility_for_repo_candidate() {
        let worktree_root = PathBuf::from("/nonexistent/worktree/test/fixtures/modules");
        let repo_binary = "/nonexistent/worktree/marmoset".to_string();

        let selected = repo_dev_binary(worktree_root.as_path(), &[], |path, env| {
            path == repo_binary
                && env.iter().any(|(name, value)| {
                    name == "MARMOSET_ROOT" && value == "/nonexistent/worktree"
                })
        });

        assert_eq!(
            selected,
            Some(RepoBinaryLaunch {
                path: repo_binary,
                marmoset_root: "/nonexistent/worktree".to_string(),
            })
        );
    }
}
