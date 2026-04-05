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

fn repo_dev_binary_candidates(worktree_root: &Path) -> Vec<RepoBinaryLaunch> {
    ancestor_dirs(worktree_root)
        .into_iter()
        .flat_map(|repo_root| {
            let root = path_to_string(repo_root.clone());
            [
                RepoBinaryLaunch {
                    path: path_to_string(repo_root.join("marmoset")),
                    marmoset_root: root.clone(),
                },
                RepoBinaryLaunch {
                    path: path_to_string(repo_root.join("_build").join("default").join("bin").join("main.exe")),
                    marmoset_root: root.clone(),
                },
                RepoBinaryLaunch {
                    path: path_to_string(
                        repo_root
                            .join("_build")
                            .join("install")
                            .join("default")
                            .join("bin")
                            .join("marmoset"),
                    ),
                    marmoset_root: root,
                },
            ]
        })
        .collect()
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

fn repo_dev_binary(
    worktree_root: &Path,
    shell_env: &[(String, String)],
    mut is_launchable: impl FnMut(&str, &[(String, String)]) -> bool,
) -> Option<RepoBinaryLaunch> {
    for candidate in repo_dev_binary_candidates(worktree_root) {
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
                let command = worktree
                    .which("marmoset")
                    .ok_or_else(|| "marmoset not found in PATH. Install it with: dune install".to_string())?;
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
    use super::{env_with_marmoset_root, repo_dev_binary, repo_dev_binary_candidates, RepoBinaryLaunch};
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
        assert!(probed
            .iter()
            .any(|(path, env)| path == &repo_binary
                && env
                    .iter()
                    .any(|(name, value)| name == "MARMOSET_ROOT" && value == "/tmp/marmoset-dev")));
    }

    #[test]
    fn falls_back_to_build_main_exe_when_repo_root_binary_is_not_launchable() {
        let worktree_root = worktree_root();
        let mut candidates = repo_dev_binary_candidates(&worktree_root)
            .into_iter()
            .filter(|candidate| candidate.marmoset_root == "/tmp/marmoset-dev");
        let repo_binary = candidates.next().unwrap();
        let main_exe = candidates.next().unwrap();
        let install_bin = candidates.next().unwrap();
        let mut probed = Vec::new();

        let selected = repo_dev_binary(worktree_root.as_path(), &[], |path, _env| {
            probed.push(path.to_string());
            path == main_exe.path
        });

        assert_eq!(selected, Some(main_exe.clone()));
        assert!(probed.ends_with(&vec![repo_binary.path, main_exe.path]));
        assert!(!probed.ends_with(&vec![install_bin.path]));
    }

    #[test]
    fn falls_back_to_installed_build_path_when_launch_probe_accepts_it() {
        let worktree_root = worktree_root();
        let mut candidates = repo_dev_binary_candidates(&worktree_root)
            .into_iter()
            .filter(|candidate| candidate.marmoset_root == "/tmp/marmoset-dev");
        let repo_binary = candidates.next().unwrap();
        let main_exe = candidates.next().unwrap();
        let install_bin = candidates.next().unwrap();
        let mut probed = Vec::new();

        let selected = repo_dev_binary(worktree_root.as_path(), &[], |path, _env| {
            probed.push(path.to_string());
            path == install_bin.path
        });

        assert_eq!(selected, Some(install_bin.clone()));
        assert!(probed.ends_with(&vec![repo_binary.path, main_exe.path, install_bin.path]));
    }

    #[test]
    fn returns_none_when_launch_probe_rejects_all_candidates() {
        let worktree_root = worktree_root();
        let candidates = repo_dev_binary_candidates(&worktree_root);
        let mut probed = Vec::new();

        let selected = repo_dev_binary(worktree_root.as_path(), &[], |path, _env| {
            probed.push(path.to_string());
            false
        });

        assert_eq!(selected, None);
        assert_eq!(probed, candidates.into_iter().map(|candidate| candidate.path).collect::<Vec<_>>());
    }

    #[test]
    fn does_not_depend_on_local_fs_visibility_for_repo_candidate() {
        let worktree_root = PathBuf::from("/nonexistent/worktree/test/fixtures/modules");
        let repo_binary = "/nonexistent/worktree/marmoset".to_string();

        let selected = repo_dev_binary(worktree_root.as_path(), &[], |path, env| {
            path == repo_binary
                && env
                    .iter()
                    .any(|(name, value)| name == "MARMOSET_ROOT" && value == "/nonexistent/worktree")
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
