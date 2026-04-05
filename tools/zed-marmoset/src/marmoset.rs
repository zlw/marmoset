use std::path::{Path, PathBuf};

use zed_extension_api::{self as zed, LanguageServerId, Result};

struct MarmosetExtension;

fn repo_dev_binary_candidates(manifest_dir: &Path) -> [String; 3] {
    let repo_root = manifest_dir.join("..").join("..");
    [
        repo_root.join("marmoset"),
        repo_root.join("_build").join("default").join("bin").join("main.exe"),
        repo_root.join("_build").join("install").join("default").join("bin").join("marmoset"),
    ]
    .map(path_to_string)
}

fn repo_dev_binary(
    manifest_dir: &Path,
    mut is_launchable: impl FnMut(&str) -> bool,
) -> Option<String> {
    repo_dev_binary_candidates(manifest_dir)
        .into_iter()
        .find(|path| is_launchable(path))
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
        let path = repo_dev_binary(Path::new(env!("CARGO_MANIFEST_DIR")), |path| {
            launchable_binary(path, &shell_env)
        })
        .or_else(|| worktree.which("marmoset"))
        .ok_or_else(|| "marmoset not found in PATH. Install it with: dune install".to_string())?;

        Ok(zed::Command {
            command: path,
            args: vec!["lsp".into()],
            env: shell_env,
        })
    }
}

zed::register_extension!(MarmosetExtension);

#[cfg(test)]
mod tests {
    use super::{repo_dev_binary, repo_dev_binary_candidates};
    use std::path::PathBuf;

    fn manifest_dir() -> PathBuf {
        PathBuf::from("/tmp/marmoset-dev/tools/zed-marmoset")
    }

    #[test]
    fn prefers_repo_root_marmoset_binary_when_launch_probe_accepts_it() {
        let manifest_dir = manifest_dir();
        let [repo_binary, _main_exe, _install_bin] = repo_dev_binary_candidates(&manifest_dir);
        let mut probed = Vec::new();

        let selected = repo_dev_binary(manifest_dir.as_path(), |path| {
            probed.push(path.to_string());
            path == repo_binary
        });

        assert_eq!(selected, Some(repo_binary.clone()));
        assert_eq!(probed, vec![repo_binary]);
    }

    #[test]
    fn falls_back_to_build_main_exe_when_repo_root_binary_is_not_launchable() {
        let manifest_dir = manifest_dir();
        let [repo_binary, main_exe, install_bin] = repo_dev_binary_candidates(&manifest_dir);
        let mut probed = Vec::new();

        let selected = repo_dev_binary(manifest_dir.as_path(), |path| {
            probed.push(path.to_string());
            path == main_exe
        });

        assert_eq!(selected, Some(main_exe.clone()));
        assert_eq!(probed, vec![repo_binary, main_exe]);
        assert_ne!(probed, vec![install_bin]);
    }

    #[test]
    fn falls_back_to_installed_build_path_when_launch_probe_accepts_it() {
        let manifest_dir = manifest_dir();
        let [repo_binary, main_exe, install_bin] = repo_dev_binary_candidates(&manifest_dir);
        let mut probed = Vec::new();

        let selected = repo_dev_binary(manifest_dir.as_path(), |path| {
            probed.push(path.to_string());
            path == install_bin
        });

        assert_eq!(selected, Some(install_bin.clone()));
        assert_eq!(probed, vec![repo_binary, main_exe, install_bin]);
    }

    #[test]
    fn returns_none_when_launch_probe_rejects_both_candidates() {
        let manifest_dir = manifest_dir();
        let [repo_binary, main_exe, install_bin] = repo_dev_binary_candidates(&manifest_dir);
        let mut probed = Vec::new();

        let selected = repo_dev_binary(manifest_dir.as_path(), |path| {
            probed.push(path.to_string());
            false
        });

        assert_eq!(selected, None);
        assert_eq!(probed, vec![repo_binary, main_exe, install_bin]);
    }

    #[test]
    fn does_not_depend_on_local_fs_visibility_for_repo_candidate() {
        let manifest_dir = PathBuf::from("/nonexistent/worktree/tools/zed-marmoset");
        let [repo_binary, _main_exe, _install_bin] = repo_dev_binary_candidates(&manifest_dir);

        let selected = repo_dev_binary(manifest_dir.as_path(), |path| path == repo_binary);

        assert_eq!(selected, Some(repo_binary));
    }
}
