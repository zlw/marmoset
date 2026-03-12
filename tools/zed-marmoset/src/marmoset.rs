use std::path::{Path, PathBuf};

use zed_extension_api::{self as zed, LanguageServerId, Result};

struct MarmosetExtension;

fn repo_dev_binary(manifest_dir: &Path) -> Option<String> {
    let repo_root = manifest_dir
        .join("..")
        .join("..")
        .canonicalize()
        .unwrap_or_else(|_| manifest_dir.join("..").join(".."));
    let candidates = [
        repo_root.join("_build").join("default").join("bin").join("main.exe"),
        repo_root.join("_build").join("install").join("default").join("bin").join("marmoset"),
    ];

    candidates
        .into_iter()
        .find(|path| path.is_file())
        .map(path_to_string)
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
        let path = repo_dev_binary(Path::new(env!("CARGO_MANIFEST_DIR"))).or_else(|| {
            worktree.which("marmoset")
        }).ok_or_else(|| "marmoset not found in PATH. Install it with: dune install".to_string())?;

        Ok(zed::Command {
            command: path,
            args: vec!["lsp".into()],
            env: worktree.shell_env(),
        })
    }
}

zed::register_extension!(MarmosetExtension);

#[cfg(test)]
mod tests {
    use super::repo_dev_binary;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_repo_root(name: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = std::env::temp_dir().join(format!("marmoset-zed-{name}-{nanos}"));
        fs::create_dir_all(root.join("tools").join("zed-marmoset")).unwrap();
        root
    }

    #[test]
    fn prefers_repo_build_main_exe_when_present() {
        let root = temp_repo_root("main-exe");
        let manifest_dir = root.join("tools").join("zed-marmoset");
        let binary = root.join("_build").join("default").join("bin").join("main.exe");
        fs::create_dir_all(binary.parent().unwrap()).unwrap();
        fs::write(&binary, "").unwrap();
        let expected = binary.canonicalize().unwrap().to_string_lossy().into_owned();

        assert_eq!(repo_dev_binary(&manifest_dir), Some(expected));

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn falls_back_to_installed_build_path_when_present() {
        let root = temp_repo_root("install-bin");
        let manifest_dir = root.join("tools").join("zed-marmoset");
        let binary = root.join("_build").join("install").join("default").join("bin").join("marmoset");
        fs::create_dir_all(binary.parent().unwrap()).unwrap();
        fs::write(&binary, "").unwrap();
        let expected = binary.canonicalize().unwrap().to_string_lossy().into_owned();

        assert_eq!(repo_dev_binary(&manifest_dir), Some(expected));

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn returns_none_when_repo_build_is_missing() {
        let root = temp_repo_root("missing");
        let manifest_dir = root.join("tools").join("zed-marmoset");

        assert_eq!(repo_dev_binary(&manifest_dir), None);

        fs::remove_dir_all(root).unwrap();
    }
}
