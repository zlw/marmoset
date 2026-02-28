use zed_extension_api::{self as zed, LanguageServerId, Result};

struct MarmosetExtension;

impl zed::Extension for MarmosetExtension {
    fn new() -> Self {
        MarmosetExtension
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let path = worktree
            .which("marmoset-lsp")
            .ok_or_else(|| "marmoset-lsp not found in PATH. Install it with: dune install".to_string())?;

        Ok(zed::Command {
            command: path,
            args: vec![],
            env: worktree.shell_env(),
        })
    }
}

zed::register_extension!(MarmosetExtension);
