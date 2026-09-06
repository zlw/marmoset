import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

function repoBinaryPath(marmosetRoot: string): string {
  return path.join(marmosetRoot, "marmoset");
}

export function activate(context: vscode.ExtensionContext) {
  const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
  const marmosetRoot = process.env.MARMOSET_ROOT;

  if (!marmosetRoot) {
    void vscode.window.showErrorMessage(
      "MARMOSET_ROOT is not set; set it to the Marmoset repo root"
    );
    return;
  }

  const env = { ...process.env, MARMOSET_ROOT: marmosetRoot };

  const serverOptions: ServerOptions = {
    command: repoBinaryPath(marmosetRoot),
    args: ["lsp"],
    options: {
      cwd: workspaceRoot,
      env,
    },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "marmoset" }],
  };

  client = new LanguageClient(
    "marmoset-lsp",
    "Marmoset Language Server",
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
