import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

function parentDir(dir: string): string | undefined {
  const parent = path.dirname(dir);
  return parent === dir ? undefined : parent;
}

function ancestorDirs(startDir: string): string[] {
  const dirs: string[] = [];
  let current = path.resolve(startDir);

  while (true) {
    dirs.push(current);
    const parent = parentDir(current);
    if (!parent) {
      return dirs;
    }
    current = parent;
  }
}

function hasToolchainStdlib(root: string): boolean {
  return fs.existsSync(path.join(root, "std", "prelude.mr"));
}

function resolveMarmosetRoot(startDir: string | undefined): string | undefined {
  if (!startDir) {
    return undefined;
  }

  return ancestorDirs(startDir).find(hasToolchainStdlib);
}

function repoBinaryPath(marmosetRoot: string): string {
  return path.join(marmosetRoot, "marmoset");
}

export function activate(context: vscode.ExtensionContext) {
  const config = vscode.workspace.getConfiguration("marmoset");
  const serverPath = config.get<string>("lsp.path", "marmoset");
  const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
  const marmosetRoot = resolveMarmosetRoot(workspaceRoot);
  const env = { ...process.env };

  if (marmosetRoot) {
    env.MARMOSET_ROOT = marmosetRoot;
  }

  const command =
    serverPath === "marmoset" && marmosetRoot
      ? repoBinaryPath(marmosetRoot)
      : serverPath;

  const serverOptions: ServerOptions = {
    command,
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
