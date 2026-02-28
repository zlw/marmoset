(* Fallback: linol bundles its own LSP types as Linol_lsp *)
include Linol_lsp

let diagnostic_message (s : string) = `String s
