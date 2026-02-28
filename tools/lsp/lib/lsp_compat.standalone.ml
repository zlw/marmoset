(* Used when the standalone "lsp" library is available (e.g. local dev) *)
include Lsp

let diagnostic_message (s : string) = s
