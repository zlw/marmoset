(* Marmoset - Main library entry point *)

(* Frontend: parsing *)
module Token = Syntax.Token
module Lexer = Syntax.Lexer
module Parser = Syntax.Parser
module Ast = Syntax.Ast

(* Frontend: type checking *)
module Types = Typecheck.Types
module Unify = Typecheck.Unify
module Infer = Typecheck.Infer
module Source_loc = Diagnostics.Source_loc
module Diagnostic = Diagnostics.Diagnostic
module String_utils = Diagnostics.String_utils
module Builtins = Typecheck.Builtins
module Checker = Typecheck.Checker
module Frontend_compiler = Frontend.Compiler

(* Backend: Go codegen *)
module Go_emitter = Codegen.Emitter
