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
module Builtins = Typecheck.Builtins
module Checker = Typecheck.Checker

(* Backend: Go codegen *)
module Go_emitter = Codegen.Emitter
