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
module Source_loc = Typecheck.Source_loc
module Builtins = Typecheck.Builtins
module Checker = Typecheck.Checker

(* Backend: tree-walking interpreter *)
module Value = Interpreter.Value
module Env = Interpreter.Env
module Eval = Interpreter.Eval
module Runtime_builtins = Interpreter.Builtins

(* Backend: bytecode VM *)
module Code = Vm.Code
module Symbol_table = Vm.Symbol_table
module Compiler = Vm.Compiler
module Machine = Vm.Machine

(* Backend: Go codegen (placeholder) *)
module Go_emitter = Codegen.Emitter
