(* Go code generation from Marmoset AST with monomorphization *)

module AST = Syntax.Ast.AST
module Types = Typecheck.Types
module Infer = Typecheck.Infer

(* ============================================================
   Type Mangling - convert types to Go-safe identifiers
   ============================================================ *)

let rec mangle_type (t : Types.mono_type) : string =
  match t with
  | Types.TInt -> "int64"
  | Types.TFloat -> "float64"
  | Types.TBool -> "bool"
  | Types.TString -> "string"
  | Types.TNull -> "unit"
  | Types.TVar name -> "T" ^ name (* shouldn't happen after monomorphization *)
  | Types.TFun (arg, ret) -> "fn_" ^ mangle_type arg ^ "_" ^ mangle_type ret
  | Types.TArray elem -> "arr_" ^ mangle_type elem
  | Types.THash (key, value) -> "map_" ^ mangle_type key ^ "_" ^ mangle_type value

(* Generate mangled function name: name_type1_type2_... *)
let mangle_func_name name (param_types : Types.mono_type list) : string =
  if param_types = [] then
    name
  else
    name ^ "_" ^ (List.map mangle_type param_types |> String.concat "_")

(* ============================================================
   Type to Go Type String
   ============================================================ *)

let rec type_to_go (t : Types.mono_type) : string =
  match t with
  | Types.TInt -> "int64"
  | Types.TFloat -> "float64"
  | Types.TBool -> "bool"
  | Types.TString -> "string"
  | Types.TNull -> "struct{}"
  | Types.TVar name -> String.uppercase_ascii name (* shouldn't happen *)
  | Types.TFun (arg, ret) -> emit_func_type arg ret
  | Types.TArray elem -> "[]" ^ type_to_go elem
  | Types.THash (key, value) -> "map[" ^ type_to_go key ^ "]" ^ type_to_go value

and emit_func_type arg ret =
  let rec collect_args = function
    | Types.TFun (a, r) ->
        let args, final_ret = collect_args r in
        (a :: args, final_ret)
    | t -> ([], t)
  in
  let args, final_ret = collect_args (Types.TFun (arg, ret)) in
  let args_str = List.map type_to_go args |> String.concat ", " in
  Printf.sprintf "func(%s) %s" args_str (type_to_go final_ret)

(* ============================================================
   Function Registry - track function definitions and instantiations
   ============================================================ *)

(* A function definition: name, params, body, and its polymorphic type *)
type func_def = {
  name : string;
  params : AST.expression list;
  body : AST.statement;
  poly_type : Types.poly_type;
  captures : string list; (* variables captured from outer scope - for closures *)
}

(* An instantiation: concrete types for a polymorphic function *)
type instantiation = {
  func_name : string;
  concrete_types : Types.mono_type list; (* param types *)
  return_type : Types.mono_type;
}

module InstSet = Set.Make (struct
  type t = instantiation

  let compare a b =
    let c = String.compare a.func_name b.func_name in
    if c <> 0 then
      c
    else
      compare a.concrete_types b.concrete_types
end)

type mono_state = {
  mutable func_defs : func_def list;
  mutable instantiations : InstSet.t;
  mutable name_counter : int;
}

let create_mono_state () = { func_defs = []; instantiations = InstSet.empty; name_counter = 0 }

(* ============================================================
   Pass 1: Collect function definitions
   ============================================================ *)

let rec collect_funcs_stmt (state : mono_state) (stmt : AST.statement) : unit =
  match stmt.stmt with
  | AST.Let (name, expr) -> (
      match expr.expr with
      | AST.Function (params, body) ->
          (* For now, assume no captures - we'll handle closures later *)
          state.func_defs <-
            {
              name;
              params;
              body;
              poly_type = Types.Forall ([], Types.TNull);
              (* filled in later *)
              captures = [];
            }
            :: state.func_defs;
          collect_funcs_stmt state body
      | _ -> collect_funcs_expr state expr)
  | AST.Return e -> collect_funcs_expr state e
  | AST.ExpressionStmt e -> collect_funcs_expr state e
  | AST.Block stmts -> List.iter (collect_funcs_stmt state) stmts

and collect_funcs_expr (state : mono_state) (expr : AST.expression) : unit =
  match expr.expr with
  | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ | AST.Identifier _ -> ()
  | AST.Prefix (_, e) -> collect_funcs_expr state e
  | AST.Infix (l, _, r) ->
      collect_funcs_expr state l;
      collect_funcs_expr state r
  | AST.If (cond, cons, alt) ->
      collect_funcs_expr state cond;
      collect_funcs_stmt state cons;
      Option.iter (collect_funcs_stmt state) alt
  | AST.Call (f, args) ->
      collect_funcs_expr state f;
      List.iter (collect_funcs_expr state) args
  | AST.Array elements -> List.iter (collect_funcs_expr state) elements
  | AST.Hash pairs ->
      List.iter
        (fun (k, v) ->
          collect_funcs_expr state k;
          collect_funcs_expr state v)
        pairs
  | AST.Index (container, index) ->
      collect_funcs_expr state container;
      collect_funcs_expr state index
  | AST.Function (_, body) ->
      (* Anonymous function - not a top-level let binding *)
      collect_funcs_stmt state body

(* ============================================================
   Pass 2: Collect instantiations at call sites
   ============================================================ *)

let infer_type env expr =
  match Infer.infer_expression env expr with
  | Ok (subst, t) -> Types.apply_substitution subst t
  | Error _ -> Types.TVar "unknown"

(* Check if a name is a user-defined function *)
let is_user_func (state : mono_state) (name : string) : bool =
  List.exists (fun fd -> fd.name = name) state.func_defs

(* Extract parameter types from a function type *)
let rec extract_param_types n = function
  | Types.TFun (arg, ret) when n > 0 ->
      let rest, final = extract_param_types (n - 1) ret in
      (arg :: rest, final)
  | t -> ([], t)

let rec collect_insts_stmt (state : mono_state) (env : Infer.type_env) (stmt : AST.statement) : Infer.type_env =
  match stmt.stmt with
  | AST.Let (name, expr) ->
      (* For recursive functions, add the binding first so the body can reference it *)
      let expr_type = infer_type env expr in
      let env_with_binding = Infer.TypeEnv.add name (Types.Forall ([], expr_type)) env in
      collect_insts_expr state env_with_binding expr;
      env_with_binding
  | AST.Return e ->
      collect_insts_expr state env e;
      env
  | AST.ExpressionStmt e ->
      collect_insts_expr state env e;
      env
  | AST.Block stmts -> List.fold_left (collect_insts_stmt state) env stmts

and collect_insts_expr (state : mono_state) (env : Infer.type_env) (expr : AST.expression) : unit =
  match expr.expr with
  | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ | AST.Identifier _ -> ()
  | AST.Prefix (_, e) -> collect_insts_expr state env e
  | AST.Infix (l, _, r) ->
      collect_insts_expr state env l;
      collect_insts_expr state env r
  | AST.If (cond, cons, alt) ->
      collect_insts_expr state env cond;
      ignore (collect_insts_stmt state env cons);
      Option.iter (fun s -> ignore (collect_insts_stmt state env s)) alt
  | AST.Call (func, args) -> (
      (* Collect from subexpressions first *)
      collect_insts_expr state env func;
      List.iter (collect_insts_expr state env) args;
      (* Check if this is a call to a user-defined function *)
      match func.expr with
      | AST.Identifier name when is_user_func state name ->
          (* Get concrete types from the arguments *)
          let arg_types = List.map (infer_type env) args in
          (* Get return type by inferring the whole call expression *)
          let call_type = infer_type env expr in
          (* If return type is still a type variable, try to resolve it:
             For many polymorphic functions (identity, arithmetic), return type = param type *)
          let return_type =
            match call_type with
            | Types.TVar _ when List.length arg_types > 0 ->
                (* Use first arg type as return type heuristic *)
                List.hd arg_types
            | t -> t
          in
          let inst = { func_name = name; concrete_types = arg_types; return_type } in
          state.instantiations <- InstSet.add inst state.instantiations
      | _ -> ())
  | AST.Array elements -> List.iter (collect_insts_expr state env) elements
  | AST.Hash pairs ->
      List.iter
        (fun (k, v) ->
          collect_insts_expr state env k;
          collect_insts_expr state env v)
        pairs
  | AST.Index (container, index) ->
      collect_insts_expr state env container;
      collect_insts_expr state env index
  | AST.Function (params, body) ->
      (* Create env with params for the body *)
      let func_type = infer_type env expr in
      let param_types, _ = extract_param_types (List.length params) func_type in
      let param_names =
        List.map
          (fun (p : AST.expression) ->
            match p.expr with
            | AST.Identifier n -> n
            | _ -> "_")
          params
      in
      let body_env =
        List.fold_left2
          (fun acc name typ -> Infer.TypeEnv.add name (Types.Forall ([], typ)) acc)
          env param_names param_types
      in
      ignore (collect_insts_stmt state body_env body)

(* ============================================================
   Code Generation State
   ============================================================ *)

type emit_state = {
  mutable indent : int;
  mono : mono_state;
}

let create_emit_state mono = { indent = 1; mono }
let indent_str state = String.make (state.indent * 4) ' '

(* ============================================================
   Expression Emission
   ============================================================ *)

let rec emit_expr (state : emit_state) (env : Infer.type_env) (expr : AST.expression) : string =
  match expr.expr with
  | AST.Integer i -> Printf.sprintf "int64(%Ld)" i
  | AST.Float f -> Printf.sprintf "float64(%g)" f
  | AST.Boolean true -> "true"
  | AST.Boolean false -> "false"
  | AST.String s -> Printf.sprintf "%S" s
  | AST.Identifier name -> name
  | AST.Prefix (op, operand) -> emit_prefix state env op operand
  | AST.Infix (left, op, right) -> emit_infix state env left op right
  | AST.If (cond, cons, alt) -> emit_if state env expr cond cons alt
  | AST.Call (func, args) -> emit_call state env func args
  | AST.Array elements -> emit_array state env elements
  | AST.Hash pairs -> emit_hash state env pairs
  | AST.Index (container, index) -> emit_index state env container index
  | AST.Function (params, body) -> emit_function_expr state env expr params body

and emit_prefix state env op operand =
  let operand_str = emit_expr state env operand in
  match op with
  | "!" -> Printf.sprintf "!(%s)" operand_str
  | "-" -> Printf.sprintf "-(%s)" operand_str
  | _ -> failwith ("Unknown prefix operator: " ^ op)

and emit_infix state env left op right =
  let left_str = emit_expr state env left in
  let right_str = emit_expr state env right in
  let go_op =
    match op with
    | "+" -> "+"
    | "-" -> "-"
    | "*" -> "*"
    | "/" -> "/"
    | "<" -> "<"
    | ">" -> ">"
    | "==" -> "=="
    | "!=" -> "!="
    | "<=" -> "<="
    | ">=" -> ">="
    | _ -> failwith ("Unknown infix operator: " ^ op)
  in
  Printf.sprintf "(%s %s %s)" left_str go_op right_str

and emit_if state env if_expr cond cons alt =
  let result_type = infer_type env if_expr in
  let result_type_str = type_to_go result_type in
  let cond_str = emit_expr state env cond in
  let ind = indent_str state in
  let inner_ind = ind ^ "    " in

  let emit_branch stmt =
    match stmt.AST.stmt with
    | AST.Block stmts -> (
        match List.rev stmts with
        | [] ->
            inner_ind ^ "    return "
            ^ (if result_type = Types.TNull then
                 "struct{}{}"
               else
                 "nil")
            ^ "\n"
        | last :: rest ->
            let prefix, env' = emit_stmts state env (List.rev rest) in
            let last_str =
              match last.stmt with
              | AST.ExpressionStmt e -> inner_ind ^ "    return " ^ emit_expr state env' e ^ "\n"
              | AST.Return e -> inner_ind ^ "    return " ^ emit_expr state env' e ^ "\n"
              | _ -> fst (emit_stmt state env' last)
            in
            prefix ^ last_str)
    | AST.ExpressionStmt e -> inner_ind ^ "    return " ^ emit_expr state env e ^ "\n"
    | _ -> fst (emit_stmt state env stmt)
  in

  let cons_str = emit_branch cons in
  let alt_str =
    match alt with
    | Some alt_stmt -> emit_branch alt_stmt
    | None ->
        inner_ind ^ "    return "
        ^ (if result_type = Types.TNull then
             "struct{}{}"
           else
             "nil")
        ^ "\n"
  in

  Printf.sprintf "func() %s {\n%s    if %s {\n%s%s    } else {\n%s%s    }\n%s}()" result_type_str ind cond_str
    cons_str ind alt_str ind ind

and emit_call state env func args =
  (* Check for builtin function calls that need special handling *)
  match func.expr with
  | AST.Identifier "len" when List.length args = 1 ->
      let arg_str = emit_expr state env (List.hd args) in
      Printf.sprintf "int64(len(%s))" arg_str
  | AST.Identifier "puts" ->
      let args_str = List.map (emit_expr state env) args |> String.concat ", " in
      Printf.sprintf "puts(%s)" args_str
  | AST.Identifier "first" when List.length args = 1 ->
      let arg_str = emit_expr state env (List.hd args) in
      Printf.sprintf "first(%s)" arg_str
  | AST.Identifier "last" when List.length args = 1 ->
      let arg_str = emit_expr state env (List.hd args) in
      Printf.sprintf "last(%s)" arg_str
  | AST.Identifier "rest" when List.length args = 1 ->
      let arg_str = emit_expr state env (List.hd args) in
      Printf.sprintf "rest(%s)" arg_str
  | AST.Identifier "push" when List.length args = 2 ->
      let arr_str = emit_expr state env (List.hd args) in
      let val_str = emit_expr state env (List.nth args 1) in
      Printf.sprintf "push(%s, %s)" arr_str val_str
  | AST.Identifier name when is_user_func state.mono name ->
      (* User-defined function - use monomorphized name based on argument types *)
      let arg_types = List.map (infer_type env) args in
      let mangled_name = mangle_func_name name arg_types in
      let args_str = List.map (emit_expr state env) args |> String.concat ", " in
      Printf.sprintf "%s(%s)" mangled_name args_str
  | _ ->
      let func_str = emit_expr state env func in
      let args_str = List.map (emit_expr state env) args |> String.concat ", " in
      Printf.sprintf "%s(%s)" func_str args_str

and emit_array state env elements =
  match elements with
  | [] -> "[]interface{}{}"
  | first :: _ ->
      let elem_type = infer_type env first in
      let elem_type_str = type_to_go elem_type in
      let elems_str = List.map (emit_expr state env) elements |> String.concat ", " in
      Printf.sprintf "[]%s{%s}" elem_type_str elems_str

and emit_hash state env pairs =
  match pairs with
  | [] -> "map[interface{}]interface{}{}"
  | (first_key, first_val) :: _ ->
      let key_type = infer_type env first_key in
      let val_type = infer_type env first_val in
      let pairs_str =
        List.map (fun (k, v) -> emit_expr state env k ^ ": " ^ emit_expr state env v) pairs |> String.concat ", "
      in
      Printf.sprintf "map[%s]%s{%s}" (type_to_go key_type) (type_to_go val_type) pairs_str

and emit_index state env container index =
  let container_str = emit_expr state env container in
  let container_type = infer_type env container in
  let index_str = emit_expr state env index in
  (* Check for literal integer (positive or negative via prefix) *)
  let literal_int =
    match index.expr with
    | AST.Integer i -> Some i
    | AST.Prefix ("-", { expr = AST.Integer i; _ }) -> Some (Int64.neg i)
    | _ -> None
  in
  match container_type with
  | Types.TString -> (
      match literal_int with
      (* Literal positive: direct access *)
      | Some i when i >= 0L -> Printf.sprintf "string(%s[%Ld])" container_str i
      (* Literal negative: transform to len-based *)
      | Some i -> Printf.sprintf "string(%s[len(%s)%Ld])" container_str container_str i
      (* Variable: use runtime helper *)
      | None -> Printf.sprintf "indexStr(%s, %s)" container_str index_str)
  | Types.TArray _ -> (
      match literal_int with
      (* Literal positive: direct access *)
      | Some i when i >= 0L -> Printf.sprintf "%s[%Ld]" container_str i
      (* Literal negative: transform to len-based *)
      | Some i -> Printf.sprintf "%s[len(%s)%Ld]" container_str container_str i
      (* Variable: use runtime helper *)
      | None -> Printf.sprintf "indexArr(%s, %s)" container_str index_str)
  | Types.THash _ -> Printf.sprintf "%s[%s]" container_str index_str
  | _ -> Printf.sprintf "%s[%s]" container_str index_str

and emit_function_expr state env func_expr params body =
  (* Inline anonymous function - no monomorphization needed *)
  let func_type = infer_type env func_expr in
  let param_types, return_type = extract_param_types (List.length params) func_type in

  let param_names =
    List.map
      (fun (p : AST.expression) ->
        match p.expr with
        | AST.Identifier name -> name
        | _ -> failwith "Function parameter must be identifier")
      params
  in

  let params_with_types = List.map2 (fun name typ -> name ^ " " ^ type_to_go typ) param_names param_types in
  let params_str = String.concat ", " params_with_types in
  let return_type_str = type_to_go return_type in

  (* Extend environment with parameter bindings for the body *)
  let body_env =
    List.fold_left2
      (fun acc name typ -> Infer.TypeEnv.add name (Types.Forall ([], typ)) acc)
      env param_names param_types
  in

  let body_str = emit_func_body state body_env body in

  Printf.sprintf "func(%s) %s {\n%s%s}" params_str return_type_str body_str (indent_str state)

and emit_func_body state env stmt =
  let ind = indent_str state in
  let inner_ind = ind ^ "    " in
  match stmt.AST.stmt with
  | AST.Block stmts -> (
      match List.rev stmts with
      | [] -> inner_ind ^ "return\n"
      | last :: rest ->
          let prefix, env' = emit_stmts state env (List.rev rest) in
          let last_str =
            match last.stmt with
            | AST.ExpressionStmt e -> inner_ind ^ "return " ^ emit_expr state env' e ^ "\n"
            | AST.Return e -> inner_ind ^ "return " ^ emit_expr state env' e ^ "\n"
            | _ -> fst (emit_stmt state env' last)
          in
          prefix ^ last_str)
  | AST.ExpressionStmt e -> inner_ind ^ "return " ^ emit_expr state env e ^ "\n"
  | _ -> fst (emit_stmt state env stmt)

(* ============================================================
   Statement Emission
   ============================================================ *)

and emit_stmt (state : emit_state) (env : Infer.type_env) (stmt : AST.statement) : string * Infer.type_env =
  let ind = indent_str state in
  match stmt.stmt with
  | AST.Let (name, expr) -> (
      match expr.expr with
      | AST.Function _ when is_user_func state.mono name ->
          (* Skip - this is a top-level function, emitted separately *)
          let expr_type = infer_type env expr in
          let env' = Infer.TypeEnv.add name (Types.Forall ([], expr_type)) env in
          ("", env')
      | _ ->
          let expr_str = emit_expr state env expr in
          let expr_type = infer_type env expr in
          let env' = Infer.TypeEnv.add name (Types.Forall ([], expr_type)) env in
          (Printf.sprintf "%s%s := %s\n" ind name expr_str, env'))
  | AST.Return expr ->
      let expr_str = emit_expr state env expr in
      (Printf.sprintf "%sreturn %s\n" ind expr_str, env)
  | AST.ExpressionStmt expr ->
      let expr_str = emit_expr state env expr in
      (Printf.sprintf "%s_ = %s\n" ind expr_str, env)
  | AST.Block stmts ->
      let code, env' = emit_stmts state env stmts in
      (code, env')

and emit_stmts state env stmts =
  let rec loop env acc = function
    | [] -> (String.concat "" (List.rev acc), env)
    | stmt :: rest ->
        let code, env' = emit_stmt state env stmt in
        loop env' (code :: acc) rest
  in
  loop env [] stmts

(* ============================================================
   Generate Monomorphized Functions
   ============================================================ *)

let emit_specialized_func (state : emit_state) (inst : instantiation) : string =
  (* Find the function definition *)
  let func_def = List.find (fun fd -> fd.name = inst.func_name) state.mono.func_defs in

  let param_names =
    List.map
      (fun (p : AST.expression) ->
        match p.expr with
        | AST.Identifier name -> name
        | _ -> failwith "Function parameter must be identifier")
      func_def.params
  in

  let mangled_name = mangle_func_name inst.func_name inst.concrete_types in

  let params_with_types =
    List.map2 (fun name typ -> name ^ " " ^ type_to_go typ) param_names inst.concrete_types
  in
  let params_str = String.concat ", " params_with_types in
  let return_type_str = type_to_go inst.return_type in

  (* Create env with parameter types for the body *)
  let base_env = Typecheck.Builtins.prelude_env () in
  (* Build the concrete function type from param types and return type *)
  let func_type =
    List.fold_right (fun param_t acc -> Types.TFun (param_t, acc)) inst.concrete_types inst.return_type
  in
  (* Add the function itself for recursive calls *)
  let env_with_func = Infer.TypeEnv.add inst.func_name (Types.Forall ([], func_type)) base_env in
  let body_env =
    List.fold_left2
      (fun acc name typ -> Infer.TypeEnv.add name (Types.Forall ([], typ)) acc)
      env_with_func param_names inst.concrete_types
  in

  (* Save and reset indent for top-level function *)
  let saved_indent = state.indent in
  state.indent <- 0;
  let body_str = emit_func_body state body_env func_def.body in
  state.indent <- saved_indent;

  Printf.sprintf "func %s(%s) %s {\n%s}\n" mangled_name params_str return_type_str body_str

(* ============================================================
   Program Emission
   ============================================================ *)

let emit_program (program : AST.program) : string =
  let mono_state = create_mono_state () in
  let base_env = Typecheck.Builtins.prelude_env () in

  (* Pass 1: Collect function definitions *)
  List.iter (collect_funcs_stmt mono_state) program;

  (* Pass 2: Collect instantiations *)
  ignore (List.fold_left (collect_insts_stmt mono_state) base_env program);

  let emit_state = create_emit_state mono_state in

  (* Generate specialized functions *)
  let specialized_funcs =
    InstSet.elements mono_state.instantiations
    |> List.map (emit_specialized_func emit_state)
    |> String.concat "\n"
  in

  (* Emit main body *)
  let main_body, _ = emit_stmts emit_state base_env program in

  (* Build final output *)
  let top_funcs =
    if specialized_funcs = "" then
      ""
    else
      specialized_funcs ^ "\n"
  in

  Printf.sprintf "package main\n\n%sfunc main() {\n%s}\n" top_funcs main_body

(* ============================================================
   Runtime
   ============================================================ *)

let runtime_go =
  {|// Marmoset Runtime - builtin functions for generated Go code
package main

import "fmt"

// puts prints values to stdout, returns struct{}
func puts[T any](v T) struct{} {
	fmt.Println(v)
	return struct{}{}
}

// indexArr handles negative indexing for arrays
func indexArr[T any](arr []T, i int64) T {
	if i < 0 {
		i = int64(len(arr)) + i
	}
	return arr[i]
}

// indexStr handles negative indexing for strings
func indexStr(s string, i int64) string {
	if i < 0 {
		i = int64(len(s)) + i
	}
	return string(s[i])
}

// first returns the first element of an array, or zero value if empty
func first[T any](arr []T) T {
	if len(arr) == 0 {
		var zero T
		return zero
	}
	return arr[0]
}

// last returns the last element of an array, or zero value if empty
func last[T any](arr []T) T {
	if len(arr) == 0 {
		var zero T
		return zero
	}
	return arr[len(arr)-1]
}

// rest returns all but the first element, or empty array if empty
func rest[T any](arr []T) []T {
	if len(arr) == 0 {
		return []T{}
	}
	return arr[1:]
}

// push appends an element to an array and returns the new array
func push[T any](arr []T, v T) []T {
	return append(arr, v)
}
|}

(* ============================================================
   Main Entry Point
   ============================================================ *)

let compile_string (source : string) : (string, string) result =
  match Syntax.Parser.parse source with
  | Error errors -> Error ("Parse error: " ^ String.concat ", " errors)
  | Ok program -> (
      let env = Typecheck.Builtins.prelude_env () in
      match Infer.infer_program ~env program with
      | Error e -> Error ("Type error: " ^ Infer.error_to_string e)
      | Ok _ -> Ok (emit_program program))

type build_output = {
  main_go : string;
  runtime_go : string;
}

let compile_to_build (source : string) : (build_output, string) result =
  match compile_string source with
  | Error e -> Error e
  | Ok main_go -> Ok { main_go; runtime_go }

let get_runtime () = runtime_go

(* ============================================================
   Tests
   ============================================================ *)

let string_contains s substring =
  let len_sub = String.length substring in
  let len_s = String.length s in
  if len_sub > len_s then
    false
  else
    let rec check i =
      if i + len_sub > len_s then
        false
      else if String.sub s i len_sub = substring then
        true
      else
        check (i + 1)
    in
    check 0

let%test "emit integer" =
  match compile_string "42" with
  | Ok code -> string_contains code "int64(42)"
  | Error _ -> false

let%test "emit addition" =
  match compile_string "1 + 2" with
  | Ok code -> string_contains code "(int64(1) + int64(2))"
  | Error _ -> false

let%test "emit let binding" =
  match compile_string "let x = 5; x" with
  | Ok code -> string_contains code "x := int64(5)" && string_contains code "_ = x"
  | Error _ -> false

let%test "emit boolean" =
  match compile_string "true" with
  | Ok code -> string_contains code "_ = true"
  | Error _ -> false

let%test "emit string" =
  match compile_string "\"hello\"" with
  | Ok code -> string_contains code "\"hello\""
  | Error _ -> false

let%test "emit comparison" =
  match compile_string "1 < 2" with
  | Ok code -> string_contains code "(int64(1) < int64(2))"
  | Error _ -> false

let%test "monomorphized function" =
  match compile_string "let double = fn(x) { x * 2 }; double(5)" with
  | Ok code ->
      (* Should have top-level func double_int64 *)
      string_contains code "func double_int64(x int64) int64"
      &&
      (* Call should use mangled name *)
      string_contains code "double_int64(int64(5))"
  | Error _ -> false

let%test "polymorphic function multiple instantiations" =
  match compile_string "let id = fn(x) { x }; id(5); id(true)" with
  | Ok code ->
      string_contains code "func id_int64(x int64) int64"
      && string_contains code "func id_bool(x bool) bool"
      && string_contains code "id_int64(int64(5))"
      && string_contains code "id_bool(true)"
  | Error _ -> false

let%test "emit array index with literal" =
  match compile_string "let a = [1,2,3]; a[0]" with
  | Ok code -> string_contains code "a[0]" && not (string_contains code "int(")
  | Error _ -> false

let%test "emit array index with variable" =
  match compile_string "let a = [1,2,3]; let i = 1; a[i]" with
  | Ok code -> string_contains code "indexArr(a, i)"
  | Error _ -> false

let%test "emit if inside function body" =
  match compile_string "let abs = fn(x) { if (x < 0) { -x } else { x } }; abs(5)" with
  | Ok code -> string_contains code "func abs_int64(x int64) int64" && string_contains code "if (x < int64(0))"
  | Error _ -> false

let%test "emit recursive function" =
  match compile_string "let fact = fn(n) { if (n < 2) { 1 } else { n * fact(n - 1) } }; fact(5)" with
  | Ok code ->
      string_contains code "func fact_int64(n int64) int64" && string_contains code "fact_int64((n - int64(1)))"
  | Error _ -> false

let%test "emit closure (function returning function)" =
  match compile_string "let make_adder = fn(x) { fn(y) { x + y } }; let add5 = make_adder(5); add5(10)" with
  | Ok code ->
      string_contains code "func make_adder_int64(x int64) func(int64) int64"
      && string_contains code "return func(y int64) int64"
  | Error _ -> false

let%test "emit string indexing returns string" =
  match compile_string "let s = \"hello\"; s[0]" with
  | Ok code -> string_contains code "string(s[0])"
  | Error _ -> false

let%test "emit negative array index" =
  match compile_string "let a = [1,2,3]; a[-1]" with
  | Ok code -> string_contains code "a[len(a)-1]"
  | Error _ -> false

let%test "emit negative string index" =
  match compile_string "let s = \"hello\"; s[-2]" with
  | Ok code -> string_contains code "s[len(s)-2]"
  | Error _ -> false
