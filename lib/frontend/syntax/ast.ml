module AST = struct
  type program = statement list [@@deriving show]

  (* Phase 2: Type expressions for annotations *)
  and type_expr =
    | TVar of string (* 'a', 'b' *)
    | TCon of string (* 'int', 'string', 'list', 'map', 'option' *)
    | TApp of string * type_expr list (* list[int], map[string, int], option[a] *)
    | TArrow of type_expr list * type_expr (* fn(int, string) -> bool *)
    | TUnion of type_expr list (* int | string | bool *)
  [@@deriving show]

  and generic_param = {
    name : string;
    constraints : string list; (* trait names like "show", "eq" *)
  }
  [@@deriving show]

  and statement = {
    stmt : stmt_kind;
    pos : int;
  }

  and stmt_kind =
    | Let of {
        name : string;
        value : expression;
        type_annotation : type_expr option;
      }
    | Return of expression
    | ExpressionStmt of expression
    | Block of statement list
  [@@deriving show]

  and expression = {
    expr : expr_kind;
    pos : int;
  }

  and expr_kind =
    | Identifier of string
    | Integer of int64
    | Float of float
    | Boolean of bool
    | String of string
    | Array of expression list
    | Index of expression * expression
    | Hash of (expression * expression) list
    | Prefix of string * expression
    | Infix of expression * string * expression
    | TypeCheck of expression * type_expr (* x is int *)
    | If of expression * statement * statement option
    | Function of {
        generics : generic_param list option; (* [a], [a: show], etc. *)
        params : (string * type_expr option) list; (* parameter names and optional type annotations *)
        return_type : type_expr option; (* return type annotation *)
        body : statement;
      }
    | Call of expression * expression list
  [@@deriving show]

  (* Smart constructors with default pos=0 (for tests) *)
  let mk_expr ?(pos = 0) expr = { expr; pos }
  let mk_stmt ?(pos = 0) stmt = { stmt; pos }

  (* Equality functions that ignore positions (for testing) *)
  let rec expr_equal (e1 : expression) (e2 : expression) : bool =
    match (e1.expr, e2.expr) with
    | Identifier a, Identifier b -> a = b
    | Integer a, Integer b -> a = b
    | Float a, Float b -> a = b
    | Boolean a, Boolean b -> a = b
    | String a, String b -> a = b
    | Array a, Array b -> List.length a = List.length b && List.for_all2 expr_equal a b
    | Index (a1, a2), Index (b1, b2) -> expr_equal a1 b1 && expr_equal a2 b2
    | Hash a, Hash b ->
        List.length a = List.length b
        && List.for_all2 (fun (k1, v1) (k2, v2) -> expr_equal k1 k2 && expr_equal v1 v2) a b
    | Prefix (op1, e1), Prefix (op2, e2) -> op1 = op2 && expr_equal e1 e2
    | Infix (l1, op1, r1), Infix (l2, op2, r2) -> op1 = op2 && expr_equal l1 l2 && expr_equal r1 r2
    | If (c1, t1, e1), If (c2, t2, e2) -> (
        expr_equal c1 c2
        && stmt_equal t1 t2
        &&
        match (e1, e2) with
        | None, None -> true
        | Some a, Some b -> stmt_equal a b
        | _ -> false)
    | Function f1, Function f2 -> List.length f1.params = List.length f2.params && stmt_equal f1.body f2.body
    | Call (f1, a1), Call (f2, a2) ->
        expr_equal f1 f2 && List.length a1 = List.length a2 && List.for_all2 expr_equal a1 a2
    | _ -> false

  and stmt_equal (s1 : statement) (s2 : statement) : bool =
    match (s1.stmt, s2.stmt) with
    | Let l1, Let l2 -> l1.name = l2.name && expr_equal l1.value l2.value
    | Return e1, Return e2 -> expr_equal e1 e2
    | ExpressionStmt e1, ExpressionStmt e2 -> expr_equal e1 e2
    | Block ss1, Block ss2 -> List.length ss1 = List.length ss2 && List.for_all2 stmt_equal ss1 ss2
    | _ -> false

  let program_equal (p1 : program) (p2 : program) : bool =
    List.length p1 = List.length p2 && List.for_all2 stmt_equal p1 p2

  let type_of (e : expression) : string =
    match e.expr with
    | Identifier _ -> "Identifier"
    | Integer _ -> "Integer"
    | Float _ -> "Float"
    | String _ -> "String"
    | Array _ -> "Array"
    | Index _ -> "Index"
    | Hash _ -> "Hash"
    | Prefix _ -> "Prefix"
    | Infix _ -> "Infix"
    | TypeCheck _ -> "TypeCheck"
    | Boolean _ -> "Boolean"
    | If _ -> "If"
    | Function _ -> "Function"
    | Call _ -> "Call"

  let to_string (program : program) : string =
    let rec statement_to_string (s : statement) : string =
      match s.stmt with
      | Let l -> Printf.sprintf "let %s = %s;" l.name (expression_to_string l.value)
      | Return expr -> Printf.sprintf "return %s;" (expression_to_string expr)
      | ExpressionStmt expr -> expression_to_string expr
      | Block stmts -> List.map statement_to_string stmts |> String.concat ""
    and expression_to_string (e : expression) : string =
      match e.expr with
      | Identifier ident -> ident
      | Integer i -> Int64.to_string i
      | Float f -> string_of_float f
      | String s -> Printf.sprintf "\"%s\"" s
      | Array exprs -> Printf.sprintf "[%s]" (args_to_string exprs)
      | Index (arr, idx) -> Printf.sprintf "(%s[%s])" (expression_to_string arr) (expression_to_string idx)
      | Hash pairs ->
          Printf.sprintf "{%s}"
            (pairs
            |> List.map (fun (k, v) -> Printf.sprintf "%s: %s" (expression_to_string k) (expression_to_string v))
            |> String.concat ", ")
      | Prefix (op, expr) -> Printf.sprintf "(%s%s)" op (expression_to_string expr)
      | Infix (left, op, right) ->
          Printf.sprintf "(%s %s %s)" (expression_to_string left) op (expression_to_string right)
      | TypeCheck (expr, type_ann) ->
          Printf.sprintf "(%s is %s)" (expression_to_string expr) (show_type_expr type_ann)
      | Boolean b ->
          if b then
            "true"
          else
            "false"
      | If (cond, cons, alt) ->
          Printf.sprintf "if %s %s%s" (expression_to_string cond) (block_to_string cons)
            (match alt with
            | Some a -> Printf.sprintf " else %s" (block_to_string a)
            | None -> "")
      | Function f -> function_to_string f.params f.body
      | Call (expr, args) -> Printf.sprintf "%s(%s)" (expression_to_string expr) (args_to_string args)
    and block_to_string (block : statement) : string = statement_to_string block
    and function_to_string (params : (string * type_expr option) list) (body : statement) : string =
      let param_str = List.map (fun (name, _annot) -> name) params |> String.concat ", " in
      Printf.sprintf "fn (%s) %s" param_str (block_to_string body)
    and args_to_string (args : expression list) : string =
      List.map expression_to_string args |> String.concat ", "
    in
    List.map statement_to_string program |> String.concat ""
end
