module AST = struct
  type program = statement list [@@deriving show]

  (* Phase 2: Type expressions for annotations *)
  and type_expr =
    | TVar of string (* 'a', 'b' *)
    | TCon of string (* 'int', 'string', 'list', 'map', 'option' *)
    | TApp of string * type_expr list (* list[int], map[string, int], option[a] *)
    | TArrow of type_expr list * type_expr (* fn(int, string) -> bool *)
    | TUnion of type_expr list (* int | string | bool *)
    | TRecord of record_type_field list * type_expr option
      (* { x: int, y: string } or { x: int, ...r } - fields + optional row variable *)
  [@@deriving show]

  (* Phase 4.4: Record type field *)
  and record_type_field = {
    field_name : string;
    field_type : type_expr;
  }
  [@@deriving show]

  (* Phase 4.2: Variant definition for enums *)
  and variant_def = {
    variant_name : string;
    variant_fields : type_expr list;
  }
  [@@deriving show]

  and generic_param = {
    name : string;
    constraints : string list; (* trait names like "show", "eq" *)
  }
  [@@deriving show]

  (* Phase 4.3: Trait definitions *)
  and trait_def = {
    name : string;
    type_param : string option; (* The 'a' in trait eq[a], or None for non-generic traits *)
    supertraits : string list; (* trait ord[a]: eq + hash *)
    fields : record_type_field list; (* field-only/mixed trait members *)
    methods : method_sig list;
  }

  and method_sig = {
    method_name : string;
    method_params : (string * type_expr) list;
    method_return_type : type_expr;
    method_default_impl : expression option;
  }

  (* Phase 4.3: Trait implementations *)
  and impl_def = {
    impl_type_params : generic_param list; (* impl eq[a: eq] for list[a] *)
    impl_trait_name : string;
    impl_for_type : type_expr;
    impl_methods : method_impl list;
  }

  (* Phase 4.5: Inherent implementations *)
  and inherent_impl_def = {
    inherent_for_type : type_expr; (* impl point { ... } *)
    inherent_methods : method_impl list;
  }

  and method_impl = {
    impl_method_name : string;
    impl_method_params : (string * type_expr option) list;
    impl_method_return_type : type_expr option;
    impl_method_body : expression;
  }

  (* Phase 4.3: Derive statements *)
  and derive_def = {
    derive_traits : derive_trait list;
    derive_for_type : type_expr;
  }

  and derive_trait = {
    derive_trait_name : string;
    derive_trait_constraints : generic_param list; (* derive eq[a: eq] for option[a] *)
  }
  [@@deriving show]

  and statement = {
    stmt : stmt_kind;
    pos : int;
    end_pos : int;
    file_id : string option;
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
    | EnumDef of {
        name : string;
        type_params : string list;
        variants : variant_def list;
      }
    | TraitDef of trait_def (* Phase 4.3: trait show[a] { ... } *)
    | ImplDef of impl_def (* Phase 4.3: impl show for int { ... } *)
    | InherentImplDef of inherent_impl_def (* Phase 4.5: impl point { ... } *)
    | DeriveDef of derive_def (* Phase 4.3: derive eq, show for color *)
    | TypeAlias of type_alias_def (* Phase 4.4: type point = { x: int, y: int } *)
  [@@deriving show]

  (* Phase 4.4: Type alias definition *)
  and type_alias_def = {
    alias_name : string;
    alias_type_params : string list;
    alias_body : type_expr;
  }
  [@@deriving show]

  and expression = {
    id : int;
    expr : expr_kind;
    pos : int;
    end_pos : int;
    file_id : string option;
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
        is_effectful : bool; (* true when => is used instead of -> *)
        body : statement;
      }
    | Call of expression * expression list
    | EnumConstructor of string * string * expression list
    (* enum_name, variant_name, arguments *)
    (* e.g., option.some(42) -> ("option", "some", [Integer 42]) *)
    | Match of expression * match_arm list (* match scrutinee { arm1, arm2, ... } *)
    | RecordLit of record_field list * expression option (* { x: 1, y: 2, ...base } - fields + optional spread *)
    | FieldAccess of expression * string (* expr.field_name *)
    | MethodCall of expression * string * expression list (* receiver.method(args) *)
  [@@deriving show]

  (* Phase 4.4: Record field in record literal *)
  and record_field = {
    field_name : string;
    field_value : expression option; (* None = punning, use variable with same name *)
  }
  [@@deriving show]

  (* Phase 4.2: Pattern matching *)
  and pattern = {
    pat : pattern_kind;
    pos : int;
    end_pos : int;
    file_id : string option;
  }

  and pattern_kind =
    | PWildcard (* _ *)
    | PVariable of string (* x *)
    | PLiteral of literal_value (* 42, "hello", true *)
    | PConstructor of string * string * pattern list
    (* enum_name, variant_name, field patterns *)
    (* e.g., option.some(x) -> ("option", "some", [PVariable "x"]) *)
    | PRecord of record_pattern_field list * string option
      (* { x:, y:, ...rest } - fields + optional rest variable *)
  [@@deriving show]

  (* Phase 4.4: Record pattern field *)
  and record_pattern_field = {
    pat_field_name : string;
    pat_field_pattern : pattern option; (* None = punning *)
  }
  [@@deriving show]

  and literal_value =
    | LInt of int64
    | LString of string
    | LBool of bool
  [@@deriving show]

  and match_arm = {
    patterns : pattern list; (* multiple patterns for | syntax *)
    body : expression;
  }
  [@@deriving show]

  (* Smart constructors with default metadata (for tests and parser helpers) *)
  let mk_expr ?(id = 0) ?(pos = 0) ?end_pos ?(file_id = None) expr =
    let end_pos = Option.value end_pos ~default:pos in
    { id; expr; pos; end_pos; file_id }

  let mk_stmt ?(pos = 0) ?end_pos ?(file_id = None) stmt =
    let end_pos = Option.value end_pos ~default:pos in
    { stmt; pos; end_pos; file_id }

  let mk_pat ?(pos = 0) ?end_pos ?(file_id = None) pat =
    let end_pos = Option.value end_pos ~default:pos in
    { pat; pos; end_pos; file_id }

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
    | EnumConstructor _ -> "EnumConstructor"
    | Match _ -> "Match"
    | RecordLit _ -> "RecordLit"
    | FieldAccess _ -> "FieldAccess"
    | MethodCall _ -> "MethodCall"

  let to_string (program : program) : string =
    let rec statement_to_string (s : statement) : string =
      match s.stmt with
      | Let l -> Printf.sprintf "let %s = %s;" l.name (expression_to_string l.value)
      | Return expr -> Printf.sprintf "return %s;" (expression_to_string expr)
      | ExpressionStmt expr -> expression_to_string expr
      | Block stmts -> List.map statement_to_string stmts |> String.concat ""
      | EnumDef { name; type_params; variants = _ } ->
          let params_str =
            if type_params = [] then
              ""
            else
              "[" ^ String.concat ", " type_params ^ "]"
          in
          Printf.sprintf "enum %s%s { ... }" name params_str
      | TraitDef { name; type_param; _ } ->
          let params =
            match type_param with
            | None -> ""
            | Some p -> Printf.sprintf "[%s]" p
          in
          Printf.sprintf "trait %s%s { ... }" name params
      | ImplDef { impl_trait_name; impl_for_type; _ } ->
          Printf.sprintf "impl %s for %s { ... }" impl_trait_name (show_type_expr impl_for_type)
      | InherentImplDef { inherent_for_type; _ } ->
          Printf.sprintf "impl %s { ... }" (show_type_expr inherent_for_type)
      | DeriveDef { derive_traits; derive_for_type } ->
          let traits_str = List.map (fun t -> t.derive_trait_name) derive_traits |> String.concat ", " in
          Printf.sprintf "derive %s for %s" traits_str (show_type_expr derive_for_type)
      | TypeAlias { alias_name; alias_type_params; alias_body } ->
          let params_str =
            if alias_type_params = [] then
              ""
            else
              "[" ^ String.concat ", " alias_type_params ^ "]"
          in
          Printf.sprintf "type %s%s = %s" alias_name params_str (show_type_expr alias_body)
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
      | EnumConstructor (enum_name, variant_name, args) ->
          Printf.sprintf "%s.%s(%s)" enum_name variant_name (args_to_string args)
      | Match (scrutinee, _arms) -> Printf.sprintf "match %s { ... }" (expression_to_string scrutinee)
      | RecordLit (fields, spread) ->
          let fields_str =
            fields
            |> List.map (fun f ->
                   match f.field_value with
                   | Some v -> Printf.sprintf "%s: %s" f.field_name (expression_to_string v)
                   | None -> Printf.sprintf "%s:" f.field_name)
            |> String.concat ", "
          in
          let spread_str =
            match spread with
            | Some e -> ", ..." ^ expression_to_string e
            | None -> ""
          in
          Printf.sprintf "{ %s%s }" fields_str spread_str
      | FieldAccess (expr, field) -> Printf.sprintf "%s.%s" (expression_to_string expr) field
      | MethodCall (receiver, method_name, args) ->
          Printf.sprintf "%s.%s(%s)" (expression_to_string receiver) method_name (args_to_string args)
    and block_to_string (block : statement) : string = statement_to_string block
    and function_to_string (params : (string * type_expr option) list) (body : statement) : string =
      let param_str = List.map (fun (name, _annot) -> name) params |> String.concat ", " in
      Printf.sprintf "fn (%s) %s" param_str (block_to_string body)
    and args_to_string (args : expression list) : string =
      List.map expression_to_string args |> String.concat ", "
    in
    List.map statement_to_string program |> String.concat ""

  let%test "mk_expr defaults end_pos and file_id" =
    let e = mk_expr ~id:7 ~pos:11 (Integer 1L) in
    e.pos = 11 && e.end_pos = 11 && e.file_id = None

  let%test "mk_stmt and mk_pat accept explicit span metadata" =
    let s = mk_stmt ~pos:5 ~end_pos:12 ~file_id:(Some "main.mr") (Return (mk_expr (Integer 0L))) in
    let p = mk_pat ~pos:2 ~end_pos:4 ~file_id:(Some "main.mr") (PVariable "x") in
    s.pos = 5
    && s.end_pos = 12
    && s.file_id = Some "main.mr"
    && p.pos = 2
    && p.end_pos = 4
    && p.file_id = Some "main.mr"
end
