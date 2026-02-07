(* Exhaustiveness checking for pattern matching *)

open Types
module AST = Syntax.Ast.AST

(* Check if patterns cover all cases of a type *)
let check_exhaustive (scrutinee_type : mono_type) (arms : AST.match_arm list) : (unit, string) result =
  (* Collect all patterns *)
  let all_patterns = List.concat_map (fun (arm : AST.match_arm) -> arm.patterns) arms in

  (* Check if there's a wildcard or variable pattern (catches all) *)
  let has_catchall =
    List.exists
      (fun (p : AST.pattern) ->
        match p.pat with
        | AST.PWildcard | AST.PVariable _ -> true
        | _ -> false)
      all_patterns
  in

  if has_catchall then
    Ok ()
  else
    match scrutinee_type with
    | TEnum (enum_name, _) -> (
        (* Get all variants *)
        match Enum_registry.lookup enum_name with
        | None -> Error (Printf.sprintf "Unknown enum: %s" enum_name)
        | Some def ->
            (* Collect covered variants *)
            let covered =
              List.filter_map
                (fun (p : AST.pattern) ->
                  match p.pat with
                  | AST.PConstructor (_, vname, _) -> Some vname
                  | _ -> None)
                all_patterns
            in

            (* Find uncovered variants *)
            let uncovered =
              List.filter (fun (v : Enum_registry.variant_def) -> not (List.mem v.name covered)) def.variants
            in

            if uncovered = [] then
              Ok ()
            else
              let names = List.map (fun (v : Enum_registry.variant_def) -> enum_name ^ "." ^ v.name) uncovered in
              Error (Printf.sprintf "Non-exhaustive match. Missing: %s" (String.concat ", " names)))
    | TInt | TString ->
        (* For primitives, need wildcard unless all literals covered *)
        Error "Non-exhaustive match on primitive type. Add wildcard pattern."
    | TBool ->
        (* Check if true and false both covered *)
        let has_true =
          List.exists
            (fun (p : AST.pattern) ->
              match p.pat with
              | AST.PLiteral (AST.LBool true) -> true
              | _ -> false)
            all_patterns
        in
        let has_false =
          List.exists
            (fun (p : AST.pattern) ->
              match p.pat with
              | AST.PLiteral (AST.LBool false) -> true
              | _ -> false)
            all_patterns
        in
        if has_true && has_false then
          Ok ()
        else
          Error "Non-exhaustive match on bool. Missing true or false."
    | _ -> Error "Non-exhaustive match. Add wildcard pattern."

module Test = struct
  (* Helper to create patterns *)
  let mk_pat kind = AST.{ pat = kind; pos = 0; end_pos = 0; file_id = None }
  let mk_expr kind = AST.{ id = 0; expr = kind; pos = 0; end_pos = 0; file_id = None }
  let mk_arm patterns = AST.{ patterns; body = mk_expr (AST.Integer 0L) }

  let%test "wildcard is exhaustive for any type" =
    let arms = [ mk_arm [ mk_pat AST.PWildcard ] ] in
    match check_exhaustive TInt arms with
    | Ok () -> true
    | Error _ -> false

  let%test "variable pattern is exhaustive for any type" =
    let arms = [ mk_arm [ mk_pat (AST.PVariable "x") ] ] in
    match check_exhaustive TString arms with
    | Ok () -> true
    | Error _ -> false

  let%test "bool with both branches is exhaustive" =
    let arms =
      [ mk_arm [ mk_pat (AST.PLiteral (AST.LBool true)) ]; mk_arm [ mk_pat (AST.PLiteral (AST.LBool false)) ] ]
    in
    match check_exhaustive TBool arms with
    | Ok () -> true
    | Error _ -> false

  let%test "bool with only true is non-exhaustive" =
    let arms = [ mk_arm [ mk_pat (AST.PLiteral (AST.LBool true)) ] ] in
    match check_exhaustive TBool arms with
    | Error _ -> true
    | Ok () -> false

  let%test "int without wildcard is non-exhaustive" =
    let arms = [ mk_arm [ mk_pat (AST.PLiteral (AST.LInt 0L)) ] ] in
    match check_exhaustive TInt arms with
    | Error _ -> true
    | Ok () -> false
end
