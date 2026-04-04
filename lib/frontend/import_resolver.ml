module Diagnostic = Diagnostics.Diagnostic
module AST = Syntax.Ast.AST
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type member_presence = {
  internal_name : string;
  has_value : bool;
  has_enum : bool;
  has_named_type : bool;
  has_transparent_type : bool;
  has_shape : bool;
  has_trait : bool;
}

type module_surface = {
  module_id : string;
  module_path : string list;
  internal_prefix : string;
  declarations : member_presence StringMap.t;
  exports : member_presence StringMap.t;
}

type namespace_node = {
  module_ref : module_surface option;
  children : namespace_node StringMap.t;
}

type resolved_imports = {
  namespace_roots : namespace_node StringMap.t;
  direct_bindings : member_presence StringMap.t;
  direct_modules : string list;
}

type namespace_member_resolution =
  [ `ModulePath
  | `Exported of member_presence
  | `NotExported of module_surface * string
  | `MissingMember of module_surface * string ]

type rewrite_result = {
  program : AST.program;
  resolved_imports : resolved_imports;
}

let ( let* ) = Result.bind

let map_result f xs =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest ->
        let* y = f x in
        go (y :: acc) rest
  in
  go [] xs

let escape_internal_component (name : string) : string =
  let buffer = Buffer.create (String.length name + 8) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> Buffer.add_char buffer c
      | c -> Buffer.add_string buffer (Printf.sprintf "_u%04x" (Char.code c)))
    name;
  let escaped = Buffer.contents buffer in
  if escaped = "" then
    "_"
  else
    escaped

let internal_prefix_of_module_id (module_id : string) : string =
  String.split_on_char '.' module_id |> List.map escape_internal_component |> String.concat "__"

let internal_name_of module_id name =
  internal_prefix_of_module_id module_id ^ "__" ^ escape_internal_component name

let%test "internal mangling keeps module separators distinct from literal underscores" =
  internal_prefix_of_module_id "foo.bar" = "foo__bar"
  && internal_prefix_of_module_id "foo__bar" = "foo_u005f_u005fbar"
  && internal_name_of "bang" "panic!" = "bang__panic_u0021"

let merge_presence
    (existing : member_presence option)
    ~(internal_name : string)
    ?(has_value = false)
    ?(has_enum = false)
    ?(has_named_type = false)
    ?(has_transparent_type = false)
    ?(has_shape = false)
    ?(has_trait = false)
    () =
  match existing with
  | None -> { internal_name; has_value; has_enum; has_named_type; has_transparent_type; has_shape; has_trait }
  | Some prior ->
      {
        internal_name = prior.internal_name;
        has_value = prior.has_value || has_value;
        has_enum = prior.has_enum || has_enum;
        has_named_type = prior.has_named_type || has_named_type;
        has_transparent_type = prior.has_transparent_type || has_transparent_type;
        has_shape = prior.has_shape || has_shape;
        has_trait = prior.has_trait || has_trait;
      }

let add_presence map name presence = StringMap.add name presence map

let presence_of_decl module_id (stmt : AST.statement) (decls : member_presence StringMap.t) :
    member_presence StringMap.t =
  let add name ~has_value ~has_enum ~has_named_type ~has_transparent_type ~has_shape ~has_trait =
    let internal_name = internal_name_of module_id name in
    let presence =
      merge_presence (StringMap.find_opt name decls) ~internal_name ~has_value ~has_enum ~has_named_type
        ~has_transparent_type ~has_shape ~has_trait ()
    in
    add_presence decls name presence
  in
  match stmt.stmt with
  | AST.Let { name; _ } when not (String.equal name "_") ->
      add name ~has_value:true ~has_enum:false ~has_named_type:false ~has_transparent_type:false ~has_shape:false
        ~has_trait:false
  | AST.EnumDef { name; _ } ->
      add name ~has_value:false ~has_enum:true ~has_named_type:false ~has_transparent_type:false ~has_shape:false
        ~has_trait:false
  | AST.TypeDef { type_name; _ } ->
      add type_name ~has_value:false ~has_enum:false ~has_named_type:true ~has_transparent_type:false
        ~has_shape:false ~has_trait:false
  | AST.ShapeDef { shape_name; _ } ->
      add shape_name ~has_value:false ~has_enum:false ~has_named_type:false ~has_transparent_type:false
        ~has_shape:true ~has_trait:false
  | AST.TraitDef { name; _ } ->
      add name ~has_value:false ~has_enum:false ~has_named_type:false ~has_transparent_type:false ~has_shape:false
        ~has_trait:true
  | AST.TypeAlias { alias_name; _ } ->
      add alias_name ~has_value:false ~has_enum:false ~has_named_type:false ~has_transparent_type:true
        ~has_shape:false ~has_trait:false
  | _ -> decls

let export_error (module_info : Module_context.parsed_module) ~(name : string) : Diagnostic.t =
  Diagnostic.error_no_span ~code:"module-export-missing"
    ~message:(Printf.sprintf "Module '%s' exports unknown name '%s'" module_info.module_id name)

let build_module_surface (module_info : Module_context.parsed_module) : (module_surface, Diagnostic.t) result =
  let declarations =
    List.fold_left
      (fun decls stmt -> presence_of_decl module_info.module_id stmt decls)
      StringMap.empty module_info.program
  in
  let rec build_exports acc = function
    | [] -> Ok acc
    | name :: rest -> (
        match StringMap.find_opt name declarations with
        | Some presence -> build_exports (StringMap.add name presence acc) rest
        | None -> Error (export_error module_info ~name))
  in
  let* exports = build_exports StringMap.empty module_info.exports in
  Ok
    {
      module_id = module_info.module_id;
      module_path = String.split_on_char '.' module_info.module_id;
      internal_prefix = internal_prefix_of_module_id module_info.module_id;
      declarations;
      exports;
    }

let build_module_surfaces (graph : Module_context.module_graph) :
    ((string, module_surface) Hashtbl.t, Diagnostic.t) result =
  let surfaces = Hashtbl.create (Hashtbl.length graph.modules) in
  let seq = Hashtbl.to_seq_values graph.modules |> List.of_seq in
  let rec go = function
    | [] -> Ok surfaces
    | module_info :: rest ->
        let* surface = build_module_surface module_info in
        Hashtbl.replace surfaces surface.module_id surface;
        go rest
  in
  go seq

let empty_namespace_node = { module_ref = None; children = StringMap.empty }

let rec insert_namespace_path (node : namespace_node) (parts : string list) (target : module_surface) :
    namespace_node =
  match parts with
  | [] -> { node with module_ref = Some target }
  | part :: rest ->
      let child = Option.value (StringMap.find_opt part node.children) ~default:empty_namespace_node in
      { node with children = StringMap.add part (insert_namespace_path child rest target) node.children }

let import_path_string parts = String.concat "." parts

let import_error (imp : Module_context.import_info) ~(code : string) ~(message : string) =
  match imp.file_id with
  | Some file_id ->
      Diagnostic.error_with_span ~code ~message ~file_id ~start_pos:imp.start_pos ~end_pos:imp.end_pos ()
  | None -> Diagnostic.error_no_span ~code ~message

let resolve_import_kind (surfaces : (string, module_surface) Hashtbl.t) (imp : Module_context.import_info) :
    ([ `Namespace of module_surface | `Direct of module_surface * member_presence ], Diagnostic.t) result =
  let full_module_id = import_path_string imp.import_path in
  let module_candidate = Hashtbl.find_opt surfaces full_module_id in
  let member_candidate =
    match List.rev imp.import_path with
    | [] | [ _ ] -> None
    | member_name :: rev_parent -> (
        let parent_id = import_path_string (List.rev rev_parent) in
        match Hashtbl.find_opt surfaces parent_id with
        | None -> None
        | Some parent_surface -> (
            match StringMap.find_opt member_name parent_surface.exports with
            | Some exported -> Some (parent_surface, exported)
            | None -> None))
  in
  match (module_candidate, member_candidate) with
  | Some _, Some _ ->
      Error
        (import_error imp ~code:"module-import-ambiguous"
           ~message:
             (Printf.sprintf "Ambiguous import '%s': it matches both module '%s' and exported member '%s'"
                full_module_id full_module_id
                (List.hd (List.rev imp.import_path))))
  | Some module_surface, None -> Ok (`Namespace module_surface)
  | None, Some (parent_surface, exported) -> Ok (`Direct (parent_surface, exported))
  | None, None -> (
      let parent_not_exported =
        match List.rev imp.import_path with
        | [] | [ _ ] -> None
        | member_name :: rev_parent -> (
            let parent_id = import_path_string (List.rev rev_parent) in
            match Hashtbl.find_opt surfaces parent_id with
            | None -> None
            | Some _ -> Some (parent_id, member_name))
      in
      match parent_not_exported with
      | Some (parent_id, member_name) ->
          Error
            (import_error imp ~code:"module-import-not-exported"
               ~message:(Printf.sprintf "Module '%s' does not export '%s'" parent_id member_name))
      | None ->
          Error
            (import_error imp ~code:"module-import-not-found"
               ~message:
                 (Printf.sprintf "Import '%s' does not resolve to a module or exported member" full_module_id)))

let import_name_collision_error (imp : Module_context.import_info) ~(local_name : string) : Diagnostic.t =
  import_error imp ~code:"module-import-name-collision"
    ~message:
      (Printf.sprintf "Import '%s' conflicts with existing binding '%s'" (import_path_string imp.import_path)
         local_name)

let build_resolved_imports
    ~(surfaces : (string, module_surface) Hashtbl.t) (module_info : Module_context.parsed_module) :
    (resolved_imports, Diagnostic.t) result =
  let rec go namespace_roots direct_bindings direct_modules = function
    | [] ->
        Ok
          {
            namespace_roots;
            direct_bindings;
            direct_modules = List.rev direct_modules |> List.sort_uniq String.compare;
          }
    | imp :: rest -> (
        let* resolved = resolve_import_kind surfaces imp in
        match resolved with
        | `Namespace module_surface ->
            let* namespace_roots =
              match imp.import_alias with
              | Some alias ->
                  if StringMap.mem alias direct_bindings || StringMap.mem alias namespace_roots then
                    Error (import_name_collision_error imp ~local_name:alias)
                  else
                    Ok
                      (StringMap.add alias
                         (insert_namespace_path empty_namespace_node [] module_surface)
                         namespace_roots)
              | None -> (
                  match module_surface.module_path with
                  | [] -> Ok namespace_roots
                  | root :: tail -> (
                      if StringMap.mem root direct_bindings then
                        Error (import_name_collision_error imp ~local_name:root)
                      else
                        let root_node =
                          Option.value (StringMap.find_opt root namespace_roots) ~default:empty_namespace_node
                        in
                        match root_node.module_ref with
                        | Some existing_root when not (String.equal existing_root.module_id root) ->
                            Error (import_name_collision_error imp ~local_name:root)
                        | _ ->
                            Ok
                              (StringMap.add root
                                 (insert_namespace_path root_node tail module_surface)
                                 namespace_roots)))
            in
            go namespace_roots direct_bindings (module_surface.module_id :: direct_modules) rest
        | `Direct (parent_surface, exported) ->
            let local_name =
              match imp.import_alias with
              | Some alias -> alias
              | None -> List.hd (List.rev imp.import_path)
            in
            if StringMap.mem local_name namespace_roots || StringMap.mem local_name direct_bindings then
              Error (import_name_collision_error imp ~local_name)
            else
              go namespace_roots
                (StringMap.add local_name exported direct_bindings)
                (parent_surface.module_id :: direct_modules)
                rest)
  in
  go StringMap.empty StringMap.empty [] module_info.imports

let presence_has_type_like (presence : member_presence) : bool =
  presence.has_enum || presence.has_named_type || presence.has_transparent_type || presence.has_shape

let presence_has_expr_qualifier (presence : member_presence) : bool =
  presence.has_value || presence_has_type_like presence || presence.has_trait

let value_binding_names (bindings : member_presence StringMap.t) : StringSet.t =
  StringMap.fold
    (fun name presence acc ->
      if presence.has_value then
        StringSet.add name acc
      else
        acc)
    bindings StringSet.empty

let direct_type_internal_name (bindings : member_presence StringMap.t) (name : string) : string option =
  match StringMap.find_opt name bindings with
  | Some presence when presence_has_type_like presence -> Some presence.internal_name
  | _ -> None

let direct_trait_internal_name (bindings : member_presence StringMap.t) (name : string) : string option =
  match StringMap.find_opt name bindings with
  | Some presence when presence.has_trait -> Some presence.internal_name
  | _ -> None

let direct_shape_internal_name (bindings : member_presence StringMap.t) (name : string) : string option =
  match StringMap.find_opt name bindings with
  | Some presence when presence.has_shape -> Some presence.internal_name
  | _ -> None

let chain_segments_of_expr (expr : AST.expression) : string list option =
  let rec go acc = function
    | { AST.expr = AST.Identifier name; _ } -> Some (name :: acc)
    | { AST.expr = AST.FieldAccess (receiver, member); _ } -> go (member :: acc) receiver
    | _ -> None
  in
  go [] expr

let%test "chain_segments_of_expr keeps qualifier order stable" =
  let open AST in
  let expr = mk_expr (FieldAccess (mk_expr (FieldAccess (mk_expr (Identifier "collections"), "list")), "head")) in
  chain_segments_of_expr expr = Some [ "collections"; "list"; "head" ]

let lookup_namespace_node (roots : namespace_node StringMap.t) (parts : string list) : namespace_node option =
  match parts with
  | [] -> None
  | root :: rest -> (
      match StringMap.find_opt root roots with
      | None -> None
      | Some node ->
          let rec walk current = function
            | [] -> Some current
            | part :: tail -> (
                match StringMap.find_opt part current.children with
                | None -> None
                | Some next -> walk next tail)
          in
          walk node rest)

let resolve_namespace_member ~(namespace_roots : namespace_node StringMap.t) (segments : string list) :
    namespace_member_resolution option =
  match lookup_namespace_node namespace_roots segments with
  | Some { module_ref = Some _; _ } -> Some `ModulePath
  | Some _ -> None
  | None ->
      let rec try_split rev_prefix rev_suffix =
        match rev_prefix with
        | [] -> None
        | part :: prefix_tail -> (
            let prefix = List.rev (part :: prefix_tail) in
            let suffix = List.rev rev_suffix in
            match lookup_namespace_node namespace_roots prefix with
            | Some { module_ref = Some module_surface; _ } -> (
                match suffix with
                | [ member_name ] -> (
                    match StringMap.find_opt member_name module_surface.exports with
                    | Some exported -> Some (`Exported exported)
                    | None ->
                        if StringMap.mem member_name module_surface.declarations then
                          Some (`NotExported (module_surface, member_name))
                        else
                          Some (`MissingMember (module_surface, member_name)))
                | _ -> None)
            | _ -> try_split prefix_tail (part :: rev_suffix))
      in
      try_split (List.rev segments) []

let collect_inherent_target_generics (te : AST.type_expr) : StringSet.t =
  let is_known_type_name (name : string) =
    Option.is_some (Typecheck.Annotation.builtin_primitive_type name)
    || Option.is_some (Typecheck.Annotation.builtin_type_constructor_name name)
  in
  let rec go ~(in_head : bool) acc = function
    | AST.TCon name ->
        if in_head || is_known_type_name name then
          acc
        else
          StringSet.add name acc
    | AST.TVar name ->
        if in_head then
          acc
        else
          StringSet.add name acc
    | AST.TTraitObject _ -> acc
    | AST.TApp (_name, args) -> List.fold_left (go ~in_head:false) acc args
    | AST.TArrow (params, ret, _) ->
        let acc = List.fold_left (go ~in_head:false) acc params in
        go ~in_head:false acc ret
    | AST.TUnion members | AST.TIntersection members -> List.fold_left (go ~in_head:false) acc members
    | AST.TRecord (fields, _row) ->
        List.fold_left
          (fun acc (field : AST.record_type_field) -> go ~in_head:false acc field.field_type)
          acc fields
  in
  go ~in_head:true StringSet.empty te

let rewrite_constraints
    ~(type_bindings : StringSet.t) ~(available_bindings : member_presence StringMap.t) (constraints : string list)
    : string list =
  List.map
    (fun name ->
      if StringSet.mem name type_bindings then
        name
      else
        match direct_trait_internal_name available_bindings name with
        | Some internal_name -> internal_name
        | None -> (
            match direct_shape_internal_name available_bindings name with
            | Some internal_name -> internal_name
            | None -> name))
    constraints

let rec rewrite_type_expr
    ~(type_bindings : StringSet.t) ~(available_bindings : member_presence StringMap.t) (te : AST.type_expr) :
    AST.type_expr =
  let rewrite_name name =
    if StringSet.mem name type_bindings then
      name
    else
      match direct_type_internal_name available_bindings name with
      | Some internal_name -> internal_name
      | None -> name
  in
  match te with
  | AST.TVar name -> AST.TVar name
  | AST.TCon name -> AST.TCon (rewrite_name name)
  | AST.TApp (name, args) ->
      AST.TApp (rewrite_name name, List.map (rewrite_type_expr ~type_bindings ~available_bindings) args)
  | AST.TTraitObject traits -> AST.TTraitObject (rewrite_constraints ~type_bindings ~available_bindings traits)
  | AST.TArrow (params, ret, is_effectful) ->
      AST.TArrow
        ( List.map (rewrite_type_expr ~type_bindings ~available_bindings) params,
          rewrite_type_expr ~type_bindings ~available_bindings ret,
          is_effectful )
  | AST.TUnion members -> AST.TUnion (List.map (rewrite_type_expr ~type_bindings ~available_bindings) members)
  | AST.TIntersection members ->
      AST.TIntersection (List.map (rewrite_type_expr ~type_bindings ~available_bindings) members)
  | AST.TRecord (fields, row) ->
      AST.TRecord
        ( List.map
            (fun (field : AST.record_type_field) ->
              { field with field_type = rewrite_type_expr ~type_bindings ~available_bindings field.field_type })
            fields,
          Option.map (rewrite_type_expr ~type_bindings ~available_bindings) row )

let identifier_expr_like original name = { original with AST.expr = AST.Identifier name }

let builtin_value_names : StringSet.t =
  List.fold_left
    (fun acc (name, _poly) -> StringSet.add name acc)
    StringSet.empty Typecheck.Builtins.builtin_types

let namespace_resolution_error (expr : AST.expression) (resolution : namespace_member_resolution) (segments : string list)
    : Diagnostic.t =
  let message =
    match resolution with
    | `ModulePath -> Printf.sprintf "Unresolved qualified name '%s'" (String.concat "." segments)
    | `Exported _ -> Printf.sprintf "Unresolved qualified name '%s'" (String.concat "." segments)
    | `NotExported (module_surface, member_name) ->
        Printf.sprintf "Module '%s' does not export '%s'" module_surface.module_id member_name
    | `MissingMember (module_surface, member_name) ->
        Printf.sprintf "Module '%s' has no member '%s'" module_surface.module_id member_name
  in
  match expr.file_id with
  | Some file_id ->
      Diagnostic.error_with_span ~code:"module-qualified-name" ~message ~file_id ~start_pos:expr.pos
        ~end_pos:expr.end_pos ()
  | None -> Diagnostic.error_no_span ~code:"module-qualified-name" ~message

let rewrite_program
    ~(current_module : module_surface)
    ~(imports : resolved_imports)
    ~(available_bindings : member_presence StringMap.t)
    (program : AST.program) : (AST.program, Diagnostic.t) result =
  let bind_value (value_scope : string StringMap.t) ~(name : string) ~(rewritten_name : string) :
      string StringMap.t =
    if String.equal name "_" then
      value_scope
    else
      StringMap.add name rewritten_name value_scope
  in
  let bind_local_value value_scope name = bind_value value_scope ~name ~rewritten_name:name in
  let rec rewrite_statements ~at_top_level value_scope stmts =
    match stmts with
    | [] -> Ok []
    | stmt :: rest ->
        let* stmt', value_scope' = rewrite_statement ~at_top_level value_scope stmt in
        let* rest' = rewrite_statements ~at_top_level value_scope' rest in
        Ok (stmt' :: rest')
  and rewrite_statement ~at_top_level value_scope (stmt : AST.statement) :
      (AST.statement * string StringMap.t, Diagnostic.t) result =
    let type_bindings_of_generic_params params =
      List.fold_left (fun acc (p : AST.generic_param) -> StringSet.add p.name acc) StringSet.empty params
    in
    match stmt.stmt with
    | AST.ExportDecl _ | AST.ImportDecl _ -> Ok (AST.{ stmt with stmt = Block [] }, value_scope)
    | AST.Let { name; value; type_annotation } ->
        let rewritten_name =
          if String.equal name "_" || not at_top_level then
            name
          else
            internal_name_of current_module.module_id name
        in
        let value_scope_for_value = bind_value value_scope ~name ~rewritten_name in
        let* value = rewrite_expr ~value_scope:value_scope_for_value ~type_bindings:StringSet.empty value in
        let type_annotation =
          Option.map (rewrite_type_expr ~type_bindings:StringSet.empty ~available_bindings) type_annotation
        in
        Ok (AST.{ stmt with stmt = Let { name = rewritten_name; value; type_annotation } }, value_scope_for_value)
    | AST.Return expr ->
        let* expr = rewrite_expr ~value_scope ~type_bindings:StringSet.empty expr in
        Ok (AST.{ stmt with stmt = Return expr }, value_scope)
    | AST.ExpressionStmt expr ->
        let* expr = rewrite_expr ~value_scope ~type_bindings:StringSet.empty expr in
        Ok (AST.{ stmt with stmt = ExpressionStmt expr }, value_scope)
    | AST.Block stmts ->
        let* stmts = rewrite_block value_scope stmts in
        Ok (AST.{ stmt with stmt = Block stmts }, value_scope)
    | AST.EnumDef { name; type_params; variants } ->
        let type_bindings = List.fold_left (fun acc name -> StringSet.add name acc) StringSet.empty type_params in
        let variants =
          List.map
            (fun (variant : AST.variant_def) ->
              {
                variant with
                variant_fields =
                  List.map (rewrite_type_expr ~type_bindings ~available_bindings) variant.variant_fields;
              })
            variants
        in
        Ok
          ( AST.
              {
                stmt with
                stmt = EnumDef { name = internal_name_of current_module.module_id name; type_params; variants };
              },
            value_scope )
    | AST.TypeDef { type_name; type_type_params; type_body } ->
        let type_bindings =
          List.fold_left (fun acc name -> StringSet.add name acc) StringSet.empty type_type_params
        in
        let type_body =
          match type_body with
          | AST.NamedTypeProduct fields ->
              AST.NamedTypeProduct
                (List.map
                   (fun (field : AST.record_type_field) ->
                     {
                       field with
                       field_type = rewrite_type_expr ~type_bindings ~available_bindings field.field_type;
                     })
                   fields)
          | AST.NamedTypeWrapper te ->
              AST.NamedTypeWrapper (rewrite_type_expr ~type_bindings ~available_bindings te)
        in
        Ok
          ( AST.
              {
                stmt with
                stmt =
                  TypeDef
                    {
                      type_name = internal_name_of current_module.module_id type_name;
                      type_type_params;
                      type_body;
                    };
              },
            value_scope )
    | AST.ShapeDef { shape_name; shape_type_params; shape_fields } ->
        let type_bindings =
          List.fold_left (fun acc name -> StringSet.add name acc) StringSet.empty shape_type_params
        in
        let shape_fields =
          List.map
            (fun (field : AST.record_type_field) ->
              { field with field_type = rewrite_type_expr ~type_bindings ~available_bindings field.field_type })
            shape_fields
        in
        Ok
          ( AST.
              {
                stmt with
                stmt =
                  ShapeDef
                    {
                      shape_name = internal_name_of current_module.module_id shape_name;
                      shape_type_params;
                      shape_fields;
                    };
              },
            value_scope )
    | AST.TraitDef { name; type_param; supertraits; methods } ->
        let type_bindings =
          match type_param with
          | Some param -> StringSet.singleton param
          | None -> StringSet.empty
        in
        let supertraits = rewrite_constraints ~type_bindings ~available_bindings supertraits in
        let* methods = map_result (rewrite_method_sig ~value_scope ~parent_type_bindings:type_bindings) methods in
        Ok
          ( AST.
              {
                stmt with
                stmt =
                  TraitDef
                    { name = internal_name_of current_module.module_id name; type_param; supertraits; methods };
              },
            value_scope )
    | AST.ImplDef { impl_type_params; impl_trait_name; impl_for_type; impl_methods } ->
        let type_bindings = type_bindings_of_generic_params impl_type_params in
        let impl_type_params =
          List.map
            (fun (p : AST.generic_param) ->
              { p with constraints = rewrite_constraints ~type_bindings ~available_bindings p.constraints })
            impl_type_params
        in
        let impl_trait_name =
          match direct_trait_internal_name available_bindings impl_trait_name with
          | Some internal_name -> internal_name
          | None -> (
              match StringMap.find_opt impl_trait_name current_module.declarations with
              | Some presence when presence.has_trait -> internal_name_of current_module.module_id impl_trait_name
              | _ -> impl_trait_name)
        in
        let impl_for_type = rewrite_type_expr ~type_bindings ~available_bindings impl_for_type in
        let* impl_methods =
          map_result (rewrite_method_impl ~value_scope ~parent_type_bindings:type_bindings) impl_methods
        in
        Ok
          ( AST.{ stmt with stmt = ImplDef { impl_type_params; impl_trait_name; impl_for_type; impl_methods } },
            value_scope )
    | AST.InherentImplDef { inherent_for_type; inherent_methods } ->
        let target_generics = collect_inherent_target_generics inherent_for_type in
        let inherent_for_type =
          rewrite_type_expr ~type_bindings:target_generics ~available_bindings inherent_for_type
        in
        let* inherent_methods =
          map_result (rewrite_method_impl ~value_scope ~parent_type_bindings:target_generics) inherent_methods
        in
        Ok (AST.{ stmt with stmt = InherentImplDef { inherent_for_type; inherent_methods } }, value_scope)
    | AST.DeriveDef { derive_traits; derive_for_type } ->
        let derive_traits =
          List.map
            (fun (trait : AST.derive_trait) ->
              let derive_trait_name =
                match direct_trait_internal_name available_bindings trait.derive_trait_name with
                | Some internal_name -> internal_name
                | None -> (
                    match StringMap.find_opt trait.derive_trait_name current_module.declarations with
                    | Some presence when presence.has_trait ->
                        internal_name_of current_module.module_id trait.derive_trait_name
                    | _ -> trait.derive_trait_name)
              in
              let derive_trait_constraints =
                List.map
                  (fun (p : AST.generic_param) ->
                    {
                      p with
                      constraints =
                        rewrite_constraints ~type_bindings:(StringSet.singleton p.name) ~available_bindings
                          p.constraints;
                    })
                  trait.derive_trait_constraints
              in
              AST.{ derive_trait_name; derive_trait_constraints })
            derive_traits
        in
        let derive_for_type =
          rewrite_type_expr ~type_bindings:StringSet.empty ~available_bindings derive_for_type
        in
        Ok (AST.{ stmt with stmt = DeriveDef { derive_traits; derive_for_type } }, value_scope)
    | AST.TypeAlias { alias_name; alias_type_params; alias_body } ->
        let type_bindings =
          List.fold_left (fun acc name -> StringSet.add name acc) StringSet.empty alias_type_params
        in
        let alias_body = rewrite_type_expr ~type_bindings ~available_bindings alias_body in
        Ok
          ( AST.
              {
                stmt with
                stmt =
                  TypeAlias
                    {
                      alias_name = internal_name_of current_module.module_id alias_name;
                      alias_type_params;
                      alias_body;
                    };
              },
            value_scope )
  and rewrite_block value_scope stmts =
    let* stmts = rewrite_statements ~at_top_level:false value_scope stmts in
    Ok
      (List.filter
         (fun (stmt : AST.statement) ->
           match stmt.stmt with
           | AST.Block [] -> false
           | _ -> true)
         stmts)
  and rewrite_method_sig ~value_scope ~parent_type_bindings (method_sig : AST.method_sig) :
      (AST.method_sig, Diagnostic.t) result =
    let method_type_bindings =
      match method_sig.method_generics with
      | None -> parent_type_bindings
      | Some params ->
          List.fold_left (fun acc (p : AST.generic_param) -> StringSet.add p.name acc) parent_type_bindings params
    in
    let method_generics =
      Option.map
        (List.map (fun (p : AST.generic_param) ->
             {
               p with
               constraints =
                 rewrite_constraints ~type_bindings:method_type_bindings ~available_bindings p.constraints;
             }))
        method_sig.method_generics
    in
    let method_params =
      List.map
        (fun (name, te) -> (name, rewrite_type_expr ~type_bindings:method_type_bindings ~available_bindings te))
        method_sig.method_params
    in
    let value_scope =
      List.fold_left (fun acc (name, _) -> bind_local_value acc name) value_scope method_sig.method_params
    in
    let return_type =
      rewrite_type_expr ~type_bindings:method_type_bindings ~available_bindings method_sig.method_return_type
    in
    let* method_default_impl =
      match method_sig.method_default_impl with
      | None -> Ok None
      | Some expr ->
          let* expr = rewrite_expr ~value_scope ~type_bindings:method_type_bindings expr in
          Ok (Some expr)
    in
    Ok { method_sig with method_generics; method_params; method_return_type = return_type; method_default_impl }
  and rewrite_method_impl ~value_scope ~parent_type_bindings (method_impl : AST.method_impl) :
      (AST.method_impl, Diagnostic.t) result =
    let method_type_bindings =
      match method_impl.impl_method_generics with
      | None -> parent_type_bindings
      | Some params ->
          List.fold_left (fun acc (p : AST.generic_param) -> StringSet.add p.name acc) parent_type_bindings params
    in
    let impl_method_generics =
      Option.map
        (List.map (fun (p : AST.generic_param) ->
             {
               p with
               constraints =
                 rewrite_constraints ~type_bindings:method_type_bindings ~available_bindings p.constraints;
             }))
        method_impl.impl_method_generics
    in
    let impl_method_params =
      List.map
        (fun (name, te_opt) ->
          (name, Option.map (rewrite_type_expr ~type_bindings:method_type_bindings ~available_bindings) te_opt))
        method_impl.impl_method_params
    in
    let body_scope =
      List.fold_left (fun acc (name, _) -> bind_local_value acc name) value_scope method_impl.impl_method_params
    in
    let impl_method_return_type =
      Option.map
        (rewrite_type_expr ~type_bindings:method_type_bindings ~available_bindings)
        method_impl.impl_method_return_type
    in
    let* impl_method_body = rewrite_statement ~at_top_level:false body_scope method_impl.impl_method_body in
    let impl_method_body = fst impl_method_body in
    Ok { method_impl with impl_method_generics; impl_method_params; impl_method_return_type; impl_method_body }
  and rewrite_pattern (pat : AST.pattern) : AST.pattern =
    let rewrite_record_field (field : AST.record_pattern_field) =
      { field with pat_field_pattern = Option.map rewrite_pattern field.pat_field_pattern }
    in
    match pat.pat with
    | AST.PConstructor (enum_name, variant_name, fields) ->
        let enum_name =
          match direct_type_internal_name available_bindings enum_name with
          | Some internal_name -> internal_name
          | None -> (
              match StringMap.find_opt enum_name current_module.declarations with
              | Some presence when presence.has_enum -> internal_name_of current_module.module_id enum_name
              | _ -> enum_name)
        in
        { pat with pat = PConstructor (enum_name, variant_name, List.map rewrite_pattern fields) }
    | AST.PRecord (fields, rest) -> { pat with pat = PRecord (List.map rewrite_record_field fields, rest) }
    | _ -> pat
  and rewrite_expr ~value_scope ~type_bindings (expr : AST.expression) : (AST.expression, Diagnostic.t) result =
    let root_has_value_binding name =
      StringMap.mem name value_scope
      ||
      match StringMap.find_opt name imports.direct_bindings with
      | Some presence when presence.has_value -> true
      | _ -> (
          match StringMap.find_opt name current_module.declarations with
          | Some presence when presence.has_value -> true
          | _ -> StringSet.mem name builtin_value_names)
    in
    let rewrite_identifier name =
      match StringMap.find_opt name value_scope with
      | Some rewritten_name -> rewritten_name
      | None -> (
          match StringMap.find_opt name imports.direct_bindings with
          | Some presence when presence_has_expr_qualifier presence -> presence.internal_name
          | _ -> (
              match StringMap.find_opt name current_module.declarations with
              | Some presence when presence_has_expr_qualifier presence -> presence.internal_name
              | _ -> name))
    in
    match expr.expr with
    | AST.Identifier name -> Ok (identifier_expr_like expr (rewrite_identifier name))
    | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> Ok expr
    | AST.Array items ->
        let* items = map_result (rewrite_expr ~value_scope ~type_bindings) items in
        Ok AST.{ expr with expr = Array items }
    | AST.Index (left, right) ->
        let* left = rewrite_expr ~value_scope ~type_bindings left in
        let* right = rewrite_expr ~value_scope ~type_bindings right in
        Ok AST.{ expr with expr = Index (left, right) }
    | AST.TypeApply (callee, type_args) ->
        let* callee = rewrite_expr ~value_scope ~type_bindings callee in
        let type_args = List.map (rewrite_type_expr ~type_bindings ~available_bindings) type_args in
        Ok AST.{ expr with expr = TypeApply (callee, type_args) }
    | AST.Hash pairs ->
        let* pairs =
          map_result
            (fun (k, v) ->
              let* k = rewrite_expr ~value_scope ~type_bindings k in
              let* v = rewrite_expr ~value_scope ~type_bindings v in
              Ok (k, v))
            pairs
        in
        Ok AST.{ expr with expr = Hash pairs }
    | AST.Prefix (op, inner) ->
        let* inner = rewrite_expr ~value_scope ~type_bindings inner in
        Ok AST.{ expr with expr = Prefix (op, inner) }
    | AST.Infix (left, op, right) ->
        let* left = rewrite_expr ~value_scope ~type_bindings left in
        let* right = rewrite_expr ~value_scope ~type_bindings right in
        Ok AST.{ expr with expr = Infix (left, op, right) }
    | AST.TypeCheck (inner, te) ->
        let* inner = rewrite_expr ~value_scope ~type_bindings inner in
        let te = rewrite_type_expr ~type_bindings ~available_bindings te in
        Ok AST.{ expr with expr = TypeCheck (inner, te) }
    | AST.If (cond, cons, alt) ->
        let* cond = rewrite_expr ~value_scope ~type_bindings cond in
        let* cons = rewrite_statement ~at_top_level:false value_scope cons in
        let cons = fst cons in
        let* alt =
          match alt with
          | None -> Ok None
          | Some stmt ->
              let* stmt = rewrite_statement ~at_top_level:false value_scope stmt in
              Ok (Some (fst stmt))
        in
        Ok AST.{ expr with expr = If (cond, cons, alt) }
    | AST.Function { origin; generics; params; return_type; is_effectful; body } ->
        let type_bindings =
          match generics with
          | None -> type_bindings
          | Some params ->
              List.fold_left (fun acc (p : AST.generic_param) -> StringSet.add p.name acc) type_bindings params
        in
        let generics =
          Option.map
            (List.map (fun (p : AST.generic_param) ->
                 { p with constraints = rewrite_constraints ~type_bindings ~available_bindings p.constraints }))
            generics
        in
        let params =
          List.map
            (fun (name, te_opt) ->
              (name, Option.map (rewrite_type_expr ~type_bindings ~available_bindings) te_opt))
            params
        in
        let value_scope = List.fold_left (fun acc (name, _) -> bind_local_value acc name) value_scope params in
        let return_type = Option.map (rewrite_type_expr ~type_bindings ~available_bindings) return_type in
        let* body = rewrite_statement ~at_top_level:false value_scope body in
        let body = fst body in
        Ok AST.{ expr with expr = Function { origin; generics; params; return_type; is_effectful; body } }
    | AST.Call (callee, args) ->
        let* callee = rewrite_expr ~value_scope ~type_bindings callee in
        let* args = map_result (rewrite_expr ~value_scope ~type_bindings) args in
        Ok AST.{ expr with expr = Call (callee, args) }
    | AST.EnumConstructor (enum_name, variant_name, args) ->
        let enum_name =
          match direct_type_internal_name available_bindings enum_name with
          | Some internal_name -> internal_name
          | None -> (
              match StringMap.find_opt enum_name current_module.declarations with
              | Some presence when presence.has_enum -> internal_name_of current_module.module_id enum_name
              | _ -> enum_name)
        in
        let* args = map_result (rewrite_expr ~value_scope ~type_bindings) args in
        Ok AST.{ expr with expr = EnumConstructor (enum_name, variant_name, args) }
    | AST.Match (scrutinee, arms) ->
        let* scrutinee = rewrite_expr ~value_scope ~type_bindings scrutinee in
        let* arms =
          map_result
            (fun (arm : AST.match_arm) ->
              let arm_scope =
                List.fold_left
                  (fun acc pat ->
                    let rec add_pattern_bindings acc (pat : AST.pattern) =
                      match pat.pat with
                      | AST.PWildcard | AST.PLiteral _ -> acc
                      | AST.PVariable name -> bind_local_value acc name
                      | AST.PConstructor (_, _, fields) -> List.fold_left add_pattern_bindings acc fields
                      | AST.PRecord (fields, rest) -> (
                          let acc =
                            List.fold_left
                              (fun inner (field : AST.record_pattern_field) ->
                                match field.pat_field_pattern with
                                | Some nested -> add_pattern_bindings inner nested
                                | None -> bind_local_value inner field.pat_field_name)
                              acc fields
                          in
                          match rest with
                          | Some name -> bind_local_value acc name
                          | None -> acc)
                    in
                    add_pattern_bindings acc pat)
                  value_scope arm.patterns
              in
              let patterns = List.map rewrite_pattern arm.patterns in
              let* body = rewrite_expr ~value_scope:arm_scope ~type_bindings arm.body in
              Ok AST.{ patterns; body })
            arms
        in
        Ok AST.{ expr with expr = Match (scrutinee, arms) }
    | AST.RecordLit (fields, spread) ->
        let* fields =
          map_result
            (fun (field : AST.record_field) ->
              let* field_value =
                match field.field_value with
                | Some value -> rewrite_expr ~value_scope ~type_bindings value |> Result.map Option.some
                | None ->
                    let rewritten = rewrite_identifier field.field_name in
                    Ok (Some (identifier_expr_like expr rewritten))
              in
              Ok { field with field_value })
            fields
        in
        let* spread =
          match spread with
          | None -> Ok None
          | Some spread -> rewrite_expr ~value_scope ~type_bindings spread |> Result.map Option.some
        in
        Ok AST.{ expr with expr = RecordLit (fields, spread) }
    | AST.FieldAccess (receiver, field_name) -> (
        match chain_segments_of_expr expr with
        | Some segments when not (root_has_value_binding (List.hd segments)) -> (
            match resolve_namespace_member ~namespace_roots:imports.namespace_roots segments with
            | Some (`Exported exported) -> Ok (identifier_expr_like expr exported.internal_name)
            | Some (`NotExported _ as resolution) | Some (`MissingMember _ as resolution) ->
                Error (namespace_resolution_error expr resolution segments)
            | Some `ModulePath -> Ok expr
            | None ->
                let* receiver = rewrite_expr ~value_scope ~type_bindings receiver in
                Ok AST.{ expr with expr = FieldAccess (receiver, field_name) })
        | _ ->
            let* receiver = rewrite_expr ~value_scope ~type_bindings receiver in
            Ok AST.{ expr with expr = FieldAccess (receiver, field_name) })
    | AST.MethodCall { mc_receiver; mc_method; mc_type_args; mc_args } -> (
        match chain_segments_of_expr mc_receiver with
        | Some receiver_segments when not (root_has_value_binding (List.hd receiver_segments)) -> (
            match
              resolve_namespace_member ~namespace_roots:imports.namespace_roots (receiver_segments @ [ mc_method ])
            with
            | Some (`Exported exported) when exported.has_value ->
                let* args = map_result (rewrite_expr ~value_scope ~type_bindings) mc_args in
                let callee = identifier_expr_like mc_receiver exported.internal_name in
                let callee =
                  match mc_type_args with
                  | None -> callee
                  | Some type_args ->
                      AST.
                        {
                          expr with
                          expr =
                            TypeApply
                              (callee, List.map (rewrite_type_expr ~type_bindings ~available_bindings) type_args);
                        }
                in
                Ok AST.{ expr with expr = Call (callee, args) }
            | Some (`Exported exported) when presence_has_type_like exported ->
                let* args = map_result (rewrite_expr ~value_scope ~type_bindings) mc_args in
                let callee = identifier_expr_like mc_receiver exported.internal_name in
                let callee =
                  match mc_type_args with
                  | None -> callee
                  | Some type_args ->
                      AST.
                        {
                          expr with
                          expr =
                            TypeApply
                              (callee, List.map (rewrite_type_expr ~type_bindings ~available_bindings) type_args);
                        }
                in
                Ok AST.{ expr with expr = Call (callee, args) }
            | Some ((`ModulePath | `NotExported _ | `MissingMember _) as resolution) ->
                Error (namespace_resolution_error expr resolution (receiver_segments @ [ mc_method ]))
            | _ ->
                let* mc_receiver = rewrite_expr ~value_scope ~type_bindings mc_receiver in
                let* mc_args = map_result (rewrite_expr ~value_scope ~type_bindings) mc_args in
                let mc_type_args =
                  Option.map (List.map (rewrite_type_expr ~type_bindings ~available_bindings)) mc_type_args
                in
                Ok AST.{ expr with expr = MethodCall { mc_receiver; mc_method; mc_type_args; mc_args } })
        | _ ->
            let* mc_receiver = rewrite_expr ~value_scope ~type_bindings mc_receiver in
            let* mc_args = map_result (rewrite_expr ~value_scope ~type_bindings) mc_args in
            let mc_type_args =
              Option.map (List.map (rewrite_type_expr ~type_bindings ~available_bindings)) mc_type_args
            in
            Ok AST.{ expr with expr = MethodCall { mc_receiver; mc_method; mc_type_args; mc_args } })
    | AST.BlockExpr stmts ->
        let* stmts = rewrite_block value_scope stmts in
        Ok AST.{ expr with expr = BlockExpr stmts }
  in
  rewrite_statements ~at_top_level:true StringMap.empty program

let available_bindings_for_module ~(current_module : module_surface) ~(imports : resolved_imports) :
    member_presence StringMap.t =
  StringMap.fold StringMap.add current_module.declarations imports.direct_bindings

let rewrite_module ~(surfaces : (string, module_surface) Hashtbl.t) (module_info : Module_context.parsed_module) :
    (rewrite_result, Diagnostic.t) result =
  let current_module = Hashtbl.find surfaces module_info.module_id in
  let* resolved_imports = build_resolved_imports ~surfaces module_info in
  let available_bindings = available_bindings_for_module ~current_module ~imports:resolved_imports in
  let* program =
    rewrite_program ~current_module ~imports:resolved_imports ~available_bindings module_info.program
  in
  let program =
    List.filter
      (fun (stmt : AST.statement) ->
        match stmt.stmt with
        | AST.Block [] -> false
        | _ -> true)
      program
  in
  Ok { program; resolved_imports }
