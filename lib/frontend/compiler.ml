module AST = Syntax.Ast.AST
module Diagnostic = Diagnostics.Diagnostic
module Discovery = Discovery
module Module_context = Module_context
module Import_resolver = Import_resolver

module Annotation = Typecheck.Annotation
module Builtins = Typecheck.Builtins
module Checker = Typecheck.Checker
module Codegen = Codegen.Emitter
module Enum_registry = Typecheck.Enum_registry
module Inherent_registry = Typecheck.Inherent_registry
module Infer = Typecheck.Infer
module Module_sig = Typecheck.Module_sig
module Trait_registry = Typecheck.Trait_registry
module Type_registry = Typecheck.Type_registry
module Types = Typecheck.Types

type checked_module = {
  module_id : string;
  file_path : string;
  program : AST.program;
  result : Checker.typecheck_result;
  signature : Module_sig.module_signature;
  locals : Module_sig.module_locals;
}

type compiled_project = {
  modules : checked_module list;
  program : AST.program;
  environment : Infer.type_env;
  type_map : Infer.type_map;
  call_resolution_map : (int, Infer.method_resolution) Hashtbl.t;
  method_type_args_map : (int, Types.mono_type list) Hashtbl.t;
  method_def_map : (int, Typecheck.Resolution_artifacts.typed_method_def) Hashtbl.t;
  trait_object_coercion_map : (int, Typecheck.Resolution_artifacts.trait_object_coercion) Hashtbl.t;
  placeholder_rewrite_map : Infer.placeholder_rewrite_map;
  diagnostics : Diagnostic.t list;
}

let ( let* ) = Result.bind

let compiler_error ~(code : string) ~(message : string) : Diagnostic.t =
  Diagnostic.error_no_span ~code ~message

let merge_hashtbl dst src = Hashtbl.iter (fun key value -> Hashtbl.replace dst key value) src

let iter_result f xs =
  let rec go = function
    | [] -> Ok ()
    | x :: rest ->
        let* () = f x in
        go rest
  in
  go xs

let error_at_stmt ~(code : string) ~(message : string) (stmt : AST.statement) : Diagnostic.t =
  match stmt.file_id with
  | Some file_id ->
      Diagnostic.error_with_span ~code ~message ~file_id ~start_pos:stmt.pos ~end_pos:stmt.end_pos ()
  | None -> compiler_error ~code ~message

let reset_module_state () =
  Annotation.clear_type_aliases ();
  Type_registry.clear ();
  Trait_registry.clear ();
  Inherent_registry.clear ();
  Enum_registry.clear ();
  Infer.clear_method_resolution_store ();
  Infer.clear_type_var_user_names ();
  Infer.clear_top_level_placeholders ();
  Infer.clear_constraint_store ()

let has_module_headers (program : AST.program) : bool =
  List.exists
    (fun (stmt : AST.statement) ->
      match stmt.stmt with
      | AST.ExportDecl _ | AST.ImportDecl _ -> true
      | _ -> false)
    program

let seed_signature_exports (signature : Module_sig.module_signature) (env : Infer.type_env) : Infer.type_env =
  Hashtbl.fold
    (fun _name (binding : Module_sig.member_binding) env_acc ->
      let env_acc =
        match binding.value_type with
        | Some poly -> Infer.TypeEnv.add binding.internal_name poly env_acc
        | None -> env_acc
      in
      Option.iter Enum_registry.register binding.enum_def;
      Option.iter Type_registry.register_named_type binding.named_type_def;
      Option.iter (Annotation.register_type_alias_info ~name:binding.internal_name) binding.transparent_type;
      Option.iter Type_registry.register_shape binding.shape_def;
      Option.iter Trait_registry.register_trait binding.trait_def;
      env_acc)
    signature.exports env

let seed_type_impls (entries : Module_sig.type_impl_entry list) : (unit, Diagnostic.t) result =
  let rec seed_methods for_type = function
    | [] -> Ok ()
    | method_sig :: rest -> (
        match Inherent_registry.register_method ~for_type method_sig with
        | Ok () -> seed_methods for_type rest
        | Error message -> Error (compiler_error ~code:"module-type-impl-register" ~message))
  in
  let rec go = function
    | [] -> Ok ()
    | (entry : Module_sig.type_impl_entry) :: rest -> (
        let* () = seed_methods entry.for_type entry.methods in
        go rest)
  in
  go entries

let seed_visible_impls (signature : Module_sig.module_signature) : (unit, Diagnostic.t) result =
  let rec seed_trait_impls = function
    | [] -> Ok ()
    | impl_def :: rest -> (
        try
          Trait_registry.register_impl impl_def;
          seed_trait_impls rest
        with Failure message -> Error (compiler_error ~code:"module-trait-impl-register" ~message))
  in
  let* () = seed_trait_impls signature.trait_impls in
  seed_type_impls signature.type_impls

let collect_inherent_target_generics (te : AST.type_expr) =
  let is_known_type_name (name : string) =
    Option.is_some (Annotation.builtin_primitive_type name)
    || Option.is_some (Annotation.builtin_type_constructor_name name)
  in
  let rec go ~(in_head : bool) acc = function
    | AST.TCon name ->
        if in_head || is_known_type_name name then
          acc
        else
          Import_resolver.StringSet.add name acc
    | AST.TVar name ->
        if in_head then
          acc
        else
          Import_resolver.StringSet.add name acc
    | AST.TTraitObject _ -> acc
    | AST.TApp (_name, args) -> List.fold_left (go ~in_head:false) acc args
    | AST.TArrow (params, ret, _) ->
        let acc = List.fold_left (go ~in_head:false) acc params in
        go ~in_head:false acc ret
    | AST.TUnion members | AST.TIntersection members -> List.fold_left (go ~in_head:false) acc members
    | AST.TRecord (fields, _row) ->
        List.fold_left
          (fun inner (field : AST.record_type_field) -> go ~in_head:false inner field.field_type)
          acc fields
  in
  go ~in_head:true Import_resolver.StringSet.empty te

let canonical_type_equal left right =
  Types.canonicalize_mono_type left = Types.canonicalize_mono_type right

let find_trait_impl ~(trait_name : string) ~(for_type : Types.mono_type) : Trait_registry.impl_def option =
  let canonical_trait = Trait_registry.canonical_trait_name trait_name in
  let canonical_for_type = Types.canonicalize_mono_type for_type in
  Trait_registry.all_impls ()
  |> List.find_opt (fun (impl_def : Trait_registry.impl_def) ->
         String.equal (Trait_registry.canonical_trait_name impl_def.impl_trait_name) canonical_trait
         && canonical_type_equal impl_def.impl_for_type canonical_for_type)

let merge_type_impl_entry (entries : Module_sig.type_impl_entry list) (entry : Module_sig.type_impl_entry) :
    Module_sig.type_impl_entry list =
  let rec go acc = function
    | [] -> List.rev (entry :: acc)
    | (existing : Module_sig.type_impl_entry) :: rest when canonical_type_equal existing.for_type entry.for_type ->
        let merged = { existing with methods = existing.methods @ entry.methods } in
        List.rev_append acc (merged :: rest)
    | (existing : Module_sig.type_impl_entry) :: rest -> go (existing :: acc) rest
  in
  go [] entries

let required_opt ~(code : string) ~(message : string) = function
  | Some value -> Ok value
  | None -> Error (compiler_error ~code ~message)

let extract_module_locals (program : AST.program) : (Module_sig.module_locals, Diagnostic.t) result =
  let rec go enums named_types transparent_types shapes traits trait_impls type_impls = function
    | [] ->
        Ok
          {
            Module_sig.enums = List.rev enums;
            named_types = List.rev named_types;
            transparent_types = List.rev transparent_types;
            shapes = List.rev shapes;
            traits = List.rev traits;
            trait_impls = List.rev trait_impls;
            type_impls = List.rev type_impls;
          }
    | (stmt : AST.statement) :: rest -> (
        match stmt.stmt with
        | AST.EnumDef { name; _ } ->
            let* enum_def =
              required_opt ~code:"module-signature-enum"
                ~message:(Printf.sprintf "Missing enum registry entry for '%s'" name)
                (Enum_registry.lookup name)
            in
            go (enum_def :: enums) named_types transparent_types shapes traits trait_impls type_impls rest
        | AST.TypeDef { type_name; _ } ->
            let* named_type_def =
              required_opt ~code:"module-signature-type"
                ~message:(Printf.sprintf "Missing named type registry entry for '%s'" type_name)
                (Type_registry.lookup_named_type type_name)
            in
            go enums (named_type_def :: named_types) transparent_types shapes traits trait_impls type_impls rest
        | AST.TypeAlias { alias_name; _ } ->
            let* alias_info =
              required_opt ~code:"module-signature-alias"
                ~message:(Printf.sprintf "Missing type alias registry entry for '%s'" alias_name)
                (Annotation.lookup_type_alias alias_name)
            in
            go enums named_types ((alias_name, alias_info) :: transparent_types) shapes traits trait_impls
              type_impls rest
        | AST.ShapeDef { shape_name; _ } ->
            let* shape_def =
              required_opt ~code:"module-signature-shape"
                ~message:(Printf.sprintf "Missing shape registry entry for '%s'" shape_name)
                (Type_registry.lookup_shape shape_name)
            in
            go enums named_types transparent_types (shape_def :: shapes) traits trait_impls type_impls rest
        | AST.TraitDef { name; _ } ->
            let* trait_def =
              required_opt ~code:"module-signature-trait"
                ~message:(Printf.sprintf "Missing trait registry entry for '%s'" name)
                (Trait_registry.lookup_trait name)
            in
            go enums named_types transparent_types shapes (trait_def :: traits) trait_impls type_impls rest
        | AST.ImplDef { impl_type_params; impl_trait_name; impl_for_type; _ } ->
            let type_bindings =
              List.map (fun (param : AST.generic_param) -> (param.name, Types.TVar param.name)) impl_type_params
            in
            let* for_type =
              Annotation.type_expr_to_mono_type_with type_bindings impl_for_type
              |> Result.map_error (fun (diag : Diagnostic.t) ->
                     compiler_error ~code:"module-signature-trait-impl" ~message:diag.message)
            in
            let* impl_def =
              required_opt ~code:"module-signature-trait-impl"
                ~message:
                  (Printf.sprintf "Missing trait impl registry entry for '%s' on %s" impl_trait_name
                     (Types.to_string for_type))
                (find_trait_impl ~trait_name:impl_trait_name ~for_type)
            in
            go enums named_types transparent_types shapes traits (impl_def :: trait_impls) type_impls rest
        | AST.InherentImplDef { inherent_for_type; inherent_methods } ->
            let type_bindings =
              Import_resolver.StringSet.elements (collect_inherent_target_generics inherent_for_type)
              |> List.map (fun name -> (name, Types.TVar name))
            in
            let* for_type =
              Annotation.type_expr_to_mono_type_with type_bindings inherent_for_type
              |> Result.map_error (fun (diag : Diagnostic.t) ->
                     compiler_error ~code:"module-signature-type-impl" ~message:diag.message)
            in
            let method_names = List.map (fun (method_impl : AST.method_impl) -> method_impl.impl_method_name) inherent_methods in
            let methods =
              Inherent_registry.all_methods ()
              |> List.filter_map (fun (registered_for_type, method_sig) ->
                     if
                       canonical_type_equal registered_for_type for_type
                       && List.mem method_sig.Trait_registry.method_name method_names
                     then
                       Some method_sig
                     else
                       None)
            in
            let type_impls = merge_type_impl_entry type_impls { Module_sig.for_type; methods } in
            go enums named_types transparent_types shapes traits trait_impls type_impls rest
        | AST.ExportDecl _ | AST.ImportDecl _ | AST.Let _ | AST.Return _ | AST.ExpressionStmt _ | AST.Block _
        | AST.DeriveDef _ ->
            go enums named_types transparent_types shapes traits trait_impls type_impls rest)
  in
  go [] [] [] [] [] [] [] program

let extract_module_signature
    ~(surface : Import_resolver.module_surface)
    ~(environment : Infer.type_env)
    ~(locals : Module_sig.module_locals) : (Module_sig.module_signature, Diagnostic.t) result =
  let exports = Hashtbl.create (Import_resolver.StringMap.cardinal surface.exports) in
  let named_type_map =
    List.fold_left
      (fun acc (def : Type_registry.named_type_def) -> Import_resolver.StringMap.add def.named_type_name def acc)
      Import_resolver.StringMap.empty locals.named_types
  in
  let enum_map =
    List.fold_left
      (fun acc (def : Enum_registry.enum_def) -> Import_resolver.StringMap.add def.name def acc)
      Import_resolver.StringMap.empty locals.enums
  in
  let alias_map =
    List.fold_left
      (fun acc (name, alias_info) -> Import_resolver.StringMap.add name alias_info acc)
      Import_resolver.StringMap.empty locals.transparent_types
  in
  let shape_map =
    List.fold_left
      (fun acc (def : Type_registry.shape_def) -> Import_resolver.StringMap.add def.shape_name def acc)
      Import_resolver.StringMap.empty locals.shapes
  in
  let trait_map =
    List.fold_left
      (fun acc (def : Trait_registry.trait_def) -> Import_resolver.StringMap.add def.trait_name def acc)
      Import_resolver.StringMap.empty locals.traits
  in
  let add_export surface_name (presence : Import_resolver.member_presence) =
    let value_type =
      if presence.has_value then
        Infer.TypeEnv.find_opt presence.internal_name environment
      else
        None
    in
    let binding =
      {
        Module_sig.internal_name = presence.internal_name;
        value_type;
        enum_def =
          (if presence.has_enum then
             Import_resolver.StringMap.find_opt presence.internal_name enum_map
           else
             None);
        named_type_def =
          (if presence.has_named_type then
             Import_resolver.StringMap.find_opt presence.internal_name named_type_map
           else
             None);
        transparent_type =
          (if presence.has_transparent_type then
             Import_resolver.StringMap.find_opt presence.internal_name alias_map
           else
             None);
        shape_def =
          (if presence.has_shape then
             Import_resolver.StringMap.find_opt presence.internal_name shape_map
           else
             None);
        trait_def =
          (if presence.has_trait then
             Import_resolver.StringMap.find_opt presence.internal_name trait_map
           else
             None);
      }
    in
    Hashtbl.replace exports surface_name binding
  in
  Import_resolver.StringMap.iter add_export surface.exports;
  Ok
    {
      Module_sig.module_id = surface.module_id;
      exports;
      trait_impls = locals.trait_impls;
      type_impls = locals.type_impls;
    }

let rewrite_project_modules
    ~(surfaces : (string, Import_resolver.module_surface) Hashtbl.t)
    (graph : Module_context.module_graph) :
    ((string, Import_resolver.rewrite_result) Hashtbl.t, Diagnostic.t list) result =
  let rewrites = Hashtbl.create (List.length graph.topo_order) in
  let rec go = function
    | [] -> Ok rewrites
    | module_id :: rest -> (
        match Hashtbl.find_opt graph.modules module_id with
        | None ->
            Error [ compiler_error ~code:"module-missing" ~message:(Printf.sprintf "Missing module '%s'" module_id) ]
        | Some module_info -> (
            let* rewrite =
              Import_resolver.rewrite_module ~surfaces module_info |> Result.map_error (fun diag -> [ diag ])
            in
            Hashtbl.replace rewrites module_id rewrite;
            go rest))
  in
  go graph.topo_order

let validate_build_wide_trait_impl_coherence
    ~(rewrites : (string, Import_resolver.rewrite_result) Hashtbl.t)
    (graph : Module_context.module_graph) : (unit, Diagnostic.t list) result =
  reset_module_state ();
  ignore (Builtins.prelude_env ());
  let programs =
    List.filter_map
      (fun module_id ->
        Hashtbl.find_opt rewrites module_id
        |> Option.map (fun (rewrite : Import_resolver.rewrite_result) -> rewrite.program))
      graph.topo_order
  in
  let* () =
    iter_result Infer.predeclare_top_level_named_declarations programs |> Result.map_error (fun diag -> [ diag ])
  in
  let* () =
    iter_result Infer.predeclare_top_level_type_aliases programs |> Result.map_error (fun diag -> [ diag ])
  in
  let* () =
    iter_result Infer.register_top_level_named_declarations programs |> Result.map_error (fun diag -> [ diag ])
  in
  let register_impl_stmt (stmt : AST.statement) =
    match stmt.stmt with
    | AST.ImplDef impl_def ->
        let type_bindings =
          List.map (fun (param : AST.generic_param) -> (param.name, Types.TVar param.name)) impl_def.impl_type_params
        in
        let* for_type =
          Annotation.type_expr_to_mono_type_with type_bindings impl_def.impl_for_type
          |> Result.map_error (fun (diag : Diagnostic.t) ->
                 [ error_at_stmt ~code:"module-trait-impl-register" ~message:diag.message stmt ])
        in
        (try
           Trait_registry.register_impl
             {
               Trait_registry.impl_trait_name = impl_def.impl_trait_name;
               impl_type_params = impl_def.impl_type_params;
               impl_for_type = for_type;
               impl_methods = [];
             };
           Ok ()
         with Failure message ->
           Error [ error_at_stmt ~code:"module-trait-impl-register" ~message stmt ])
    | _ -> Ok ()
  in
  iter_result
    (fun module_id ->
      match Hashtbl.find_opt rewrites module_id with
      | None -> Ok ()
      | Some rewrite -> iter_result register_impl_stmt rewrite.program)
    graph.topo_order

let compile_module
    ~(surfaces : (string, Import_resolver.module_surface) Hashtbl.t)
    ~(typed_signatures : (string, Module_sig.module_signature) Hashtbl.t)
    ~(rewrite : Import_resolver.rewrite_result)
    (module_info : Module_context.parsed_module) : (checked_module, Diagnostic.t list) result =
  reset_module_state ();
  let* env =
    let rec seed_imports env_acc = function
      | [] -> Ok env_acc
      | module_id :: rest -> (
          match Hashtbl.find_opt typed_signatures module_id with
          | None -> seed_imports env_acc rest
          | Some signature ->
              let env_acc = seed_signature_exports signature env_acc in
              let* () = seed_visible_impls signature |> Result.map_error (fun diag -> [ diag ]) in
              seed_imports env_acc rest)
    in
    seed_imports (Builtins.prelude_env ()) rewrite.resolved_imports.direct_modules
  in
  let* result =
    Checker.check_program_with_annotations ~prepare_state:false ~env rewrite.program
    |> Result.map_error (fun diags -> diags)
  in
  let* locals = extract_module_locals rewrite.program |> Result.map_error (fun diag -> [ diag ]) in
  let* surface =
    match Hashtbl.find_opt surfaces module_info.module_id with
    | Some surface -> Ok surface
    | None ->
        Error
          [
            compiler_error ~code:"module-surface-missing"
              ~message:(Printf.sprintf "Missing module surface for '%s'" module_info.module_id);
          ]
  in
  let* signature =
    extract_module_signature ~surface ~environment:result.environment ~locals
    |> Result.map_error (fun diag -> [ diag ])
  in
  Ok
    {
      module_id = module_info.module_id;
      file_path = module_info.file_path;
      program = rewrite.program;
      result;
      signature;
      locals;
    }

let merge_checked_modules (modules : checked_module list) : compiled_project =
  let program = List.concat_map (fun (m : checked_module) -> m.program) modules in
  let environment =
    List.fold_left
      (fun env_acc (m : checked_module) ->
        Infer.TypeEnv.union (fun _ existing _ -> Some existing) env_acc m.result.environment)
      Infer.empty_env modules
  in
  let type_map = Hashtbl.create 512 in
  let call_resolution_map = Hashtbl.create 256 in
  let method_type_args_map = Hashtbl.create 128 in
  let method_def_map = Hashtbl.create 128 in
  let trait_object_coercion_map = Hashtbl.create 128 in
  let placeholder_rewrite_map = Hashtbl.create 128 in
  let diagnostics =
    modules |> List.concat_map (fun (m : checked_module) -> m.result.diagnostics)
  in
  List.iter
    (fun (m : checked_module) ->
      merge_hashtbl type_map m.result.type_map;
      merge_hashtbl call_resolution_map m.result.call_resolution_map;
      merge_hashtbl method_type_args_map m.result.method_type_args_map;
      merge_hashtbl method_def_map m.result.method_def_map;
      merge_hashtbl trait_object_coercion_map m.result.trait_object_coercion_map;
      merge_hashtbl placeholder_rewrite_map m.result.placeholder_rewrite_map)
    modules;
  {
    modules;
    program;
    environment;
    type_map;
    call_resolution_map;
    method_type_args_map;
    method_def_map;
    trait_object_coercion_map;
    placeholder_rewrite_map;
    diagnostics;
  }

let compile_project (graph : Module_context.module_graph) : (compiled_project, Diagnostic.t list) result =
  let* surfaces = Import_resolver.build_module_surfaces graph |> Result.map_error (fun diag -> [ diag ]) in
  let* rewrites = rewrite_project_modules ~surfaces graph in
  let* () = validate_build_wide_trait_impl_coherence ~rewrites graph in
  let typed_signatures = Hashtbl.create (List.length graph.topo_order) in
  let rec go acc = function
    | [] -> Ok (merge_checked_modules (List.rev acc))
    | module_id :: rest -> (
        match Hashtbl.find_opt graph.modules module_id with
        | None ->
            Error [ compiler_error ~code:"module-missing" ~message:(Printf.sprintf "Missing module '%s'" module_id) ]
        | Some module_info -> (
            let* rewrite =
              match Hashtbl.find_opt rewrites module_id with
              | Some rewrite -> Ok rewrite
              | None ->
                  Error
                    [ compiler_error ~code:"module-rewrite-missing" ~message:(Printf.sprintf "Missing rewrite for '%s'" module_id) ]
            in
            let* checked_module = compile_module ~surfaces ~typed_signatures ~rewrite module_info in
            Hashtbl.replace typed_signatures module_id checked_module.signature;
            go (checked_module :: acc) rest))
  in
  go [] graph.topo_order

let seed_module_locals (locals : Module_sig.module_locals) : (unit, Diagnostic.t) result =
  List.iter Enum_registry.register locals.enums;
  List.iter Type_registry.register_named_type locals.named_types;
  List.iter (fun (name, alias_info) -> Annotation.register_type_alias_info ~name alias_info) locals.transparent_types;
  List.iter Type_registry.register_shape locals.shapes;
  List.iter Trait_registry.register_trait locals.traits;
  let rec seed_trait_impls = function
    | [] -> Ok ()
    | impl_def :: rest -> (
        try
          Trait_registry.register_impl impl_def;
          seed_trait_impls rest
        with Failure message -> Error (compiler_error ~code:"module-trait-impl-register" ~message))
  in
  let* () = seed_trait_impls locals.trait_impls in
  seed_type_impls locals.type_impls

let emit_compiled_project (project : compiled_project) : (Codegen.build_output, Diagnostic.t list) result =
  reset_module_state ();
  ignore (Builtins.prelude_env ());
  let* () =
    let rec seed = function
      | [] -> Ok ()
      | (module_ : checked_module) :: rest ->
          let* () = seed_module_locals module_.locals in
          seed rest
    in
    seed project.modules |> Result.map_error (fun diag -> [ diag ])
  in
  try
    let main_go =
      Codegen.emit_program_with_typed_env ~call_resolution_map:project.call_resolution_map
        ~method_type_args_map:project.method_type_args_map ~method_def_map:project.method_def_map
        ~trait_object_coercion_map:project.trait_object_coercion_map
        ~placeholder_rewrite_map:project.placeholder_rewrite_map project.type_map project.environment
        project.program
    in
    Ok { Codegen.main_go; runtime_go = Codegen.get_runtime (); diagnostics = project.diagnostics }
  with
  | Failure message -> Error [ Codegen.diagnostic_of_codegen_failure_message message ]
  | exn ->
      let message = Codegen.normalize_codegen_failure_message (Printexc.to_string exn) in
      Error [ Diagnostic.error_no_span ~code:"codegen-internal" ~message ]

let compile_project_to_build (graph : Module_context.module_graph) : (Codegen.build_output, Diagnostic.t list) result =
  let* project = compile_project graph in
  emit_compiled_project project

let check_entry ~(entry_file : string) : (Diagnostic.t list, Diagnostic.t list) result =
  match Discovery.discover_project ~entry_file with
  | Error diag -> Error [ diag ]
  | Ok graph -> (
      match Hashtbl.find_opt graph.modules graph.entry_module with
      | Some entry_module
        when Hashtbl.length graph.modules = 1 && not (has_module_headers entry_module.program) ->
          reset_module_state ();
          let env = Builtins.prelude_env () in
          Checker.check_program_with_annotations ~env entry_module.program
          |> Result.map (fun (result : Checker.typecheck_result) -> result.diagnostics)
      | _ -> (
          match compile_project graph with
          | Ok project -> Ok project.diagnostics
          | Error diags -> Error diags))

let compile_entry_to_build ~(entry_file : string) : (Codegen.build_output, Diagnostic.t list) result =
  let* graph = Discovery.discover_project ~entry_file |> Result.map_error (fun diag -> [ diag ]) in
  match Hashtbl.find_opt graph.modules graph.entry_module with
  | Some entry_module
    when Hashtbl.length graph.modules = 1 && not (has_module_headers entry_module.program) ->
      Codegen.compile_to_build ~file_id:entry_module.file_path entry_module.source
  | _ -> compile_project_to_build graph

let%test "compile_project rewrites namespace imports to internal names" =
  Discovery.with_temp_project
    [
      ("main.mr", "import math\nputs(math.add(1, 2))\n");
      ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
    ]
    (fun root ->
      match Discovery.discover_project ~entry_file:(Filename.concat root "main.mr") with
      | Error _ -> false
      | Ok graph -> (
          match compile_project graph with
          | Error _ -> false
          | Ok project ->
              project.program
              |> List.exists (fun (stmt : AST.statement) ->
                     match stmt.stmt with
                     | AST.ExpressionStmt { expr = AST.Call ({ expr = AST.Identifier name; _ }, _); _ } ->
                         String.equal name "puts" || String.equal name "math__add"
                     | _ -> false)))

let%test "compile_entry_to_build keeps legacy single-file path working" =
  Discovery.with_temp_project
    [ ("main.mr", "fn add(x: Int, y: Int) -> Int = x + y\nputs(add(1, 2))\n") ]
    (fun root ->
      match compile_entry_to_build ~entry_file:(Filename.concat root "main.mr") with
      | Error _ -> false
      | Ok build_output -> Diagnostics.String_utils.contains_substring ~needle:"func add_" build_output.main_go)

let%test "check_entry rejects colliding direct imports" =
  Discovery.with_temp_project
    [
      ("main.mr", "import a.add\nimport b.add\nputs(add(1, 2))\n");
      ("a.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
      ("b.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y + 10\n");
    ]
    (fun root ->
      match check_entry ~entry_file:(Filename.concat root "main.mr") with
      | Ok _ -> false
      | Error diags ->
          List.exists
            (fun (diag : Diagnostic.t) ->
              diag.code = "module-import-name-collision"
              && Diagnostics.String_utils.contains_substring ~needle:"existing binding 'add'" diag.message)
            diags)

let%test "compile_project rejects duplicate impls across transitive imports" =
  Discovery.with_temp_project
    [
      ("main.mr", "import x\nimport y\nputs(1)\n");
      ("point.mr", "export Point, Printer\ntype Point = { x: Int, y: Int }\ntrait Printer[a] = { fn print(x: a) -> Str }\n");
      ("a_impl.mr", "import point.Point\nimport point.Printer\nimpl Printer[Point] = { fn print(p: Point) -> Str = \"a\" }\n");
      ("b_impl.mr", "import point.Point\nimport point.Printer\nimpl Printer[Point] = { fn print(p: Point) -> Str = \"b\" }\n");
      ("x.mr", "import a_impl\nexport x\nlet x = 1\n");
      ("y.mr", "import b_impl\nexport y\nlet y = 2\n");
    ]
    (fun root ->
      match Discovery.discover_project ~entry_file:(Filename.concat root "main.mr") with
      | Error _ -> false
      | Ok graph -> (
          match compile_project graph with
          | Ok _ -> false
          | Error diags ->
              List.exists
                (fun (diag : Diagnostic.t) ->
                  diag.code = "module-trait-impl-register"
                  && Diagnostics.String_utils.contains_substring ~needle:"Duplicate impl registration for trait" diag.message)
                diags))
