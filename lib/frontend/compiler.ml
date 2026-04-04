module AST = Syntax.Ast.AST
module Diagnostic = Diagnostics.Diagnostic
module Discovery = Discovery
module Module_context = Module_context
module Import_resolver = Import_resolver
module Parser = Syntax.Parser
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

type module_navigation = {
  surface : Import_resolver.module_surface;
  resolved_imports : Import_resolver.resolved_imports;
}

type checked_module = {
  module_id : string;
  file_path : string;
  program : AST.program;
  result : Checker.typecheck_result;
  type_var_user_names : (string * string) list;
  signature : Module_sig.module_signature;
  locals : Module_sig.module_locals;
  navigation : module_navigation;
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
  symbol_table : (Infer.symbol_id * Infer.symbol) list;
  identifier_symbols : (int * Infer.symbol_id) list;
  diagnostics : Diagnostic.t list;
}

type analysis_mode =
  | Standalone
  | Modules

type file_analysis = {
  file_path : string;
  module_id : string option;
  source : string;
  surface_program : AST.program option;
  typed_program : AST.program option;
  type_map : Infer.type_map option;
  environment : Infer.type_env option;
  type_var_user_names : (string * string) list;
  symbol_table : (Infer.symbol_id * Infer.symbol) list;
  identifier_symbols : (int * Infer.symbol_id) list;
  diagnostics : Diagnostic.t list;
}

type entry_analysis = {
  mode : analysis_mode;
  source_root : string option;
  project_root : string option;
  graph : Module_context.module_graph option;
  project : compiled_project option;
  active_file : file_analysis;
}

let ( let* ) = Result.bind
let compiler_error ~(code : string) ~(message : string) : Diagnostic.t = Diagnostic.error_no_span ~code ~message
let merge_hashtbl dst src = Hashtbl.iter (fun key value -> Hashtbl.replace dst key value) src

let diagnostics_have_errors (diagnostics : Diagnostic.t list) : bool =
  List.exists (fun (diag : Diagnostic.t) -> diag.severity = Diagnostic.Error) diagnostics

let resolved_project_root ?source_root ~(entry_file : string) () : string =
  match source_root with
  | Some root -> Discovery.normalize_path root
  | None -> Filename.dirname (Discovery.normalize_path entry_file)

let read_source_file (path : string) : string =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

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

let prebound_value_symbols_of_signature (signature : Module_sig.module_signature) :
    (string * Infer.prebound_symbol_info) list =
  Hashtbl.fold
    (fun _name (binding : Module_sig.member_binding) acc ->
      match (binding.value_type, binding.value_definition) with
      | Some _, Some definition ->
          ( binding.internal_name,
            {
              Infer.kind = Infer.TopLevelLet;
              definition_pos = definition.start_pos;
              definition_end_pos = definition.end_pos;
              file_id = Some definition.file_path;
            } )
          :: acc
      | _ -> acc)
    signature.exports []

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
    | (entry : Module_sig.type_impl_entry) :: rest ->
        let* () = seed_methods entry.for_type entry.methods in
        go rest
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

let canonical_type_equal left right = Types.canonicalize_mono_type left = Types.canonicalize_mono_type right

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
    | (existing : Module_sig.type_impl_entry) :: rest when canonical_type_equal existing.for_type entry.for_type
      ->
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
            go enums named_types
              ((alias_name, alias_info) :: transparent_types)
              shapes traits trait_impls type_impls rest
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
            let method_names =
              List.map (fun (method_impl : AST.method_impl) -> method_impl.impl_method_name) inherent_methods
            in
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
        value_definition = presence.value_definition;
        enum_def =
          (if presence.has_enum then
             Import_resolver.StringMap.find_opt presence.internal_name enum_map
           else
             None);
        enum_definition = presence.enum_definition;
        named_type_def =
          (if presence.has_named_type then
             Import_resolver.StringMap.find_opt presence.internal_name named_type_map
           else
             None);
        named_type_definition = presence.named_type_definition;
        transparent_type =
          (if presence.has_transparent_type then
             Import_resolver.StringMap.find_opt presence.internal_name alias_map
           else
             None);
        transparent_type_definition = presence.transparent_type_definition;
        shape_def =
          (if presence.has_shape then
             Import_resolver.StringMap.find_opt presence.internal_name shape_map
           else
             None);
        shape_definition = presence.shape_definition;
        trait_def =
          (if presence.has_trait then
             Import_resolver.StringMap.find_opt presence.internal_name trait_map
           else
             None);
        trait_definition = presence.trait_definition;
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
    ~(surfaces : (string, Import_resolver.module_surface) Hashtbl.t) (graph : Module_context.module_graph) :
    ((string, Import_resolver.rewrite_result) Hashtbl.t, Diagnostic.t list) result =
  let rewrites = Hashtbl.create (List.length graph.topo_order) in
  let rec go = function
    | [] -> Ok rewrites
    | module_id :: rest -> (
        match Hashtbl.find_opt graph.modules module_id with
        | None ->
            Error
              [ compiler_error ~code:"module-missing" ~message:(Printf.sprintf "Missing module '%s'" module_id) ]
        | Some module_info ->
            let* rewrite =
              Import_resolver.rewrite_module ~surfaces module_info |> Result.map_error (fun diag -> [ diag ])
            in
            Hashtbl.replace rewrites module_id rewrite;
            go rest)
  in
  go graph.topo_order

let validate_build_wide_trait_impl_coherence
    ~(rewrites : (string, Import_resolver.rewrite_result) Hashtbl.t) (graph : Module_context.module_graph) :
    (unit, Diagnostic.t list) result =
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
    | AST.ImplDef impl_def -> (
        let type_bindings =
          List.map
            (fun (param : AST.generic_param) -> (param.name, Types.TVar param.name))
            impl_def.impl_type_params
        in
        let* for_type =
          Annotation.type_expr_to_mono_type_with type_bindings impl_def.impl_for_type
          |> Result.map_error (fun (diag : Diagnostic.t) ->
                 [ error_at_stmt ~code:"module-trait-impl-register" ~message:diag.message stmt ])
        in
        try
          Trait_registry.register_impl
            {
              Trait_registry.impl_trait_name = impl_def.impl_trait_name;
              impl_type_params = impl_def.impl_type_params;
              impl_for_type = for_type;
              impl_methods = [];
            };
          Ok ()
        with Failure message -> Error [ error_at_stmt ~code:"module-trait-impl-register" ~message stmt ])
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
  let state = Infer.create_inference_state () in
  let* env, prebound_symbols =
    let rec seed_imports env_acc prebound_symbols_acc = function
      | [] -> Ok (env_acc, List.rev prebound_symbols_acc)
      | module_id :: rest -> (
          match Hashtbl.find_opt typed_signatures module_id with
          | None -> seed_imports env_acc prebound_symbols_acc rest
          | Some signature ->
              let env_acc = seed_signature_exports signature env_acc in
              let* () = seed_visible_impls signature |> Result.map_error (fun diag -> [ diag ]) in
              seed_imports env_acc
                (List.rev_append (prebound_value_symbols_of_signature signature) prebound_symbols_acc)
                rest)
    in
    seed_imports (Builtins.prelude_env ()) [] rewrite.resolved_imports.direct_modules
  in
  let* result =
    Checker.check_program_with_annotations ~state ~prebound_symbols ~prepare_state:false ~env rewrite.program
    |> Result.map_error (fun diags -> diags)
  in
  let type_var_user_names = Infer.type_var_user_name_bindings_in_state state in
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
      type_var_user_names;
      signature;
      locals;
      navigation = { surface; resolved_imports = rewrite.resolved_imports };
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
  let symbol_table = Hashtbl.create 256 in
  let identifier_symbols = Hashtbl.create 256 in
  let diagnostics = modules |> List.concat_map (fun (m : checked_module) -> m.result.diagnostics) in
  List.iter
    (fun (m : checked_module) ->
      merge_hashtbl type_map m.result.type_map;
      merge_hashtbl call_resolution_map m.result.call_resolution_map;
      merge_hashtbl method_type_args_map m.result.method_type_args_map;
      merge_hashtbl method_def_map m.result.method_def_map;
      merge_hashtbl trait_object_coercion_map m.result.trait_object_coercion_map;
      merge_hashtbl placeholder_rewrite_map m.result.placeholder_rewrite_map;
      List.iter (fun (symbol_id, symbol) -> Hashtbl.replace symbol_table symbol_id symbol) m.result.symbol_table;
      List.iter
        (fun (expr_id, symbol_id) -> Hashtbl.replace identifier_symbols expr_id symbol_id)
        m.result.identifier_symbols)
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
    symbol_table = Hashtbl.to_seq symbol_table |> List.of_seq;
    identifier_symbols = Hashtbl.to_seq identifier_symbols |> List.of_seq;
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
            Error
              [ compiler_error ~code:"module-missing" ~message:(Printf.sprintf "Missing module '%s'" module_id) ]
        | Some module_info ->
            let* rewrite =
              match Hashtbl.find_opt rewrites module_id with
              | Some rewrite -> Ok rewrite
              | None ->
                  Error
                    [
                      compiler_error ~code:"module-rewrite-missing"
                        ~message:(Printf.sprintf "Missing rewrite for '%s'" module_id);
                    ]
            in
            let* checked_module = compile_module ~surfaces ~typed_signatures ~rewrite module_info in
            Hashtbl.replace typed_signatures module_id checked_module.signature;
            go (checked_module :: acc) rest)
  in
  go [] graph.topo_order

let seed_module_locals (locals : Module_sig.module_locals) : (unit, Diagnostic.t) result =
  List.iter Enum_registry.register locals.enums;
  List.iter Type_registry.register_named_type locals.named_types;
  List.iter
    (fun (name, alias_info) -> Annotation.register_type_alias_info ~name alias_info)
    locals.transparent_types;
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

let compile_project_to_build (graph : Module_context.module_graph) :
    (Codegen.build_output, Diagnostic.t list) result =
  let* project = compile_project graph in
  emit_compiled_project project

let make_file_analysis
    ~(file_path : string)
    ?module_id
    ~(source : string)
    ?surface_program
    ?typed_program
    ?type_map
    ?environment
    ?(type_var_user_names = [])
    ?(symbol_table = [])
    ?(identifier_symbols = [])
    ~(diagnostics : Diagnostic.t list)
    () : file_analysis =
  {
    file_path;
    module_id;
    source;
    surface_program;
    typed_program;
    type_map;
    environment;
    type_var_user_names;
    symbol_table;
    identifier_symbols;
    diagnostics;
  }

let find_symbol_by_id (symbol_table : (Infer.symbol_id * Infer.symbol) list) (symbol_id : Infer.symbol_id) :
    Infer.symbol option =
  List.find_map
    (fun (candidate_id, symbol) ->
      if candidate_id = symbol_id then
        Some symbol
      else
        None)
    symbol_table

let find_active_file_symbol (analysis : entry_analysis) ~(expr_id : int) : Infer.symbol option =
  match
    List.find_map
      (fun (candidate_expr_id, symbol_id) ->
        if candidate_expr_id = expr_id then
          Some symbol_id
        else
          None)
      analysis.active_file.identifier_symbols
  with
  | None -> None
  | Some symbol_id -> find_symbol_by_id analysis.active_file.symbol_table symbol_id

let find_parsed_module_by_file (analysis : entry_analysis) ~(file_path : string) :
    Module_context.parsed_module option =
  match analysis.graph with
  | None -> None
  | Some graph ->
      Hashtbl.to_seq_values graph.modules
      |> List.of_seq
      |> List.find_opt (fun (module_info : Module_context.parsed_module) ->
             String.equal module_info.file_path file_path)

let find_checked_module_by_file (analysis : entry_analysis) ~(file_path : string) : checked_module option =
  match analysis.project with
  | None -> None
  | Some project ->
      List.find_opt (fun (module_ : checked_module) -> String.equal module_.file_path file_path) project.modules

let find_checked_module_by_id (analysis : entry_analysis) ~(module_id : string) : checked_module option =
  match analysis.project with
  | None -> None
  | Some project ->
      List.find_opt (fun (module_ : checked_module) -> String.equal module_.module_id module_id) project.modules

let find_export_binding (analysis : entry_analysis) ~(module_id : string) ~(surface_name : string) :
    Module_sig.member_binding option =
  match find_checked_module_by_id analysis ~module_id with
  | None -> None
  | Some module_ -> Module_sig.find_export module_.signature surface_name

let analyze_standalone_program ?source_root ~(entry_file : string) ~(source : string) (program : AST.program) :
    entry_analysis =
  reset_module_state ();
  let env = Builtins.prelude_env () in
  let state = Infer.create_inference_state () in
  match Checker.check_program_with_annotations ~state ~env program with
  | Error diags ->
      let active_file =
        make_file_analysis ~file_path:entry_file ~source ~surface_program:program ~diagnostics:diags
          ~type_var_user_names:(Infer.type_var_user_name_bindings_in_state state)
          ~symbol_table:(Infer.symbol_table_bindings_in_state state)
          ~identifier_symbols:(Infer.identifier_symbol_bindings_in_state state)
          ()
      in
      { mode = Standalone; source_root; project_root = None; graph = None; project = None; active_file }
  | Ok result ->
      let active_file =
        make_file_analysis ~file_path:entry_file ~source ~surface_program:program ~typed_program:program
          ~type_map:result.type_map ~environment:result.environment ~diagnostics:result.diagnostics
          ~type_var_user_names:(Infer.type_var_user_name_bindings_in_state state)
          ~symbol_table:result.symbol_table ~identifier_symbols:result.identifier_symbols ()
      in
      { mode = Standalone; source_root; project_root = None; graph = None; project = None; active_file }

let analyze_module_graph
    ~(source_root : string option)
    ~(project_root : string)
    ~(entry_file : string)
    ~(source : string)
    ~(entry_program : AST.program)
    (graph : Module_context.module_graph) : entry_analysis =
  match Hashtbl.find_opt graph.modules graph.entry_module with
  | None ->
      let active_file =
        make_file_analysis ~file_path:entry_file ~module_id:graph.entry_module ~source
          ~surface_program:entry_program
          ~diagnostics:
            [
              compiler_error ~code:"module-missing"
                ~message:(Printf.sprintf "Missing entry module '%s'" graph.entry_module);
            ]
          ()
      in
      {
        mode = Modules;
        source_root;
        project_root = Some project_root;
        graph = Some graph;
        project = None;
        active_file;
      }
  | Some entry_module -> (
      match compile_project graph with
      | Error diags ->
          let active_file =
            make_file_analysis ~file_path:entry_module.file_path ~module_id:entry_module.module_id
              ~source:entry_module.source ~surface_program:entry_module.program ~diagnostics:diags ()
          in
          {
            mode = Modules;
            source_root;
            project_root = Some project_root;
            graph = Some graph;
            project = None;
            active_file;
          }
      | Ok project -> (
          match
            List.find_opt
              (fun (module_ : checked_module) -> String.equal module_.module_id graph.entry_module)
              project.modules
          with
          | None ->
              let active_file =
                make_file_analysis ~file_path:entry_module.file_path ~module_id:entry_module.module_id
                  ~source:entry_module.source ~surface_program:entry_module.program
                  ~diagnostics:
                    [
                      compiler_error ~code:"module-missing"
                        ~message:(Printf.sprintf "Missing checked entry module '%s'" graph.entry_module);
                    ]
                  ()
              in
              {
                mode = Modules;
                source_root;
                project_root = Some project_root;
                graph = Some graph;
                project = Some project;
                active_file;
              }
          | Some checked_entry ->
              let active_file =
                make_file_analysis ~file_path:entry_module.file_path ~module_id:entry_module.module_id
                  ~source:entry_module.source ~surface_program:entry_module.program
                  ~typed_program:checked_entry.program ~type_map:checked_entry.result.type_map
                  ~environment:checked_entry.result.environment
                  ~type_var_user_names:checked_entry.type_var_user_names
                  ~symbol_table:checked_entry.result.symbol_table
                  ~identifier_symbols:checked_entry.result.identifier_symbols ~diagnostics:project.diagnostics ()
              in
              {
                mode = Modules;
                source_root;
                project_root = Some project_root;
                graph = Some graph;
                project = Some project;
                active_file;
              }))

let rec analyze_entry_with_source
    ?source_root ?(force_modules = false) ~(entry_file : string) ~(entry_source : string) () : entry_analysis =
  analyze_entry_with_overrides ?source_root ~force_modules ~entry_file ~entry_source
    ~source_overrides:(Hashtbl.create 0) ()

and analyze_entry_with_overrides
    ?source_root
    ?(force_modules = false)
    ~(entry_file : string)
    ~(entry_source : string)
    ~(source_overrides : (string, string) Hashtbl.t)
    () : entry_analysis =
  match Parser.parse ~file_id:entry_file entry_source with
  | Error diags ->
      let active_file = make_file_analysis ~file_path:entry_file ~source:entry_source ~diagnostics:diags () in
      { mode = Standalone; source_root; project_root = None; graph = None; project = None; active_file }
  | Ok entry_program -> (
      if (not force_modules) && not (has_module_headers entry_program) then
        analyze_standalone_program ?source_root ~entry_file ~source:entry_source entry_program
      else
        let project_root = resolved_project_root ?source_root ~entry_file () in
        let all_overrides = Hashtbl.copy source_overrides in
        Hashtbl.replace all_overrides (Discovery.normalize_path entry_file) entry_source;
        match
          Discovery.discover_project_with_overrides ?source_root ~entry_file ~source_overrides:all_overrides ()
        with
        | Error diag ->
            let active_file =
              make_file_analysis ~file_path:entry_file ~source:entry_source ~surface_program:entry_program
                ~diagnostics:[ diag ] ()
            in
            {
              mode = Modules;
              source_root;
              project_root = Some project_root;
              graph = None;
              project = None;
              active_file;
            }
        | Ok graph ->
            analyze_module_graph ~source_root ~project_root:graph.root_dir ~entry_file ~source:entry_source
              ~entry_program graph)

let analyze_entry ?source_root ?(force_modules = false) ~(entry_file : string) () : entry_analysis =
  analyze_entry_with_source ?source_root ~force_modules ~entry_file ~entry_source:(read_source_file entry_file) ()

let check_graph (graph : Module_context.module_graph) : (Diagnostic.t list, Diagnostic.t list) result =
  match Hashtbl.find_opt graph.modules graph.entry_module with
  | Some entry_module when Hashtbl.length graph.modules = 1 && not (has_module_headers entry_module.program) ->
      reset_module_state ();
      let env = Builtins.prelude_env () in
      Checker.check_program_with_annotations ~env entry_module.program
      |> Result.map (fun (result : Checker.typecheck_result) -> result.diagnostics)
  | _ -> (
      match compile_project graph with
      | Ok project -> Ok project.diagnostics
      | Error diags -> Error diags)

let check_entry ?source_root ?(force_modules = false) ~(entry_file : string) :
    unit -> (Diagnostic.t list, Diagnostic.t list) result =
 fun () ->
  let analysis = analyze_entry ?source_root ~force_modules ~entry_file () in
  if diagnostics_have_errors analysis.active_file.diagnostics then
    Error analysis.active_file.diagnostics
  else
    Ok analysis.active_file.diagnostics

let check_entry_with_source ?source_root ?(force_modules = false) ~(entry_file : string) ~(entry_source : string)
    : unit -> (Diagnostic.t list, Diagnostic.t list) result =
 fun () ->
  let analysis = analyze_entry_with_source ?source_root ~force_modules ~entry_file ~entry_source () in
  if diagnostics_have_errors analysis.active_file.diagnostics then
    Error analysis.active_file.diagnostics
  else
    Ok analysis.active_file.diagnostics

let compile_entry_to_build ?source_root ~(entry_file : string) () :
    (Codegen.build_output, Diagnostic.t list) result =
  let* graph =
    Discovery.discover_project ?source_root ~entry_file () |> Result.map_error (fun diag -> [ diag ])
  in
  match Hashtbl.find_opt graph.modules graph.entry_module with
  | Some entry_module when Hashtbl.length graph.modules = 1 && not (has_module_headers entry_module.program) ->
      Codegen.compile_to_build ~file_id:entry_module.file_path entry_module.source
  | _ -> compile_project_to_build graph

let%test "compile_project rewrites namespace imports to internal names" =
  Discovery.with_temp_project
    [
      ("main.mr", "import math\nputs(math.add(1, 2))\n");
      ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
    ]
    (fun root ->
      match Discovery.discover_project ~entry_file:(Filename.concat root "main.mr") () with
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
      match compile_entry_to_build ~entry_file:(Filename.concat root "main.mr") () with
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
      match check_entry ~entry_file:(Filename.concat root "main.mr") () with
      | Ok _ -> false
      | Error diags ->
          List.exists
            (fun (diag : Diagnostic.t) ->
              diag.code = "module-import-name-collision"
              && Diagnostics.String_utils.contains_substring ~needle:"existing binding 'add'" diag.message)
            diags)

let%test "check_entry reports non-exported namespace members clearly" =
  Discovery.with_temp_project
    [ ("main.mr", "import sum\nsum.reduce\n"); ("sum.mr", "fn reduce(values: List[Int]) -> Int = len(values)\n") ]
    (fun root ->
      match check_entry ~entry_file:(Filename.concat root "main.mr") () with
      | Ok _ -> false
      | Error diags ->
          List.exists
            (fun (diag : Diagnostic.t) ->
              diag.code = "module-qualified-name"
              && String.equal diag.message "Module 'sum' does not export 'reduce'")
            diags)

let%test "check_entry distinguishes missing namespace members from private ones" =
  Discovery.with_temp_project
    [ ("main.mr", "import sum\nsum.reduce\n"); ("sum.mr", "let value = 1\n") ]
    (fun root ->
      match check_entry ~entry_file:(Filename.concat root "main.mr") () with
      | Ok _ -> false
      | Error diags ->
          List.exists
            (fun (diag : Diagnostic.t) ->
              diag.code = "module-qualified-name"
              && String.equal diag.message "Module 'sum' has no member 'reduce'")
            diags)

let%test "check_entry_with_source resolves unsaved entry imports" =
  Discovery.with_temp_project
    [
      ("main.mr", "import sum\nputs(0)\n");
      ("sum.mr", "export sum\nfn sum(values: List[Int]) -> Int = len(values)\n");
    ]
    (fun root ->
      match
        check_entry_with_source ~entry_file:(Filename.concat root "main.mr")
          ~entry_source:"import sum\nputs(sum.sum([1, 2, 3]))\n" ()
      with
      | Error _ -> false
      | Ok diagnostics -> diagnostics = [])

let%test "compile_project rejects duplicate impls across transitive imports" =
  Discovery.with_temp_project
    [
      ("main.mr", "import x\nimport y\nputs(1)\n");
      ( "point.mr",
        "export Point, Printer\ntype Point = { x: Int, y: Int }\ntrait Printer[a] = { fn print(x: a) -> Str }\n"
      );
      ( "a_impl.mr",
        "import point.Point\nimport point.Printer\nimpl Printer[Point] = { fn print(p: Point) -> Str = \"a\" }\n"
      );
      ( "b_impl.mr",
        "import point.Point\nimport point.Printer\nimpl Printer[Point] = { fn print(p: Point) -> Str = \"b\" }\n"
      );
      ("x.mr", "import a_impl\nexport x\nlet x = 1\n");
      ("y.mr", "import b_impl\nexport y\nlet y = 2\n");
    ]
    (fun root ->
      match Discovery.discover_project ~entry_file:(Filename.concat root "main.mr") () with
      | Error _ -> false
      | Ok graph -> (
          match compile_project graph with
          | Ok _ -> false
          | Error diags ->
              List.exists
                (fun (diag : Diagnostic.t) ->
                  diag.code = "module-trait-impl-register"
                  && Diagnostics.String_utils.contains_substring ~needle:"Duplicate impl registration for trait"
                       diag.message)
                diags))

let%test "analyze_entry_with_source returns typed standalone file state" =
  Discovery.with_temp_project
    [ ("main.mr", "let id = (x) -> x\nid(1)\n") ]
    (fun root ->
      let analysis =
        analyze_entry_with_source ~entry_file:(Filename.concat root "main.mr")
          ~entry_source:"let id = (x) -> x\nid(1)\n" ()
      in
      analysis.mode = Standalone
      && analysis.graph = None
      && analysis.project = None
      && analysis.active_file.file_path = Filename.concat root "main.mr"
      && analysis.active_file.surface_program <> None
      && analysis.active_file.typed_program <> None
      && analysis.active_file.type_map <> None
      && analysis.active_file.environment <> None
      && analysis.active_file.diagnostics = [])

let%test "analyze_entry_with_source keeps surface entry AST alongside typed module project" =
  Discovery.with_temp_project
    [
      ("main.mr", "import sum\nputs(sum.sum([1, 2, 3]))\n");
      ("sum.mr", "export sum\nfn sum(values: List[Int]) -> Int = len(values)\n");
    ]
    (fun root ->
      let analysis =
        analyze_entry_with_source ~entry_file:(Filename.concat root "main.mr")
          ~entry_source:"import sum\nputs(sum.sum([1, 2, 3]))\n" ()
      in
      let surface_has_namespace =
        match analysis.active_file.surface_program with
        | None -> false
        | Some program ->
            List.exists
              (fun (stmt : AST.statement) ->
                match stmt.stmt with
                | AST.ExpressionStmt
                    {
                      expr =
                        AST.Call
                          ( { expr = AST.Identifier "puts"; _ },
                            [
                              {
                                expr =
                                  AST.MethodCall
                                    {
                                      mc_receiver = { expr = AST.Identifier "sum"; _ };
                                      mc_method = "sum";
                                      mc_args = _;
                                      _;
                                    };
                                _;
                              };
                            ] );
                      _;
                    } ->
                    true
                | _ -> false)
              program
      in
      let typed_has_internal_name =
        match analysis.active_file.typed_program with
        | None -> false
        | Some program ->
            List.exists
              (fun (stmt : AST.statement) ->
                match stmt.stmt with
                | AST.ExpressionStmt
                    {
                      expr =
                        AST.Call
                          ( { expr = AST.Identifier "puts"; _ },
                            [ { expr = AST.Call ({ expr = AST.Identifier "sum__sum"; _ }, _); _ } ] );
                      _;
                    } ->
                    true
                | _ -> false)
              program
      in
      analysis.mode = Modules
      && analysis.graph <> None
      && analysis.project <> None
      && analysis.active_file.type_map <> None
      && analysis.active_file.environment <> None
      && surface_has_namespace
      && typed_has_internal_name)

let%test "analyze_entry_with_source preserves imported definition provenance for symbol lookup" =
  Discovery.with_temp_project
    [
      ("main.mr", "import math.add\nadd(1, 2)\n");
      ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
    ]
    (fun root ->
      let analysis =
        analyze_entry_with_source ~entry_file:(Filename.concat root "main.mr")
          ~entry_source:"import math.add\nadd(1, 2)\n" ()
      in
      let add_ref_id =
        match analysis.active_file.surface_program with
        | Some
            [
              _;
              { stmt = AST.ExpressionStmt { expr = AST.Call ({ expr = AST.Identifier "add"; id; _ }, _); _ }; _ };
            ] ->
            Some id
        | _ -> None
      in
      let exported_value_definition =
        match find_checked_module_by_file analysis ~file_path:(Filename.concat root "math.mr") with
        | None -> None
        | Some checked_math -> (
            match Module_sig.find_export checked_math.signature "add" with
            | None -> None
            | Some binding -> binding.value_definition)
      in
      match (add_ref_id, exported_value_definition) with
      | Some expr_id, Some definition -> (
          match find_active_file_symbol analysis ~expr_id with
          | None -> false
          | Some symbol ->
              analysis.mode = Modules
              && symbol.file_id = Some (Filename.concat root "math.mr")
              && symbol.definition_pos = definition.start_pos
              && symbol.definition_end_pos = definition.end_pos)
      | _ -> false)

let%test "analyze_entry_with_source respects explicit source_root" =
  Discovery.with_temp_project
    [
      ("src/main.mr", "import lib.answer\nputs(answer())\n");
      ("lib.mr", "export answer\nfn answer() -> Int = 42\n");
    ]
    (fun root ->
      let entry_file = Filename.concat root "src/main.mr" in
      let analysis =
        analyze_entry_with_source ~source_root:root ~entry_file
          ~entry_source:"import lib.answer\nputs(answer())\n" ()
      in
      match analysis.graph with
      | None -> false
      | Some graph ->
          analysis.mode = Modules
          && analysis.source_root = Some root
          && analysis.project_root = Some root
          && graph.entry_module = "src.main"
          && analysis.active_file.diagnostics = [])

let%test "analyze_entry_with_overrides sees imported file overrides" =
  Discovery.with_temp_project
    [ ("main.mr", "import math.answer\nanswer() + 1\n"); ("math.mr", "export answer\nfn answer() -> Int = 41\n") ]
    (fun root ->
      let overrides = Hashtbl.create 1 in
      let math_path = Discovery.normalize_path (Filename.concat root "math.mr") in
      Hashtbl.replace overrides math_path "export answer\nfn answer() -> Str = \"forty one\"\n";
      let analysis =
        analyze_entry_with_overrides ~entry_file:(Filename.concat root "main.mr")
          ~entry_source:"import math.answer\nanswer() + 1\n" ~source_overrides:overrides ()
      in
      List.exists
        (fun (diag : Diagnostic.t) ->
          diag.code = "type-mismatch" || Diagnostics.String_utils.contains_substring ~needle:"Str" diag.message)
        analysis.active_file.diagnostics)

let%test "analyze_entry_with_source can force headerless files into module mode" =
  Discovery.with_temp_project
    [ ("pkg/util.mr", "let helper = 1\n") ]
    (fun root ->
      let entry_file = Filename.concat root "pkg/util.mr" in
      let analysis =
        analyze_entry_with_source ~source_root:root ~force_modules:true ~entry_file
          ~entry_source:"let helper = 1\n" ()
      in
      match analysis.graph with
      | None -> false
      | Some graph ->
          analysis.mode = Modules
          && analysis.project_root = Some root
          && analysis.active_file.module_id = Some "pkg.util"
          && graph.entry_module = "pkg.util")

let%test "checked_module navigation keeps resolved imports for namespace and alias lookups" =
  Discovery.with_temp_project
    [
      ("main.mr", "import math\nimport math.add as plus\nputs(math.add(1, 2) + plus(3, 4))\n");
      ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
    ]
    (fun root ->
      let analysis =
        analyze_entry_with_source ~entry_file:(Filename.concat root "main.mr")
          ~entry_source:"import math\nimport math.add as plus\nputs(math.add(1, 2) + plus(3, 4))\n" ()
      in
      match find_checked_module_by_file analysis ~file_path:(Filename.concat root "main.mr") with
      | None -> false
      | Some checked_main -> (
          Import_resolver.StringMap.mem "plus" checked_main.navigation.resolved_imports.direct_bindings
          &&
          match
            Import_resolver.resolve_namespace_member
              ~namespace_roots:checked_main.navigation.resolved_imports.namespace_roots [ "math"; "add" ]
          with
          | Some (`Exported exported) -> String.equal exported.internal_name "math__add"
          | _ -> false))

let%test "find_export_binding exposes exported definition metadata through compiler boundary" =
  Discovery.with_temp_project
    [
      ("main.mr", "import math\nputs(math.make(1))\n");
      ("math.mr", "export make, Point\ntype Point = { value: Int }\nfn make(x: Int) -> Point = { value: x }\n");
    ]
    (fun root ->
      let analysis =
        analyze_entry_with_source ~entry_file:(Filename.concat root "main.mr")
          ~entry_source:"import math\nputs(math.make(1))\n" ()
      in
      match
        ( find_export_binding analysis ~module_id:"math" ~surface_name:"make",
          find_export_binding analysis ~module_id:"math" ~surface_name:"Point" )
      with
      | Some value_binding, Some type_binding -> (
          let type_definition =
            match
              [
                type_binding.named_type_definition;
                type_binding.transparent_type_definition;
                type_binding.enum_definition;
              ]
              |> List.find_opt Option.is_some
            with
            | Some (Some definition) -> Some definition
            | _ -> None
          in
          match (value_binding.value_definition, type_definition) with
          | Some value_definition, Some type_definition ->
              String.equal value_definition.file_path (Filename.concat root "math.mr")
              && String.equal type_definition.file_path (Filename.concat root "math.mr")
          | _ -> false)
      | _ -> false)
