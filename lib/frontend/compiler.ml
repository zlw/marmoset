module AST = Syntax.Ast.AST
module Diagnostic = Diagnostics.Diagnostic
module Derive_expand = Typecheck.Derive_expand
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

let prelude_module_id = "std.prelude"

type checked_module = {
  module_id : string;
  file_path : string;
  program : AST.program;
  result : Checker.typecheck_result;
  type_var_user_names : (string * string) list;
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
  graph : Module_context.module_graph option;
  project : compiled_project option;
  active_file : file_analysis;
}

let ( let* ) = Result.bind
let compiler_error ~(code : string) ~(message : string) : Diagnostic.t = Diagnostic.error_no_span ~code ~message
let merge_hashtbl dst src = Hashtbl.iter (fun key value -> Hashtbl.replace dst key value) src

let diagnostics_have_errors (diagnostics : Diagnostic.t list) : bool =
  List.exists (fun (diag : Diagnostic.t) -> diag.severity = Diagnostic.Error) diagnostics

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
    | (entry : Module_sig.trait_impl_entry) :: rest -> (
        try
          Trait_registry.register_impl ~origin:entry.origin entry.impl_def;
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

let is_prelude_module (module_id : string) : bool = String.equal module_id prelude_module_id

let is_file_backed_entry (entry_file : string) : bool = Filename.check_suffix entry_file ".mr"

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
            let* origin =
              required_opt ~code:"module-signature-trait-impl"
                ~message:
                  (Printf.sprintf "Missing trait impl provenance for '%s' on %s" impl_trait_name
                     (Types.to_string for_type))
                (Trait_registry.lookup_impl_origin impl_trait_name for_type)
            in
            go enums named_types transparent_types shapes traits
              ({ Module_sig.impl_def; origin } :: trait_impls)
              type_impls rest
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
        | AST.DeriveDef { derive_traits; derive_for_type } ->
            let type_bindings =
              Import_resolver.StringSet.elements (collect_inherent_target_generics derive_for_type)
              |> List.map (fun name -> (name, Types.TVar name))
            in
            let* for_type =
              Annotation.type_expr_to_mono_type_with type_bindings derive_for_type
              |> Result.map_error (fun (diag : Diagnostic.t) ->
                     compiler_error ~code:"module-signature-derived-impl" ~message:diag.message)
            in
            let* derived_impls =
              List.fold_left
                (fun acc (derive_trait : AST.derive_trait) ->
                  let* entries = acc in
                  let* impl_def =
                    required_opt ~code:"module-signature-derived-impl"
                      ~message:
                        (Printf.sprintf "Missing derived impl registry entry for '%s' on %s"
                           derive_trait.derive_trait_name (Types.to_string for_type))
                      (find_trait_impl ~trait_name:derive_trait.derive_trait_name ~for_type)
                  in
                  let* origin =
                    required_opt ~code:"module-signature-derived-impl"
                      ~message:
                        (Printf.sprintf "Missing derived impl provenance for '%s' on %s"
                           derive_trait.derive_trait_name (Types.to_string for_type))
                      (Trait_registry.lookup_impl_origin derive_trait.derive_trait_name for_type)
                  in
                  Ok ({ Module_sig.impl_def; origin } :: entries))
                (Ok [])
                derive_traits
            in
            go enums named_types transparent_types shapes traits
              (List.rev_append derived_impls trait_impls)
              type_impls rest
        | AST.ExportDecl _ | AST.ImportDecl _ | AST.Let _ | AST.Return _ | AST.ExpressionStmt _ | AST.Block _ ->
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
             match Import_resolver.StringMap.find_opt presence.internal_name trait_map with
             | Some trait_def -> Some trait_def
             | None -> Trait_registry.lookup_trait presence.internal_name
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
  let* expanded_program =
    Derive_expand.expand_user_derives rewrite.program |> Result.map_error (fun diag -> [ diag ])
  in
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
    seed_imports (Builtins.builtin_value_env ()) [] rewrite.resolved_imports.direct_modules
  in
  if not (is_prelude_module module_info.module_id) then
    Builtins.init_builtin_impls ();
  let* result =
    Checker.check_program_with_annotations ~state ~prebound_symbols ~prepare_state:false ~expand_derives:false
      ~env expanded_program
    |> Result.map_error (fun diags -> diags)
  in
  let type_var_user_names = Infer.type_var_user_name_bindings_in_state state in
  let* locals = extract_module_locals expanded_program |> Result.map_error (fun diag -> [ diag ]) in
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
      program = expanded_program;
      result;
      type_var_user_names;
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
    | (entry : Module_sig.trait_impl_entry) :: rest -> (
        try
          Trait_registry.register_impl ~origin:entry.origin entry.impl_def;
          seed_trait_impls rest
        with Failure message -> Error (compiler_error ~code:"module-trait-impl-register" ~message))
  in
  let* () = seed_trait_impls locals.trait_impls in
  seed_type_impls locals.type_impls

let emit_compiled_project (project : compiled_project) : (Codegen.build_output, Diagnostic.t list) result =
  reset_module_state ();
  let* () =
    let rec seed = function
      | [] -> Ok ()
      | (module_ : checked_module) :: rest ->
          let* () = seed_module_locals module_.locals in
          seed rest
    in
    (match List.find_opt (fun (module_ : checked_module) -> is_prelude_module module_.module_id) project.modules with
    | Some prelude_module ->
        let non_prelude_modules =
          List.filter (fun (module_ : checked_module) -> not (is_prelude_module module_.module_id)) project.modules
        in
        let* () = seed_module_locals prelude_module.locals in
        Builtins.init_builtin_impls ();
        seed non_prelude_modules
    | None ->
        Error
          (compiler_error ~code:"stdlib-not-found"
             ~message:"Missing required toolchain stdlib module 'std.prelude' in compiled project"))
    |> Result.map_error (fun diag -> [ diag ])
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
      { mode = Standalone; source_root; graph = None; project = None; active_file }
  | Ok result ->
      let active_file =
        make_file_analysis ~file_path:entry_file ~source ~surface_program:program ~typed_program:program
          ~type_map:result.type_map ~environment:result.environment ~diagnostics:result.diagnostics
          ~type_var_user_names:(Infer.type_var_user_name_bindings_in_state state)
          ~symbol_table:result.symbol_table ~identifier_symbols:result.identifier_symbols ()
      in
      { mode = Standalone; source_root; graph = None; project = None; active_file }

let analyze_module_graph
    ~(source_root : string option)
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
      { mode = Modules; source_root; graph = Some graph; project = None; active_file }
  | Some entry_module -> (
      match compile_project graph with
      | Error diags ->
          let active_file =
            make_file_analysis ~file_path:entry_module.file_path ~module_id:entry_module.module_id
              ~source:entry_module.source ~surface_program:entry_module.program ~diagnostics:diags ()
          in
          { mode = Modules; source_root; graph = Some graph; project = None; active_file }
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
              { mode = Modules; source_root; graph = Some graph; project = Some project; active_file }
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
              { mode = Modules; source_root; graph = Some graph; project = Some project; active_file }))

let analyze_entry_with_source
    ?source_root ?stdlib_root ?(force_modules = false) ~(entry_file : string) ~(entry_source : string) ()
    : entry_analysis =
  match Parser.parse ~file_id:entry_file entry_source with
  | Error diags ->
      let active_file = make_file_analysis ~file_path:entry_file ~source:entry_source ~diagnostics:diags () in
      { mode = Standalone; source_root; graph = None; project = None; active_file }
  | Ok entry_program -> (
      if force_modules || is_file_backed_entry entry_file || has_module_headers entry_program then
        match Discovery.discover_project_with_entry_source ?source_root ?stdlib_root ~entry_file ~entry_source () with
        | Error diag ->
            let active_file =
              make_file_analysis ~file_path:entry_file ~source:entry_source ~surface_program:entry_program
                ~diagnostics:[ diag ] ()
            in
            { mode = Modules; source_root; graph = None; project = None; active_file }
        | Ok graph -> analyze_module_graph ~source_root ~entry_file ~source:entry_source ~entry_program graph
      else
        analyze_standalone_program ?source_root ~entry_file ~source:entry_source entry_program)

let analyze_entry ?source_root ?stdlib_root ?(force_modules = false) ~(entry_file : string) () : entry_analysis =
  analyze_entry_with_source ?source_root ?stdlib_root ~force_modules ~entry_file
    ~entry_source:(read_source_file entry_file) ()

let check_graph (graph : Module_context.module_graph) : (Diagnostic.t list, Diagnostic.t list) result =
  match compile_project graph with
  | Ok project -> Ok project.diagnostics
  | Error diags -> Error diags

let check_entry ?source_root ?stdlib_root ?(force_modules = false) ~(entry_file : string) :
    unit -> (Diagnostic.t list, Diagnostic.t list) result =
 fun () ->
  let analysis = analyze_entry ?source_root ?stdlib_root ~force_modules ~entry_file () in
  if diagnostics_have_errors analysis.active_file.diagnostics then
    Error analysis.active_file.diagnostics
  else
    Ok analysis.active_file.diagnostics

let check_entry_with_source ?source_root ?stdlib_root ?(force_modules = false) ~(entry_file : string)
    ~(entry_source : string)
    : unit -> (Diagnostic.t list, Diagnostic.t list) result =
 fun () ->
  let analysis = analyze_entry_with_source ?source_root ?stdlib_root ~force_modules ~entry_file ~entry_source () in
  if diagnostics_have_errors analysis.active_file.diagnostics then
    Error analysis.active_file.diagnostics
  else
    Ok analysis.active_file.diagnostics

let compile_entry_to_build ?source_root ?stdlib_root ~(entry_file : string) () :
    (Codegen.build_output, Diagnostic.t list) result =
  let* graph =
    Discovery.discover_project ?source_root ?stdlib_root ~entry_file () |> Result.map_error (fun diag -> [ diag ])
  in
  compile_project_to_build graph

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

let%test "compile_project rejects duplicate impls with qualified namespace headers" =
  Discovery.with_temp_project
    [
      ( "main.mr",
        "import geometry\n\
         impl geometry.Drawable[geometry.Point] = { fn draw(p: geometry.Point) -> Str = \"local\" }\n\
         puts(0)\n" );
      ( "geometry.mr",
        "export Point, Drawable\n\
         type Point = { x: Int, y: Int }\n\
         trait Drawable[a] = { fn draw(x: a) -> Str }\n\
         impl Drawable[Point] = { fn draw(p: Point) -> Str = \"base\" }\n" );
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

let%test "analyze_entry_with_source routes file-backed headerless entries through module analysis" =
  Discovery.with_temp_project
    [ ("main.mr", "let id = (x) -> x\nid(1)\n") ]
    (fun root ->
      let analysis =
        analyze_entry_with_source ~entry_file:(Filename.concat root "main.mr")
          ~entry_source:"let id = (x) -> x\nid(1)\n" ()
      in
      analysis.mode = Modules
      && analysis.graph <> None
      && analysis.project <> None
      && analysis.active_file.file_path = Filename.concat root "main.mr"
      && analysis.active_file.module_id = Some "main"
      && analysis.active_file.surface_program <> None
      && analysis.active_file.typed_program <> None
      && analysis.active_file.type_map <> None
      && analysis.active_file.environment <> None
      && analysis.active_file.diagnostics = [])

let%test "analyze_entry_with_source auto-loads std.prelude for headerless entries" =
  Discovery.with_temp_project
    [
      ( "main.mr",
        "let opt: Option[Int] = Option.Some(42)\n\
         let status: Result[Str, Int] = Result.Success(\"ok\")\n\
         puts(Option.unwrap_or(opt, 0))\n\
         puts(Result.unwrap_or(Result.map(status, (msg: Str) -> msg + \"!\"), \"bad\"))\n\
         puts(10 % 3)\n" );
    ]
    (fun root ->
      let entry_file = Filename.concat root "main.mr" in
      let analysis =
        analyze_entry_with_source ~entry_file
          ~entry_source:
            "let opt: Option[Int] = Option.Some(42)\n\
             let status: Result[Str, Int] = Result.Success(\"ok\")\n\
             puts(Option.unwrap_or(opt, 0))\n\
             puts(Result.unwrap_or(Result.map(status, (msg: Str) -> msg + \"!\"), \"bad\"))\n\
             puts(10 % 3)\n"
          ()
      in
      match (analysis.graph, analysis.project) with
      | Some graph, Some project ->
          analysis.mode = Modules
          && Hashtbl.mem graph.modules "std.prelude"
          && Hashtbl.mem graph.modules "std.option"
          && Hashtbl.mem graph.modules "std.result"
          && List.exists (fun (module_ : checked_module) -> String.equal module_.module_id "std.prelude") project.modules
          && List.exists (fun (module_ : checked_module) -> String.equal module_.module_id "std.option") project.modules
          && List.exists (fun (module_ : checked_module) -> String.equal module_.module_id "std.result") project.modules
          && analysis.active_file.diagnostics = []
      | _ -> false)

let%test "analyze_entry_with_source rewrites record punning to explicit synthetic identifiers in module mode" =
  Discovery.with_temp_project
    [ ("main.mr", "let x = 3\nlet y = 4\nlet r = { x:, y: }\nputs(r.x + r.y)\n") ]
    (fun root ->
      let analysis =
        analyze_entry_with_source ~entry_file:(Filename.concat root "main.mr")
          ~entry_source:"let x = 3\nlet y = 4\nlet r = { x:, y: }\nputs(r.x + r.y)\n" ()
      in
      match analysis.active_file.typed_program with
      | None -> false
      | Some program -> (
          match
            List.find_map
              (fun (stmt : AST.statement) ->
                match stmt.stmt with
                | AST.Let
                    {
                      name = "r";
                      value = { id = record_id; expr = AST.RecordLit (fields, None); _ };
                      type_annotation = _;
                    } ->
                    Some (record_id, fields)
                | _ -> None)
              program
          with
          | None -> false
          | Some (record_id, fields) ->
              let materialized =
                List.filter_map
                  (fun (field : AST.record_field) ->
                    match field.field_value with
                    | Some { id; expr = AST.Identifier name; _ } -> Some (field.field_name, name, id)
                    | _ -> None)
                  fields
              in
              let ids = List.map (fun (_, _, id) -> id) materialized in
              List.length materialized = 2
              && List.sort compare (List.map (fun (field_name, name, _) -> (field_name, name)) materialized)
                 = [ ("x", "x"); ("y", "y") ]
              && List.for_all (fun id -> id < 0 && id <> record_id) ids
              && List.length ids = List.length (List.sort_uniq Int.compare ids)))

let%test "check_entry resolves toolchain stdlib without a project-local std directory" =
  Discovery.with_temp_project
    [
      ( "main.mr",
        "let opt: Option[Int] = Option.Some(42)\n\
         let status: Result[Str, Int] = Result.Success(\"ok\")\n\
         let rendered = Result.map(status, (msg: Str) -> msg + \"!\")\n\
         let remainder = 10 % 3\n\
         match opt {\n\
         \  case Option.Some(value): puts(value + remainder)\n\
         \  case Option.None: puts(0)\n\
         }\n\
         puts(Result.unwrap_or(rendered, \"bad\"))\n" );
    ]
    (fun root ->
      match check_entry ~entry_file:(Filename.concat root "main.mr") () with
      | Error _ -> false
      | Ok diagnostics -> diagnostics = [])

let%test "check_entry errors when the configured toolchain stdlib root is invalid" =
  Discovery.with_temp_project
    [ ("main.mr", "puts(1)\n") ]
    (fun root ->
      let stdlib_root = Discovery.make_temp_dir "marmoset_invalid_stdlib_" in
      Fun.protect
        ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote stdlib_root)))
        (fun () ->
          match check_entry ~stdlib_root ~entry_file:(Filename.concat root "main.mr") () with
          | Ok _ -> false
          | Error
              [
                {
                  Diagnostic.code = "stdlib-not-found";
                  message;
                  _;
                };
              ] ->
              Diagnostics.String_utils.contains_substring ~needle:"std/prelude.mr" message
          | Error _ -> false))

let%test "std.option signature exports Option as an enum for downstream modules" =
  Discovery.with_temp_project
    [ ("main.mr", "import std.foo as foo\nputs(foo.value())\n") ]
    (fun root ->
      let stdlib_root = Discovery.make_temp_dir "marmoset_review_stdlib_" in
      Fun.protect
        ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote stdlib_root)))
        (fun () ->
          Discovery.mkdir_p (Filename.concat stdlib_root "std");
          Discovery.write_file (Filename.concat stdlib_root "std/prelude.mr")
            "export Ordering, Eq, Show, Debug, Ord, Hash, Num, Rem, Neg\n\
             type Ordering = { Less, Equal, Greater }\n\
             trait Eq[a] = { fn eq(x: a, y: a) -> Bool }\n\
             trait Show[a] = { fn show(x: a) -> Str }\n\
             trait Debug[a] = { fn debug(x: a) -> Str }\n\
             trait Ord[a]: Eq = { fn compare(x: a, y: a) -> Ordering }\n\
             trait Hash[a] = { fn hash(x: a) -> Int }\n\
             trait Num[a] = {\n\
             \  fn add(x: a, y: a) -> a\n\
             \  fn sub(x: a, y: a) -> a\n\
             \  fn mul(x: a, y: a) -> a\n\
             \  fn div(x: a, y: a) -> a\n\
             }\n\
             trait Rem[a] = { fn rem(x: a, y: a) -> a }\n\
             trait Neg[a] = { fn neg(x: a) -> a }\n";
          Discovery.write_file (Filename.concat stdlib_root "std/option.mr")
            "export Option\n\
             type Option[a] = { Some(a), None }\n\
             impl[a] Option[a] = {\n\
             \  fn unwrap_or(self: Option[a], fallback: a) -> a = match self {\n\
             \    case Option.Some(v): v\n\
             \    case Option.None: fallback\n\
             \  }\n\
             }\n";
          Discovery.write_file (Filename.concat stdlib_root "std/result.mr")
            "export Result\n\
             type Result[a, e] = { Success(a), Failure(e) }\n\
             impl[a, e] Result[a, e] = {\n\
             \  fn unwrap_or(self: Result[a, e], fallback: a) -> a = match self {\n\
             \    case Result.Success(v): v\n\
             \    case Result.Failure(_): fallback\n\
             \  }\n\
             }\n";
          Discovery.write_file (Filename.concat stdlib_root "std/foo.mr")
            "export value\nfn value() -> Int = Option.Some(1).unwrap_or(0)\n";
          match Discovery.discover_project ~stdlib_root ~entry_file:(Filename.concat root "main.mr") () with
          | Error _ -> false
          | Ok graph -> (
              match Import_resolver.build_module_surfaces graph with
              | Error _ -> false
              | Ok surfaces -> (
                  match rewrite_project_modules ~surfaces graph with
                  | Error _ -> false
                  | Ok rewrites ->
                      let typed_signatures = Hashtbl.create (List.length graph.topo_order) in
                      let rec compile_until_option = function
                        | [] -> false
                        | module_id :: rest -> (
                            match Hashtbl.find_opt graph.modules module_id with
                            | None -> false
                            | Some module_info -> (
                                match Hashtbl.find_opt rewrites module_id with
                                | None -> false
                                | Some rewrite -> (
                                    match compile_module ~surfaces ~typed_signatures ~rewrite module_info with
                                    | Error _ -> false
                                    | Ok checked_module ->
                                        Hashtbl.replace typed_signatures module_id checked_module.signature;
                                        if String.equal module_id "std.option" then
                                          match Hashtbl.find_opt checked_module.signature.Module_sig.exports "Option" with
                                          | Some binding -> Option.is_some binding.Module_sig.enum_def
                                          | None -> false
                                        else
                                          compile_until_option rest)))
                      in
                      compile_until_option graph.topo_order))))

let%test "source-backed module compilation sees std.option and std.result signatures before std.foo" =
  Discovery.with_temp_project
    [ ("main.mr", "import std.foo as foo\nputs(foo.value())\n") ]
    (fun root ->
      let stdlib_root = Discovery.make_temp_dir "marmoset_review_stdlib_" in
      Fun.protect
        ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote stdlib_root)))
        (fun () ->
          Discovery.mkdir_p (Filename.concat stdlib_root "std");
          Discovery.write_file (Filename.concat stdlib_root "std/prelude.mr")
            "export Ordering, Eq, Show, Debug, Ord, Hash, Num, Rem, Neg\n\
             type Ordering = { Less, Equal, Greater }\n\
             trait Eq[a] = { fn eq(x: a, y: a) -> Bool }\n\
             trait Show[a] = { fn show(x: a) -> Str }\n\
             trait Debug[a] = { fn debug(x: a) -> Str }\n\
             trait Ord[a]: Eq = { fn compare(x: a, y: a) -> Ordering }\n\
             trait Hash[a] = { fn hash(x: a) -> Int }\n\
             trait Num[a] = {\n\
             \  fn add(x: a, y: a) -> a\n\
             \  fn sub(x: a, y: a) -> a\n\
             \  fn mul(x: a, y: a) -> a\n\
             \  fn div(x: a, y: a) -> a\n\
             }\n\
             trait Rem[a] = { fn rem(x: a, y: a) -> a }\n\
             trait Neg[a] = { fn neg(x: a) -> a }\n";
          Discovery.write_file (Filename.concat stdlib_root "std/option.mr")
            "export Option\n\
             type Option[a] = { Some(a), None }\n\
             impl[a] Option[a] = {\n\
             \  fn unwrap_or(self: Option[a], fallback: a) -> a = match self {\n\
             \    case Option.Some(v): v\n\
             \    case Option.None: fallback\n\
             \  }\n\
             }\n";
          Discovery.write_file (Filename.concat stdlib_root "std/result.mr")
            "export Result\n\
             type Result[a, e] = { Success(a), Failure(e) }\n\
             impl[a, e] Result[a, e] = {\n\
             \  fn unwrap_or(self: Result[a, e], fallback: a) -> a = match self {\n\
             \    case Result.Success(v): v\n\
             \    case Result.Failure(_): fallback\n\
             \  }\n\
             }\n";
          Discovery.write_file (Filename.concat stdlib_root "std/foo.mr")
            "export value\nfn value() -> Int = Option.unwrap_or(Option.Some(1), 0)\n";
          let entry_file = Filename.concat root "main.mr" in
          match
            Discovery.discover_project_with_entry_source ~stdlib_root ~entry_file
              ~entry_source:"import std.foo as foo\nputs(foo.value())\n" ()
          with
          | Error _ -> false
          | Ok graph -> (
              match Import_resolver.build_module_surfaces graph with
              | Error _ -> false
              | Ok surfaces -> (
                  match rewrite_project_modules ~surfaces graph with
                  | Error _ -> false
                  | Ok rewrites ->
                      let typed_signatures = Hashtbl.create (List.length graph.topo_order) in
                      let rec compile_modules = function
                        | [] -> false
                        | module_id :: rest -> (
                            if String.equal module_id "std.foo" then
                              Hashtbl.mem typed_signatures "std.option"
                              && Hashtbl.mem typed_signatures "std.result"
                              &&
                              match Hashtbl.find_opt rewrites module_id with
                              | None -> false
                              | Some rewrite ->
                                  List.mem "std.option" rewrite.resolved_imports.direct_modules
                                  && List.mem "std.result" rewrite.resolved_imports.direct_modules
                            else
                              match Hashtbl.find_opt graph.modules module_id with
                              | None -> false
                              | Some module_info -> (
                                  match Hashtbl.find_opt rewrites module_id with
                                  | None -> false
                                  | Some rewrite -> (
                                      match compile_module ~surfaces ~typed_signatures ~rewrite module_info with
                                      | Error _ -> false
                                      | Ok checked_module ->
                                          Hashtbl.replace typed_signatures module_id checked_module.signature;
                                          compile_modules rest)))
                      in
                      compile_modules graph.topo_order))))

let%test "non-core stdlib modules implicitly see Option and Result" =
  Discovery.with_temp_project
    [ ("main.mr", "import std.foo as foo\nputs(foo.value())\n") ]
    (fun root ->
      let stdlib_root = Discovery.make_temp_dir "marmoset_review_stdlib_" in
      Fun.protect
        ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote stdlib_root)))
        (fun () ->
          Discovery.mkdir_p (Filename.concat stdlib_root "std");
          Discovery.write_file (Filename.concat stdlib_root "std/prelude.mr")
            "export Ordering, Eq, Show, Debug, Ord, Hash, Num, Rem, Neg\n\
             type Ordering = { Less, Equal, Greater }\n\
             trait Eq[a] = { fn eq(x: a, y: a) -> Bool }\n\
             trait Show[a] = { fn show(x: a) -> Str }\n\
             trait Debug[a] = { fn debug(x: a) -> Str }\n\
             trait Ord[a]: Eq = { fn compare(x: a, y: a) -> Ordering }\n\
             trait Hash[a] = { fn hash(x: a) -> Int }\n\
             trait Num[a] = {\n\
             \  fn add(x: a, y: a) -> a\n\
             \  fn sub(x: a, y: a) -> a\n\
             \  fn mul(x: a, y: a) -> a\n\
             \  fn div(x: a, y: a) -> a\n\
             }\n\
             trait Rem[a] = { fn rem(x: a, y: a) -> a }\n\
             trait Neg[a] = { fn neg(x: a) -> a }\n";
          Discovery.write_file (Filename.concat stdlib_root "std/option.mr")
            "export Option\n\
             type Option[a] = { Some(a), None }\n\
             impl[a] Option[a] = {\n\
             \  fn unwrap_or(self: Option[a], fallback: a) -> a = match self {\n\
             \    case Option.Some(v): v\n\
             \    case Option.None: fallback\n\
             \  }\n\
             }\n";
          Discovery.write_file (Filename.concat stdlib_root "std/result.mr")
            "export Result\n\
             type Result[a, e] = { Success(a), Failure(e) }\n\
             impl[a, e] Result[a, e] = {\n\
             \  fn unwrap_or(self: Result[a, e], fallback: a) -> a = match self {\n\
             \    case Result.Success(v): v\n\
             \    case Result.Failure(_): fallback\n\
             \  }\n\
             }\n";
          Discovery.write_file (Filename.concat stdlib_root "std/foo.mr")
            "export value\nfn value() -> Int = Option.unwrap_or(Option.Some(1), 0)\n";
          match check_entry ~stdlib_root ~entry_file:(Filename.concat root "main.mr") () with
          | Error _ -> false
          | Ok diagnostics -> diagnostics = []))

let%test "compile_entry_to_build preserves derived builtin impl helpers across module reseeding" =
  Discovery.with_temp_project
    [
      ( "main.mr",
        "type Point = { x: Int, y: Int } derive Show\n\
         fn render[a: Show](x: a) -> Str = Show.show(x)\n\
         let p = { x: 1, y: 2 }\n\
         puts(render(p))\n" );
    ]
    (fun root ->
      match compile_entry_to_build ~entry_file:(Filename.concat root "main.mr") () with
      | Error _ -> false
      | Ok build_output ->
          Diagnostics.String_utils.contains_substring ~needle:"func show_show_record_x_int64_y_int64_closed"
            build_output.main_go)

let%test "compile_entry_to_build supports record punning for headerless file-backed entries" =
  Discovery.with_temp_project
    [ ("main.mr", "let x = 3\nlet y = 4\nlet r = { x:, y: }\nputs(r.x + r.y)\n") ]
    (fun root ->
      match compile_entry_to_build ~entry_file:(Filename.concat root "main.mr") () with
      | Error _ -> false
      | Ok _ -> true)

let%test "compile_entry_to_build preserves derived Dyn boxing helpers across module reseeding" =
  Discovery.with_temp_project
    [
      ( "main.mr",
        "trait Boxed[a]: Show = {\n\
         \  fn box(self: a) -> Dyn[Show] = self\n\
         }\n\
         \n\
         type Box[t] = { value: t } derive Boxed, Show\n\
         \n\
         let value = Boxed.box({value: 1})\n\
         puts(Show.show(value))\n" );
    ]
    (fun root ->
      match compile_entry_to_build ~entry_file:(Filename.concat root "main.mr") () with
      | Error _ -> false
      | Ok build_output ->
          Diagnostics.String_utils.contains_substring ~needle:"func show_show_record_value_int64_closed"
            build_output.main_go)

let%test "compile_entry_to_build lets headerless local Result shadow injected std.result" =
  Discovery.with_temp_project
    [
      ( "main.mr",
        "enum Result = { Ok(Int), Err(Str) }\n\
         let r = Result.Ok(42)\n\
         match r {\n\
         \  case Result.Ok(v): puts(v)\n\
         \  case Result.Err(s): puts(s)\n\
         }\n" );
    ]
    (fun root ->
      match compile_entry_to_build ~entry_file:(Filename.concat root "main.mr") () with
      | Error _ -> false
      | Ok _ -> true)

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
          && graph.entry_module = "src.main"
          && analysis.active_file.diagnostics = [])

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
          && analysis.active_file.module_id = Some "pkg.util"
          && graph.entry_module = "pkg.util")

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
