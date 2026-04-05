module AST = Syntax.Ast.AST
module Surface = Syntax.Surface_ast.Surface
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

type project_resolution_artifacts = {
  type_map : Infer.type_map;
  call_resolution_map : (int, Infer.method_resolution) Hashtbl.t;
  method_type_args_map : (int, Types.mono_type list) Hashtbl.t;
  method_def_map : (int, Typecheck.Resolution_artifacts.typed_method_def) Hashtbl.t;
  trait_object_coercion_map : (int, Typecheck.Resolution_artifacts.trait_object_coercion) Hashtbl.t;
  placeholder_rewrite_map : Infer.placeholder_rewrite_map;
}

type compiled_project = {
  modules : checked_module list;
  program : AST.program;
  environment : Infer.type_env;
  artifacts : project_resolution_artifacts;
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
  surface_program : Surface.surface_program option;
  lowered_program : AST.program option;
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

let builtin_env_with_traits () : Infer.type_env =
  Builtins.init_builtin_traits ();
  Builtins.init_builtin_impls ();
  Builtins.builtin_value_env ()

let create_project_resolution_artifacts () : project_resolution_artifacts =
  {
    type_map = Hashtbl.create 512;
    call_resolution_map = Hashtbl.create 256;
    method_type_args_map = Hashtbl.create 128;
    method_def_map = Hashtbl.create 128;
    trait_object_coercion_map = Hashtbl.create 128;
    placeholder_rewrite_map = Hashtbl.create 128;
  }

let merge_project_resolution_artifacts (dst : project_resolution_artifacts) (result : Checker.typecheck_result) :
    unit =
  merge_hashtbl dst.type_map result.type_map;
  merge_hashtbl dst.call_resolution_map result.call_resolution_map;
  merge_hashtbl dst.method_type_args_map result.method_type_args_map;
  merge_hashtbl dst.method_def_map result.method_def_map;
  merge_hashtbl dst.trait_object_coercion_map result.trait_object_coercion_map;
  merge_hashtbl dst.placeholder_rewrite_map result.placeholder_rewrite_map

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

type module_locals_acc = {
  enums : Enum_registry.enum_def list;
  named_types : Type_registry.named_type_def list;
  transparent_types : (string * Annotation.type_alias_info) list;
  shapes : Type_registry.shape_def list;
  traits : Trait_registry.trait_def list;
  trait_impls : Module_sig.trait_impl_entry list;
  type_impls : Module_sig.type_impl_entry list;
}

let extract_module_locals (program : AST.program) : (Module_sig.module_locals, Diagnostic.t) result =
  let empty_locals_acc =
    {
      enums = [];
      named_types = [];
      transparent_types = [];
      shapes = [];
      traits = [];
      trait_impls = [];
      type_impls = [];
    }
  in
  let finish_locals (acc : module_locals_acc) : Module_sig.module_locals =
    {
      Module_sig.enums = List.rev acc.enums;
      named_types = List.rev acc.named_types;
      transparent_types = List.rev acc.transparent_types;
      shapes = List.rev acc.shapes;
      traits = List.rev acc.traits;
      trait_impls = List.rev acc.trait_impls;
      type_impls = List.rev acc.type_impls;
    }
  in
  let target_type_bindings (type_expr : AST.type_expr) =
    Import_resolver.StringSet.elements (collect_inherent_target_generics type_expr)
    |> List.map (fun name -> (name, Types.TVar name))
  in
  let rec go (acc : module_locals_acc) = function
    | [] -> Ok (finish_locals acc)
    | (stmt : AST.statement) :: rest -> (
        match stmt.stmt with
        | AST.EnumDef { name; _ } ->
            let* enum_def =
              required_opt ~code:"module-signature-enum"
                ~message:(Printf.sprintf "Missing enum registry entry for '%s'" name)
                (Enum_registry.lookup name)
            in
            go { acc with enums = enum_def :: acc.enums } rest
        | AST.TypeDef { type_name; _ } ->
            let* named_type_def =
              required_opt ~code:"module-signature-type"
                ~message:(Printf.sprintf "Missing named type registry entry for '%s'" type_name)
                (Type_registry.lookup_named_type type_name)
            in
            go { acc with named_types = named_type_def :: acc.named_types } rest
        | AST.TypeAlias { alias_name; _ } ->
            let* alias_info =
              required_opt ~code:"module-signature-alias"
                ~message:(Printf.sprintf "Missing type alias registry entry for '%s'" alias_name)
                (Annotation.lookup_type_alias alias_name)
            in
            go { acc with transparent_types = (alias_name, alias_info) :: acc.transparent_types } rest
        | AST.ShapeDef { shape_name; _ } ->
            let* shape_def =
              required_opt ~code:"module-signature-shape"
                ~message:(Printf.sprintf "Missing shape registry entry for '%s'" shape_name)
                (Type_registry.lookup_shape shape_name)
            in
            go { acc with shapes = shape_def :: acc.shapes } rest
        | AST.TraitDef { name; _ } ->
            let* trait_def =
              required_opt ~code:"module-signature-trait"
                ~message:(Printf.sprintf "Missing trait registry entry for '%s'" name)
                (Trait_registry.lookup_trait name)
            in
            go { acc with traits = trait_def :: acc.traits } rest
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
            go { acc with trait_impls = { Module_sig.impl_def; origin } :: acc.trait_impls } rest
        | AST.InherentImplDef { inherent_for_type; inherent_methods } ->
            let type_bindings = target_type_bindings inherent_for_type in
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
            let type_impls = merge_type_impl_entry acc.type_impls { Module_sig.for_type; methods } in
            go { acc with type_impls } rest
        | AST.DeriveDef { derive_traits; derive_for_type } ->
            let type_bindings = target_type_bindings derive_for_type in
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
                (Ok []) derive_traits
            in
            go { acc with trait_impls = List.rev_append derived_impls acc.trait_impls } rest
        | AST.ExportDecl _ | AST.ImportDecl _ | AST.Let _ | AST.Return _ | AST.ExpressionStmt _ | AST.Block _ ->
            go acc rest)
  in
  go empty_locals_acc program

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
  let artifacts = create_project_resolution_artifacts () in
  let symbol_table = Hashtbl.create 256 in
  let identifier_symbols = Hashtbl.create 256 in
  let diagnostics = modules |> List.concat_map (fun (m : checked_module) -> m.result.diagnostics) in
  List.iter
    (fun (m : checked_module) ->
      merge_project_resolution_artifacts artifacts m.result;
      List.iter (fun (symbol_id, symbol) -> Hashtbl.replace symbol_table symbol_id symbol) m.result.symbol_table;
      List.iter
        (fun (expr_id, symbol_id) -> Hashtbl.replace identifier_symbols expr_id symbol_id)
        m.result.identifier_symbols)
    modules;
  {
    modules;
    program;
    environment;
    artifacts;
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
    (match
       List.find_opt (fun (module_ : checked_module) -> is_prelude_module module_.module_id) project.modules
     with
    | Some prelude_module ->
        let non_prelude_modules =
          List.filter
            (fun (module_ : checked_module) -> not (is_prelude_module module_.module_id))
            project.modules
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
      Codegen.emit_program_with_typed_env ~call_resolution_map:project.artifacts.call_resolution_map
        ~method_type_args_map:project.artifacts.method_type_args_map
        ~method_def_map:project.artifacts.method_def_map
        ~trait_object_coercion_map:project.artifacts.trait_object_coercion_map
        ~placeholder_rewrite_map:project.artifacts.placeholder_rewrite_map project.artifacts.type_map
        project.environment project.program
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
    ?lowered_program
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
    lowered_program;
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

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b

let definition_site_of_name_ref (name_ref : Surface.name_ref) : Module_sig.definition_site option =
  Option.map
    (fun file_path -> { Module_sig.file_path; start_pos = name_ref.pos; end_pos = name_ref.end_pos })
    name_ref.file_id

let definition_site_of_presence (presence : Import_resolver.member_presence) : Module_sig.definition_site option =
  [
    presence.value_definition;
    presence.enum_definition;
    presence.named_type_definition;
    presence.transparent_type_definition;
    presence.shape_definition;
    presence.trait_definition;
  ]
  |> List.find_opt Option.is_some
  |> Option.join

let navigation_from_surface_graph (analysis : entry_analysis) ~(file_path : string) : module_navigation option =
  match analysis.graph with
  | None -> None
  | Some graph -> (
      match find_parsed_module_by_file analysis ~file_path with
      | None -> None
      | Some parsed_module -> (
          match Import_resolver.build_module_surfaces graph with
          | Error _ -> None
          | Ok surfaces -> (
              match Hashtbl.find_opt surfaces parsed_module.module_id with
              | None -> None
              | Some surface -> (
                  match Import_resolver.build_resolved_imports ~surfaces parsed_module with
                  | Ok resolved_imports -> Some { surface; resolved_imports }
                  | Error _ -> None))))

let current_navigation (analysis : entry_analysis) ~(file_path : string) : module_navigation option =
  first_some
    (Option.map (fun (module_ : checked_module) -> module_.navigation) (find_checked_module_by_file analysis ~file_path))
    (navigation_from_surface_graph analysis ~file_path)

let find_parsed_module_by_id (analysis : entry_analysis) ~(module_id : string) : Module_context.parsed_module option =
  match analysis.graph with
  | None -> None
  | Some graph -> Hashtbl.find_opt graph.modules module_id

let find_visible_presence (analysis : entry_analysis) ~(file_path : string) ~(surface_name : string) :
    Import_resolver.member_presence option =
  match current_navigation analysis ~file_path with
  | None -> None
  | Some navigation ->
      first_some
        (Import_resolver.StringMap.find_opt surface_name navigation.surface.declarations)
        (Import_resolver.StringMap.find_opt surface_name navigation.resolved_imports.direct_bindings)

let presence_has_type_namespace (presence : Import_resolver.member_presence) : bool =
  presence.has_enum || presence.has_named_type || presence.has_transparent_type || presence.has_shape || presence.has_trait

let presence_has_constraint_namespace (presence : Import_resolver.member_presence) : bool =
  presence.has_trait || presence.has_shape

let parsed_module_of_presence (analysis : entry_analysis) (presence : Import_resolver.member_presence) :
    Module_context.parsed_module option =
  Option.bind (definition_site_of_presence presence) (fun site ->
      first_some
        (find_parsed_module_by_file analysis ~file_path:site.file_path)
        (Option.bind
           (find_checked_module_by_file analysis ~file_path:site.file_path)
           (fun checked_module -> find_parsed_module_by_id analysis ~module_id:checked_module.module_id)))

let find_type_head_site_in_surface_program
    (program : Surface.surface_program)
    ~(surface_name : string) : Module_sig.definition_site option =
  List.find_map
    (fun (stmt : Surface.surface_top_stmt) ->
      match stmt.std_decl with
      | Surface.STypeDef { type_name; type_name_ref; _ } when String.equal type_name surface_name ->
          definition_site_of_name_ref type_name_ref
      | Surface.SShapeDef { shape_name; shape_name_ref; _ } when String.equal shape_name surface_name ->
          definition_site_of_name_ref shape_name_ref
      | Surface.STraitDef { name; name_ref; _ } when String.equal name surface_name -> definition_site_of_name_ref name_ref
      | _ -> None)
    program

let find_variant_site_in_surface_program
    (program : Surface.surface_program)
    ~(type_name : string)
    ~(variant_name : string) : Module_sig.definition_site option =
  List.find_map
    (fun (stmt : Surface.surface_top_stmt) ->
      match stmt.std_decl with
      | Surface.STypeDef { type_name = candidate_type; type_body = Surface.STNamedSum variants; _ }
        when String.equal candidate_type type_name ->
          List.find_map
            (fun (variant : Surface.surface_variant_def) ->
              if String.equal variant.sv_name variant_name then
                definition_site_of_name_ref variant.sv_name_ref
              else
                None)
            variants
      | _ -> None)
    program

let find_text_site_in_range
    ~(source : string)
    ~(file_path : string)
    ~(needle : string)
    ~(start_pos : int)
    ~(end_pos : int) : Module_sig.definition_site option =
  let source_len = String.length source in
  let needle_len = String.length needle in
  let last_start = min (source_len - needle_len) end_pos in
  let rec search pos =
    if needle_len <= 0 || pos > last_start then
      None
    else if pos < 0 then
      search 0
    else if String.sub source pos needle_len = needle then
      Some { Module_sig.file_path; start_pos = pos; end_pos = pos + needle_len - 1 }
    else
      search (pos + 1)
  in
  if needle_len <= 0 || source_len < needle_len || start_pos > end_pos then
    None
  else
    search start_pos

let find_visible_type_declaration_site
    (analysis : entry_analysis)
    ~(file_path : string)
    ~(surface_name : string) : Module_sig.definition_site option =
  Option.bind
    (find_visible_presence analysis ~file_path ~surface_name)
    (fun presence ->
      if not (presence_has_type_namespace presence) then
        None
      else
        let fallback = definition_site_of_presence presence in
        let parsed_module =
          first_some (find_parsed_module_by_file analysis ~file_path) (parsed_module_of_presence analysis presence)
        in
        match parsed_module with
        | None -> fallback
        | Some parsed_module ->
            first_some
              (find_type_head_site_in_surface_program parsed_module.surface_program ~surface_name)
              fallback)

let find_visible_constraint_declaration_site
    (analysis : entry_analysis)
    ~(file_path : string)
    ~(surface_name : string) : Module_sig.definition_site option =
  Option.bind
    (find_visible_presence analysis ~file_path ~surface_name)
    (fun presence ->
      if not (presence_has_constraint_namespace presence) then
        None
      else
        let fallback = definition_site_of_presence presence in
        let parsed_module =
          first_some (find_parsed_module_by_file analysis ~file_path) (parsed_module_of_presence analysis presence)
        in
        match parsed_module with
        | None -> fallback
        | Some parsed_module ->
            first_some
              (find_type_head_site_in_surface_program parsed_module.surface_program ~surface_name)
              fallback)

let find_visible_variant_declaration_site
    (analysis : entry_analysis)
    ~(file_path : string)
    ~(type_name : string)
    ~(variant_name : string) : Module_sig.definition_site option =
  Option.bind
    (find_visible_presence analysis ~file_path ~surface_name:type_name)
    (fun presence ->
      if not presence.has_enum then
        None
      else
        let fallback = definition_site_of_presence presence in
        let parsed_module =
          first_some (find_parsed_module_by_file analysis ~file_path) (parsed_module_of_presence analysis presence)
        in
        match parsed_module with
        | None -> fallback
        | Some parsed_module ->
            let exact_site =
              find_variant_site_in_surface_program parsed_module.surface_program ~type_name ~variant_name
            in
            first_some exact_site
              (first_some
                 (Option.bind fallback (fun site ->
                      find_text_site_in_range ~source:parsed_module.source ~file_path:site.file_path ~needle:variant_name
                        ~start_pos:site.start_pos ~end_pos:site.end_pos))
                 (first_some
                    (find_text_site_in_range ~source:parsed_module.source ~file_path:parsed_module.file_path
                       ~needle:variant_name ~start_pos:0 ~end_pos:(String.length parsed_module.source - 1))
                    fallback)))

let find_method_site_in_surface_program
    (program : Surface.surface_program)
    ~(callable_id : int) : Module_sig.definition_site option =
  List.find_map
    (fun (stmt : Surface.surface_top_stmt) ->
      match stmt.std_decl with
      | Surface.STraitDef { methods; _ } ->
          List.find_map
            (fun (method_ : Surface.surface_method_sig) ->
              if method_.sm_id = callable_id then
                definition_site_of_name_ref method_.sm_name_ref
              else
                None)
            methods
      | Surface.SAmbiguousImplDef { impl_methods; _ } | Surface.SInherentImplDef { inherent_methods = impl_methods; _ }
        ->
          List.find_map
            (fun (method_ : Surface.surface_method_impl) ->
              if method_.smi_id = callable_id then
                definition_site_of_name_ref method_.smi_name_ref
              else
                None)
            impl_methods
      | _ -> None)
    program

let find_callable_definition_site
    (analysis : entry_analysis)
    ~(callable_key : Typecheck.Resolution_artifacts.callable_key) : Module_sig.definition_site option =
  match callable_key with
  | Typecheck.Resolution_artifacts.SyntheticCallable _ -> None
  | Typecheck.Resolution_artifacts.UserCallable { callable_id; _ } ->
      first_some
        (Option.bind analysis.active_file.surface_program (fun program ->
             find_method_site_in_surface_program program ~callable_id))
        (match analysis.graph with
        | None -> None
        | Some graph ->
            Hashtbl.to_seq_values graph.modules |> List.of_seq
            |> List.find_map (fun (module_ : Module_context.parsed_module) ->
                   find_method_site_in_surface_program module_.surface_program ~callable_id))

let find_active_file_method_resolution (analysis : entry_analysis) ~(expr_id : int) : Infer.method_resolution option =
  Option.bind analysis.project (fun project -> Hashtbl.find_opt project.artifacts.call_resolution_map expr_id)

let resolve_visible_type_name_to_mono
    (analysis : entry_analysis)
    ~(file_path : string)
    ~(surface_name : string) : Types.mono_type option =
  let fresh_args arity =
    List.init arity (fun idx -> Types.TVar (Printf.sprintf "__lsp_%s_%d" surface_name idx))
  in
  match Annotation.builtin_primitive_type surface_name with
  | Some primitive -> Some primitive
  | None -> (
      match find_visible_presence analysis ~file_path ~surface_name with
      | Some presence when presence.has_named_type -> (
          match Type_registry.named_type_arity surface_name with
          | Some arity -> Some (Types.TNamed (surface_name, fresh_args arity))
          | None -> None)
      | Some presence when presence.has_transparent_type -> (
          match Annotation.lookup_type_alias surface_name with
          | Some alias_info when alias_info.alias_type_params = [] -> (
              match Annotation.type_expr_to_mono_type alias_info.alias_body with
              | Ok mono -> Some mono
              | Error _ -> None)
          | Some _ | None -> None)
      | Some presence when presence.has_enum -> (
          match Annotation.lookup_enum_by_source_name surface_name with
          | Some enum_def -> Some (Types.TEnum (enum_def.name, fresh_args (List.length enum_def.type_params)))
          | None -> None)
      | Some _ | None -> None)

let find_trait_method_declaration_site
    (analysis : entry_analysis)
    ~(trait_name : string)
    ~(method_name : string) : Module_sig.definition_site option =
  match Trait_registry.lookup_trait_method_with_supertraits trait_name method_name with
  | Some (_source_trait, method_sig) -> find_callable_definition_site analysis ~callable_key:method_sig.method_key
  | None -> None

let find_inherent_method_declaration_site
    (analysis : entry_analysis)
    ~(receiver_type : Types.mono_type)
    ~(method_name : string) : Module_sig.definition_site option =
  match Inherent_registry.resolve_method receiver_type method_name with
  | Ok (Some method_sig) -> find_callable_definition_site analysis ~callable_key:method_sig.method_key
  | Ok None | Error _ -> None

let analyze_module_graph
    ~(source_root : string option)
    ~(project_root : string)
    ~(entry_file : string)
    ~(source : string)
    ~(entry_surface_program : Surface.surface_program)
    ~(entry_program : AST.program)
    (graph : Module_context.module_graph) : entry_analysis =
  match Hashtbl.find_opt graph.modules graph.entry_module with
  | None ->
      let active_file =
        make_file_analysis ~file_path:entry_file ~module_id:graph.entry_module ~source
          ~surface_program:entry_surface_program ~lowered_program:entry_program
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
              ~source:entry_module.source ~surface_program:entry_module.surface_program
              ~lowered_program:entry_module.program ~diagnostics:diags ()
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
                  ~source:entry_module.source ~surface_program:entry_module.surface_program
                  ~lowered_program:entry_module.program
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
                  ~source:entry_module.source ~surface_program:entry_module.surface_program
                  ~lowered_program:entry_module.program
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

let rec analyze_entry_with_source ?source_root ?stdlib_root ~(entry_file : string) ~(entry_source : string) () :
    entry_analysis =
  analyze_entry_with_overrides ?source_root ?stdlib_root ~entry_file ~entry_source
    ~source_overrides:(Hashtbl.create 0) ()

and analyze_entry_with_overrides
    ?source_root
    ?stdlib_root
    ~(entry_file : string)
    ~(entry_source : string)
    ~(source_overrides : (string, string) Hashtbl.t)
    () : entry_analysis =
  match Parser.parse_with_surface ~file_id:entry_file entry_source with
  | Error diags ->
      let active_file = make_file_analysis ~file_path:entry_file ~source:entry_source ~diagnostics:diags () in
      { mode = Standalone; source_root; project_root = None; graph = None; project = None; active_file }
  | Ok entry_parse_result -> (
      let project_root = resolved_project_root ?source_root ~entry_file () in
      let all_overrides = Hashtbl.copy source_overrides in
      Hashtbl.replace all_overrides (Discovery.normalize_path entry_file) entry_source;
      match
        Discovery.discover_project_with_overrides ?source_root ?stdlib_root ~entry_file
          ~source_overrides:all_overrides ()
      with
      | Error diag ->
          let active_file =
            make_file_analysis ~file_path:entry_file ~source:entry_source
              ~surface_program:entry_parse_result.surface_program ~lowered_program:entry_parse_result.program
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
            ~entry_surface_program:entry_parse_result.surface_program ~entry_program:entry_parse_result.program
            graph)

let analyze_entry ?source_root ?stdlib_root ~(entry_file : string) () : entry_analysis =
  analyze_entry_with_source ?source_root ?stdlib_root ~entry_file ~entry_source:(read_source_file entry_file) ()

let check_graph (graph : Module_context.module_graph) : (Diagnostic.t list, Diagnostic.t list) result =
  match compile_project graph with
  | Ok project -> Ok project.diagnostics
  | Error diags -> Error diags

let check_entry ?source_root ?stdlib_root ~(entry_file : string) :
    unit -> (Diagnostic.t list, Diagnostic.t list) result =
 fun () ->
  let analysis = analyze_entry ?source_root ?stdlib_root ~entry_file () in
  if diagnostics_have_errors analysis.active_file.diagnostics then
    Error analysis.active_file.diagnostics
  else
    Ok analysis.active_file.diagnostics

let check_entry_with_source ?source_root ?stdlib_root ~(entry_file : string) ~(entry_source : string) :
    unit -> (Diagnostic.t list, Diagnostic.t list) result =
 fun () ->
  let analysis = analyze_entry_with_source ?source_root ?stdlib_root ~entry_file ~entry_source () in
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
        "import geometry\nimpl geometry.Drawable[geometry.Point] = { fn draw(p: geometry.Point) -> Str = \"local\" }\nputs(0)\n"
      );
      ( "geometry.mr",
        "export Point, Drawable\ntype Point = { x: Int, y: Int }\ntrait Drawable[a] = { fn draw(x: a) -> Str }\nimpl Drawable[Point] = { fn draw(p: Point) -> Str = \"base\" }\n"
      );
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
      && analysis.active_file.lowered_program <> None
      && analysis.active_file.typed_program <> None
      && analysis.active_file.type_map <> None
      && analysis.active_file.environment <> None
      && analysis.active_file.diagnostics = [])

let%test "analyze_entry_with_source auto-loads std.prelude for headerless entries" =
  Discovery.with_temp_project
    [
      ( "main.mr",
        "let opt: Option[Int] = Option.Some(42)\nlet status: Result[Str, Int] = Result.Success(\"ok\")\nputs(Option.unwrap_or(opt, 0))\nputs(Result.value_or(Result.map(status, (msg: Str) -> msg + \"!\"), \"bad\"))\nputs(10 % 3)\n"
      );
    ]
    (fun root ->
      let entry_file = Filename.concat root "main.mr" in
      let analysis =
        analyze_entry_with_source ~entry_file
          ~entry_source:
            "let opt: Option[Int] = Option.Some(42)\nlet status: Result[Str, Int] = Result.Success(\"ok\")\nputs(Option.unwrap_or(opt, 0))\nputs(Result.value_or(Result.map(status, (msg: Str) -> msg + \"!\"), \"bad\"))\nputs(10 % 3)\n"
          ()
      in
      match (analysis.graph, analysis.project) with
      | Some graph, Some project ->
          analysis.mode = Modules
          && Hashtbl.mem graph.modules "std.prelude"
          && Hashtbl.mem graph.modules "std.option"
          && Hashtbl.mem graph.modules "std.result"
          && List.exists
               (fun (module_ : checked_module) -> String.equal module_.module_id "std.prelude")
               project.modules
          && List.exists
               (fun (module_ : checked_module) -> String.equal module_.module_id "std.option")
               project.modules
          && List.exists
               (fun (module_ : checked_module) -> String.equal module_.module_id "std.result")
               project.modules
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
        "let opt: Option[Int] = Option.Some(42)\nlet status: Result[Str, Int] = Result.Success(\"ok\")\nlet rendered = Result.map(status, (msg: Str) -> msg + \"!\")\nlet remainder = 10 % 3\nmatch opt {\n\  case Option.Some(value): puts(value + remainder)\n\  case Option.None: puts(0)\n}\nputs(Result.value_or(rendered, \"bad\"))\n"
      );
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
          | Error [ { Diagnostic.code = "stdlib-not-found"; message; _ } ] ->
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
          Discovery.write_file
            (Filename.concat stdlib_root "std/prelude.mr")
            "export Ordering, Eq, Show, Debug, Ord, Hash, Num, Rem, Neg\ntype Ordering = { Less, Equal, Greater }\ntrait Eq[a] = { fn eq(x: a, y: a) -> Bool }\ntrait Show[a] = { fn show(x: a) -> Str }\ntrait Debug[a] = { fn debug(x: a) -> Str }\ntrait Ord[a]: Eq = { fn compare(x: a, y: a) -> Ordering }\ntrait Hash[a] = { fn hash(x: a) -> Int }\ntrait Num[a] = {\n\  fn add(x: a, y: a) -> a\n\  fn sub(x: a, y: a) -> a\n\  fn mul(x: a, y: a) -> a\n\  fn div(x: a, y: a) -> a\n}\ntrait Rem[a] = { fn rem(x: a, y: a) -> a }\ntrait Neg[a] = { fn neg(x: a) -> a }\n";
          Discovery.write_file
            (Filename.concat stdlib_root "std/option.mr")
            "export Option\ntype Option[a] = { Some(a), None }\nimpl[a] Option[a] = {\n\  fn unwrap_or(self: Option[a], fallback: a) -> a = match self {\n\    case Option.Some(v): v\n\    case Option.None: fallback\n\  }\n}\n";
          Discovery.write_file
            (Filename.concat stdlib_root "std/result.mr")
            "export Result\ntype Result[a, e] = { Success(a), Failure(e) }\nimpl[a, e] Result[a, e] = {\n\  fn value_or(self: Result[a, e], fallback: a) -> a = match self {\n\    case Result.Success(v): v\n\    case Result.Failure(_): fallback\n\  }\n}\n";
          Discovery.write_file
            (Filename.concat stdlib_root "std/foo.mr")
            "export value\nfn value() -> Int = Option.unwrap_or(Option.Some(1), 0)\n";
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
                                          match
                                            Hashtbl.find_opt checked_module.signature.Module_sig.exports "Option"
                                          with
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
          Discovery.write_file
            (Filename.concat stdlib_root "std/prelude.mr")
            "export Ordering, Eq, Show, Debug, Ord, Hash, Num, Rem, Neg\ntype Ordering = { Less, Equal, Greater }\ntrait Eq[a] = { fn eq(x: a, y: a) -> Bool }\ntrait Show[a] = { fn show(x: a) -> Str }\ntrait Debug[a] = { fn debug(x: a) -> Str }\ntrait Ord[a]: Eq = { fn compare(x: a, y: a) -> Ordering }\ntrait Hash[a] = { fn hash(x: a) -> Int }\ntrait Num[a] = {\n\  fn add(x: a, y: a) -> a\n\  fn sub(x: a, y: a) -> a\n\  fn mul(x: a, y: a) -> a\n\  fn div(x: a, y: a) -> a\n}\ntrait Rem[a] = { fn rem(x: a, y: a) -> a }\ntrait Neg[a] = { fn neg(x: a) -> a }\n";
          Discovery.write_file
            (Filename.concat stdlib_root "std/option.mr")
            "export Option\ntype Option[a] = { Some(a), None }\nimpl[a] Option[a] = {\n\  fn unwrap_or(self: Option[a], fallback: a) -> a = match self {\n\    case Option.Some(v): v\n\    case Option.None: fallback\n\  }\n}\n";
          Discovery.write_file
            (Filename.concat stdlib_root "std/result.mr")
            "export Result\ntype Result[a, e] = { Success(a), Failure(e) }\nimpl[a, e] Result[a, e] = {\n\  fn value_or(self: Result[a, e], fallback: a) -> a = match self {\n\    case Result.Success(v): v\n\    case Result.Failure(_): fallback\n\  }\n}\n";
          Discovery.write_file
            (Filename.concat stdlib_root "std/foo.mr")
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
          Discovery.write_file
            (Filename.concat stdlib_root "std/prelude.mr")
            "export Ordering, Eq, Show, Debug, Ord, Hash, Num, Rem, Neg\ntype Ordering = { Less, Equal, Greater }\ntrait Eq[a] = { fn eq(x: a, y: a) -> Bool }\ntrait Show[a] = { fn show(x: a) -> Str }\ntrait Debug[a] = { fn debug(x: a) -> Str }\ntrait Ord[a]: Eq = { fn compare(x: a, y: a) -> Ordering }\ntrait Hash[a] = { fn hash(x: a) -> Int }\ntrait Num[a] = {\n\  fn add(x: a, y: a) -> a\n\  fn sub(x: a, y: a) -> a\n\  fn mul(x: a, y: a) -> a\n\  fn div(x: a, y: a) -> a\n}\ntrait Rem[a] = { fn rem(x: a, y: a) -> a }\ntrait Neg[a] = { fn neg(x: a) -> a }\n";
          Discovery.write_file
            (Filename.concat stdlib_root "std/option.mr")
            "export Option\ntype Option[a] = { Some(a), None }\nimpl[a] Option[a] = {\n\  fn unwrap_or(self: Option[a], fallback: a) -> a = match self {\n\    case Option.Some(v): v\n\    case Option.None: fallback\n\  }\n}\n";
          Discovery.write_file
            (Filename.concat stdlib_root "std/result.mr")
            "export Result\ntype Result[a, e] = { Success(a), Failure(e) }\nimpl[a, e] Result[a, e] = {\n\  fn value_or(self: Result[a, e], fallback: a) -> a = match self {\n\    case Result.Success(v): v\n\    case Result.Failure(_): fallback\n\  }\n}\n";
          Discovery.write_file
            (Filename.concat stdlib_root "std/foo.mr")
            "export value\nfn value() -> Int = Option.unwrap_or(Option.Some(1), 0)\n";
          match check_entry ~stdlib_root ~entry_file:(Filename.concat root "main.mr") () with
          | Error _ -> false
          | Ok diagnostics -> diagnostics = []))

let%test "compile_entry_to_build preserves derived builtin impl helpers across module reseeding" =
  Discovery.with_temp_project
    [
      ( "main.mr",
        "type Point = { x: Int, y: Int } derive Show\nfn render[a: Show](x: a) -> Str = Show.show(x)\nlet p = { x: 1, y: 2 }\nputs(render(p))\n"
      );
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
        "trait Boxed[a]: Show = {\n\  fn box(self: a) -> Dyn[Show] = self\n}\n\ntype Box[t] = { value: t } derive Boxed, Show\n\nlet value = Boxed.box({value: 1})\nputs(Show.show(value))\n"
      );
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
        "enum Result = { Ok(Int), Err(Str) }\nlet r = Result.Ok(42)\nmatch r {\n\  case Result.Ok(v): puts(v)\n\  case Result.Err(s): puts(s)\n}\n"
      );
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
              (fun (stmt : Syntax.Surface_ast.Surface.surface_top_stmt) ->
                match stmt.std_decl with
                | Syntax.Surface_ast.Surface.SExpressionStmt
                    {
                      se_expr =
                        Syntax.Surface_ast.Surface.SECall
                          ( { se_expr = Syntax.Surface_ast.Surface.SEIdentifier { text = "puts"; _ }; _ },
                            [
                              {
                                se_expr =
                                  Syntax.Surface_ast.Surface.SEMethodCall
                                    {
                                      se_receiver =
                                        {
                                          se_expr =
                                            Syntax.Surface_ast.Surface.SEIdentifier { text = "sum"; _ };
                                          _;
                                        };
                                      se_method = "sum";
                                      se_args = _;
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
      let lowered_has_namespace =
        match analysis.active_file.lowered_program with
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
      && lowered_has_namespace
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
              {
                Syntax.Surface_ast.Surface.std_decl =
                  Syntax.Surface_ast.Surface.SExpressionStmt
                    {
                      se_expr =
                        Syntax.Surface_ast.Surface.SECall
                          ( { se_expr = Syntax.Surface_ast.Surface.SEIdentifier { text = "add"; _ }; se_id = id; _ },
                            _ );
                      _;
                    };
                _;
              };
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

let%test "analyze_entry_with_source routes extensionless entries through module analysis" =
  Discovery.with_temp_project
    [ ("pkg/util", "let helper = 1\n") ]
    (fun root ->
      let entry_file = Filename.concat root "pkg/util" in
      let analysis =
        analyze_entry_with_source ~source_root:root ~entry_file ~entry_source:"let helper = 1\n" ()
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
