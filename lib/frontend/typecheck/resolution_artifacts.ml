(* Resolution artifacts: shared key types for checker/emitter/LSP plumbing.
   User-defined callables carry parser-allocated ids from Phase 2.
   Builtins and derive-generated methods use SyntheticCallable. *)

type expr_key = {
  file_id : string option;
  expr_id : int;
}
[@@deriving show, eq]

type callable_key =
  | UserCallable of {
      file_id : string option;
      callable_id : int;
    }
  | SyntheticCallable of string
[@@deriving show, eq]

type trait_object_coercion = {
  target_traits : string list;
  source_type : Types.mono_type;
}

type call_resolution =
  | TraitMethod of string
  | DynamicTraitMethod of string
  | InherentMethod
  | QualifiedTraitMethod of string (* Trait.method(receiver, args...) *)
  | QualifiedInherentMethod (* Type.method(receiver, args...) *)
  | FieldFunctionCall
  | ShimQualifiedCall of string

type shim_func = {
  shim_key : string;
  shim_id : string;
  owner_module_id : string;
  source_qualifier : string;
  marmoset_func_name : string;
  go_symbol_name : string;
  param_names : string list;
  param_boundary_types : Shim_boundary.boundary_type list;
  return_boundary_type : Shim_boundary.boundary_type;
  is_effectful : bool;
  source_span : Diagnostics.Diagnostic.span;
  boundary_spans : Diagnostics.Diagnostic.span list;
}

type shim_call = {
  call_func_key : string;
  call_arg_boundary_types : Shim_boundary.boundary_type list;
  call_return_boundary_type : Shim_boundary.boundary_type;
  call_effectful : bool;
}

type extern_func = shim_func
type extern_call = shim_call

(* Phase 5.4: Typed method-definition artifact.
   Records inferred signatures so the emitter can use them as source-of-truth
   without re-reading trait/inherent registries. Populated during Phase 6. *)
type typed_method_def = {
  md_param_names : string list;
  md_param_types : Types.mono_type list;
  md_return_type : Types.mono_type;
  md_is_effectful : bool;
  md_body_id : int;
}
