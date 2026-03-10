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
