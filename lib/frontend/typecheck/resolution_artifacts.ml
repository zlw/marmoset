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
