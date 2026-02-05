# Type Map Refactor Plan

## Problem

Currently, codegen (`emitter.ml`) re-runs type inference on expressions via `infer_type` calls (~20 call sites). This is:
1. Inefficient - we already inferred types during type checking
2. Bug-prone - re-inference can produce different type variables, causing issues like the Phase 4.4 heterogeneous enum bug

## Solution

Infer types once during type checking, store them in a map keyed by expression ID, and look them up during codegen.

## Changes Required

### Phase 1: Add unique IDs to AST expressions

**File: `lib/frontend/syntax/ast.ml`**
- Add `id: int` field to `expression` record
- Update `mk_expr` to take an id parameter
- Keep `expr_equal` ignoring id (like it ignores pos)

```ocaml
and expression = {
  id: int;        (* NEW - unique identifier *)
  expr: expr_kind;
  pos: int;
}

let mk_expr ?(id = 0) ?(pos = 0) expr = { id; expr; pos }
```

### Phase 2: Thread counter through parser

**File: `lib/frontend/syntax/parser.ml`**
- Add `next_id: int` field to `parser` record
- Create helper `fresh_id` that returns id and increments counter
- Update all `mk_expr` calls to use fresh IDs
- Thread the counter through all parsing functions

```ocaml
type parser = {
  lexer: Lexer.lexer;
  curr_token: Token.token;
  peek_token: Token.token;
  errors: errors;
  next_id: int;   (* NEW - counter for unique expression IDs *)
}

let fresh_id p = (p.next_id, { p with next_id = p.next_id + 1 })
```

**Estimated changes:** ~50-80 call sites where expressions are created

### Phase 3: Build type map during inference

**File: `lib/frontend/typecheck/infer.ml`**
- Define `type_map` type as `(int, Types.mono_type) Hashtbl.t`
- Thread type map through `infer_expression` and related functions
- After inferring each expression, store `expr.id -> type` in map
- Return type map from `infer_program`

```ocaml
type type_map = (int, Types.mono_type) Hashtbl.t

let infer_program program : (type_env * type_map * mono_type) infer_result =
  let type_map = Hashtbl.create 256 in
  (* ... during inference ... *)
  Hashtbl.add type_map expr.id inferred_type;
  (* ... *)
  Ok (env, type_map, result_type)
```

**Estimated changes:** ~20-30 locations where we return from infer_expression

### Phase 4: Update codegen to use type map

**File: `lib/backend/go/emitter.ml`**
- Remove `infer_type` function
- Add `type_map` parameter to emit functions
- Replace all `infer_type env expr` calls with `Hashtbl.find type_map expr.id`
- Update `compile_string` to pass type map from inference

```ocaml
let get_type type_map expr =
  match Hashtbl.find_opt type_map expr.id with
  | Some t -> t
  | None -> failwith (Printf.sprintf "No type for expression id %d" expr.id)

(* Replace: infer_type env expr *)
(* With:    get_type type_map expr *)
```

**Estimated changes:** ~20 call sites (all current `infer_type` calls)

### Phase 5: Update other callers

**File: `lib/frontend/typecheck/checker.ml`**
- Update `check_program_with_annotations` to return type map
- Pass type map to codegen

**File: `lib/backend/go/emitter.ml`**
- Update `emit_program_with_typed_env` signature to take type map
- Update `compile_string` and `compile_to_build` to thread type map

## Files Modified (Summary)

| File | Changes |
|------|---------|
| `lib/frontend/syntax/ast.ml` | Add `id` field to expression |
| `lib/frontend/syntax/parser.ml` | Thread counter, assign IDs (~50-80 sites) |
| `lib/frontend/typecheck/infer.ml` | Build type map during inference (~20-30 sites) |
| `lib/frontend/typecheck/checker.ml` | Pass type map through |
| `lib/backend/go/emitter.ml` | Use type map instead of re-inferring (~20 sites) |
| `lib/backend/interpreter/eval.ml` | Might need updates if it uses AST |
| `lib/backend/vm/compiler.ml` | Might need updates if it uses AST |

## Testing Strategy

1. After Phase 2: Run parser tests - all should pass (IDs are ignored in equality)
2. After Phase 3: Run type inference tests - all should pass
3. After Phase 4: Run full integration tests - all 47 should pass
4. Verify no more `infer_type` calls in emitter.ml

## Risks

1. **Parser complexity** - Threading counter through ~50-80 sites is tedious but mechanical
2. **Missed call sites** - Could miss some `infer_type` calls; grep will catch them
3. **Interpreter/VM** - May need updates if they construct AST nodes

## Estimated Effort

- Phase 1: 10 minutes
- Phase 2: 30-45 minutes (many mechanical changes)
- Phase 3: 20-30 minutes
- Phase 4: 20 minutes
- Phase 5: 10 minutes
- Testing: 15 minutes

**Total: ~2 hours**

## Alternative Considered

Annotating AST nodes with mutable `inferred_type` field - rejected because:
- Breaks immutability
- Couples AST to type system
- Circular dependency risk
