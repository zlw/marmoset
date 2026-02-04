# Phase 4, Milestone 1: Union Types

**Timeline**: 2-3 weeks  
**Status**: Not started  
**Dependencies**: Phase 3 complete (21/21 tests passing)  
**Last Updated**: 2026-02-03

---

## Overview

Union types enable values to have one of several possible types. They're the foundation for:

- **Null safety** via `option[a]` (value OR null)
- **Error handling** via `result[a, e]` (success OR error)  
- **Flexible APIs** that accept multiple input types
- **Type narrowing** for safe refinement in conditionals

**Example**:
```marmoset
fn maybe(x: int) -> int | string {
    if (x > 0) { x } else { "negative" }
}

let a = maybe(5)      // a: int | string
let b = maybe(-3)     // b: int | string
```

**Deliverable**: Union types work end-to-end: parse → typecheck → codegen → run.

---

## Current State Analysis

### What Already Exists

1. **AST Support** (`lib/frontend/syntax/ast.ml:10`):
   ```ocaml
   | TUnion of type_expr list (* int | string | bool *)
   ```
   ✅ AST already has union type representation

2. **Annotation Placeholder** (`lib/frontend/typecheck/annotation.ml:49-51`):
   ```ocaml
   | Syntax.Ast.AST.TUnion _ ->
       (* Union types (Phase 3): int | string | bool *)
       failwith "Union types not yet supported (Phase 3)"
   ```
   ✅ Module knows unions exist, just rejects them

3. **TNull Type** (`lib/frontend/typecheck/types.ml:9`):
   ```ocaml
   | TNull (* Null / unit *)
   ```
   ✅ We have a null type (will be replaced by `option[a]` in this milestone)

### What's Missing

1. ❌ **No `Pipe` token** in `lib/frontend/syntax/token.ml`
2. ❌ **No lexer support** for `|` character
3. ❌ **No parser logic** for union types in `parse_type_expr`
4. ❌ **No `TUnion`** in `lib/frontend/typecheck/types.ml` mono_type
5. ❌ **No unification rules** for unions
6. ❌ **No type inference** that creates unions from if-else
7. ❌ **No type narrowing** with `typeof` checks
8. ❌ **No codegen** for unions to Go

---

## Architecture Decisions

### 1. Union Representation

**Decision**: Store unions as normalized lists of mono_types.

```ocaml
type mono_type =
  | TInt
  | TString
  | TBool
  | TFloat
  | TNull
  | TVar of string
  | TFun of mono_type * mono_type
  | TArray of mono_type
  | THash of mono_type * mono_type
  | TUnion of mono_type list  (* NEW *)
```

**Normalization rules**:
- Flatten nested unions: `(int | string) | bool` → `int | string | bool`
- Remove duplicates: `int | string | int` → `int | string`
- Sort members for canonical representation
- Single-element unions reduce: `int` → just `int` (not `int` union)
- Empty unions are errors

**Rationale**: Normalized representation makes equality checking and unification deterministic.

---

### 2. Unification Strategy

**Decision**: Structural subtyping for union compatibility.

**Rules**:

1. **Widening** (concrete → union):
   ```
   int  ≤  int | string    ✓ OK
   ```
   A concrete type unifies with a union if it matches any member.

2. **Member access** (union → concrete):
   ```
   x: int | string
   x + 1               ✗ ERROR (can't add to union)
   ```
   Cannot use union directly; must narrow first.

3. **Union-to-union** unification:
   ```
   int | string  ≤  int | string | bool    ✓ OK (subset)
   ```
   Union A unifies with union B if every member of A is in B.

4. **Type variable unification**:
   ```
   'a = int | string    ✓ OK (bind 'a to union)
   ```

**Rationale**: This is simple, predictable, and matches TypeScript/Flow semantics. Full union subtyping (with variance in function positions) is deferred to future work.

---

### 3. Type Narrowing with `typeof`

**Decision**: Introduce `typeof` builtin for runtime type checks.

**Syntax**:
```marmoset
typeof(expr) == "typename"
```

**Narrowing rules**:

1. **In consequence branch**, narrow to matched type:
   ```marmoset
   fn process(x: int | string) -> int {
       if (typeof(x) == "int") {
           return x + 1    // x narrowed to int
       } else {
           return len(x)   // x narrowed to string
       }
   }
   ```

2. **In alternative branch**, narrow to complement:
   ```marmoset
   if (typeof(x) == "int") {
       // x: int
   } else {
       // x: string (if original was int | string)
   }
   ```

3. **Multiple checks compose**:
   ```marmoset
   fn handle(x: int | string | bool) {
       if (typeof(x) == "int") {
           // x: int
       }
       if (typeof(x) == "string") {
           // x: string
       }
       // x: bool (only option left)
   }
   ```

**Type names**:
- `"int"` for `TInt`
- `"float"` for `TFloat`
- `"string"` for `TString`
- `"bool"` for `TBool`
- `"null"` for `TNull`
- `"array"` for `TArray`
- `"hash"` for `THash`
- `"function"` for `TFun`

**Implementation**: Extend type environment to track refined types within scopes.

---

### 4. Null Safety with Option Types

**Decision**: Use unions to eliminate unsafe null access.

**Current problem** (Phase 1-3):
```marmoset
let arr = []
let x = arr[0]    // x: ?? (runtime null but no type safety)
x + 1             // ✓ Compiles but crashes at runtime
```

**Solution** (Phase 4.1):
```marmoset
let arr: list[int] = []
let x = arr[0]    // x: int | null (explicit option type)

// ERROR: can't use union directly
x + 1             // ✗ Type error

// OK: narrow first
if (typeof(x) == "int") {
    x + 1         // ✓ x is int here
}
```

**Migration strategy**:
1. **Milestone 1**: `int | null` as temporary option type
2. **Milestone 2**: Replace with proper `option[int]` enum with `some(value)` and `none`

**Breaking changes**:
- Array indexing returns `T | null` instead of `T`
- Hash access returns `V | null` instead of `V`
- `first`, `last`, `rest` return `T | null`

All existing tests will need updating to handle null checks.

---

### 5. Codegen Strategy

**Decision**: Union types compile to `interface{}` with type switches.

**Type mapping**:
```ocaml
TUnion [TInt; TString]  →  interface{}
```

**Value boxing**: All union values are boxed:
```go
var x interface{} = int64(42)       // int in union
var y interface{} = "hello"         // string in union
```

**Type narrowing** with `typeof` becomes Go type switch:

**Marmoset**:
```marmoset
if (typeof(x) == "int") {
    x + 1
} else {
    len(x)
}
```

**Generated Go**:
```go
switch v := x.(type) {
case int64:
    v + 1
case string:
    len(v)
}
```

**Performance considerations**:
- Boxing has overhead (heap allocation)
- Type switches are fast (single vtable lookup)
- Minimize unions in hot paths
- Monomorphization eliminates unions when type known at compile time

**Future optimization** (Milestone 3+): When union type is statically known at call site, specialize and avoid boxing.

---

## Detailed Implementation Plan

### Step 1.1: Lexer - Add Pipe Token

**Duration**: 0.5 days  
**Files**: `lib/frontend/syntax/token.ml`, `lib/frontend/syntax/lexer.ml`

#### Test (RED)

Add to `test/test_typecheck_and_codegen.sh`:

```bash
echo ""
echo "-- PHASE 4.1: UNION TYPE TESTS --"
test_case "Lex pipe token" \
    'let f = fn(x: int | string) { x }; f(5)' \
    "true"
```

Run: `./test/test_typecheck_and_codegen.sh`

**Expected**: FAIL - lexer doesn't recognize `|`

#### Implementation

**File**: `lib/frontend/syntax/token.ml`

Add after line 27 (`FatArrow`):

```ocaml
  | Pipe (* | for union types *)
```

Update `show_token_type` if it exists (for debugging).

**File**: `lib/frontend/syntax/lexer.ml`

Locate the character dispatch in `next_token` or `read_char`. Add case for `|`:

```ocaml
| '|' -> 
    let tok = Token.init Token.Pipe "|" ~pos:lexer.position in
    (advance lexer, tok)
```

**Note**: If `|` is already used for bitwise OR, we're co-opting it for union types. Bitwise OR can be added later as a separate operator if needed.

#### Verification

1. Build: `dune build`
2. Run inline lexer tests: `dune runtest`
3. Integration test should still fail but for a different reason (parser doesn't handle `|`)

---

### Step 1.2: Parser - Union Type Syntax

**Duration**: 1-2 days  
**Files**: `lib/frontend/syntax/parser.ml`

#### Test (RED)

```bash
test_case "Parse union in return type" \
    'let f = fn(x: int) -> int | string { if (x > 0) { x } else { "neg" } }; f(5)' \
    "true"

test_case "Parse multi-member union" \
    'let f = fn(x: int | string | bool) { x }; f(true)' \
    "true"

test_case "Parse union in parameter" \
    'let f = fn(x: int | string, y: bool) { x }; f(5, true)' \
    "true"
```

**Expected**: FAIL - parser doesn't parse `|` in type expressions

#### Implementation

**Strategy**: 
1. Parse type atoms (identifiers, applications, parentheses)
2. After parsing an atom, check for `Pipe` token
3. If found, collect more types and create `TUnion`
4. Otherwise, return single type

**File**: `lib/frontend/syntax/parser.ml`

Modify `parse_type_expr` (around lines 96-125). Split into two functions:

```ocaml
(* Parse a single type atom (not union) *)
and parse_type_atom (p : parser) : (parser * AST.type_expr, parser) result =
  if curr_token_is p Token.Ident then
    let ident = p.curr_token.literal in
    let p2 = next_token p in
    (* Check for generic application: list[int], map[string, int], etc. *)
    if curr_token_is p2 Token.LBracket then
      let* p3, type_args = parse_type_expr_list (next_token p2) in
      if curr_token_is p3 Token.RBracket then
        Ok (next_token p3, AST.TApp (ident, type_args))
      else
        Error (peek_error p3 Token.RBracket)
    else
      Ok (p2, AST.TCon ident)
  else if curr_token_is p Token.Function then
    (* Function type: fn(int, string) -> bool *)
    let* p2 = expect_peek p Token.LParen in
    let* p3, param_types = parse_type_expr_list (next_token p2) in
    let* p4 = expect_peek p3 Token.RParen in
    let* p5 = expect_peek p4 Token.Arrow in
    let* p6, return_type = parse_type_expr (next_token p5) in
    Ok (p6, AST.TArrow (param_types, return_type))
  else if curr_token_is p Token.LParen then
    (* Parenthesized type or function type: (int, string) -> bool *)
    let* p2, first = parse_type_expr (next_token p) in
    if curr_token_is p2 Token.Comma then
      (* Multiple params: (int, string) -> bool *)
      let rec collect_params lp params =
        let* lp2, param_type = parse_type_expr (next_token lp) in
        let new_params = params @ [param_type] in
        if curr_token_is lp2 Token.Comma then
          collect_params lp2 new_params
        else if curr_token_is lp2 Token.RParen then
          Ok (next_token lp2, new_params)
        else
          Error (peek_error lp2 Token.RParen)
      in
      let* p3, params = collect_params p2 [first] in
      let* p4 = expect_peek p3 Token.Arrow in
      let* p5, return_type = parse_type_expr (next_token p4) in
      Ok (p5, AST.TArrow (params, return_type))
    else if curr_token_is p2 Token.RParen then
      (* Single type in parens: (int) *)
      let p3 = next_token p2 in
      if curr_token_is p3 Token.Arrow then
        (* Single-param function: (int) -> bool *)
        let* p4 = expect_peek p3 Token.Arrow in
        let* p5, return_type = parse_type_expr (next_token p4) in
        Ok (p5, AST.TArrow ([first], return_type))
      else
        (* Just grouping: (int | string) *)
        Ok (p3, first)
    else
      Error (peek_error p2 Token.RParen)
  else
    Error (no_prefix_parse_fn_error p p.curr_token.token_type)

(* Parse a type expression, including unions: int | string | bool *)
and parse_type_expr (p : parser) : (parser * AST.type_expr, parser) result =
  let* p2, first_type = parse_type_atom p in
  (* Check for union: type | type | type ... *)
  if curr_token_is p2 Token.Pipe then
    let rec collect_union_members lp members =
      let* lp2, next_type = parse_type_atom (next_token lp) in
      let new_members = members @ [next_type] in
      if curr_token_is lp2 Token.Pipe then
        collect_union_members lp2 new_members
      else
        Ok (lp2, AST.TUnion new_members)
    in
    collect_union_members p2 [first_type]
  else
    Ok (p2, first_type)
```

**Precedence**: `|` has lower precedence than function arrows. So:
- `int -> string | bool` parses as `(int -> string) | bool`
- Use parens for clarity: `int -> (string | bool)`

#### Verification

1. Build: `dune build`
2. Check parser inline tests: `dune runtest`
3. Integration tests should now fail on type system (no `TUnion` in `types.ml`)

---

### Step 1.3: Type System - Add TUnion

**Duration**: 1 day  
**Files**: `lib/frontend/typecheck/types.ml`

#### Test (RED)

Previous tests still fail - type system doesn't have `TUnion`.

#### Implementation

**File**: `lib/frontend/typecheck/types.ml`

Add to `mono_type` definition (after line 13):

```ocaml
  | TUnion of mono_type list  (* Union: Int | String | Bool *)
```

Update `to_string` function (after `THash` case around line 53):

```ocaml
  | TUnion types ->
      String.concat " | " (List.map to_string types)
```

Update `apply_substitution` (after `THash` case around line 82):

```ocaml
  | TUnion types -> 
      TUnion (List.map (apply_substitution subst) types)
```

Update `free_type_vars` (after `THash` case around line 110):

```ocaml
  | TUnion types ->
      List.fold_left
        (fun acc t -> TypeVarSet.union acc (free_type_vars t))
        TypeVarSet.empty types
```

Update `collect_vars_in_order` (after `THash` case around line 142):

```ocaml
  | TUnion types -> 
      List.concat_map collect_vars_in_order types
```

Add normalization function (before tests, around line 165):

```ocaml
(* ============================================================
   Union Type Normalization
   ============================================================ *)

(* Normalize a union type: flatten, dedupe, sort *)
let rec normalize_union (types : mono_type list) : mono_type =
  (* Step 1: Flatten nested unions *)
  let rec flatten = function
    | TUnion inner -> List.concat_map flatten inner
    | t -> [t]
  in
  let flattened = List.concat_map flatten types in
  
  (* Step 2: Remove duplicates by sorting and filtering *)
  let sorted = List.sort compare flattened in
  let rec dedup = function
    | [] -> []
    | [x] -> [x]
    | x :: y :: rest when x = y -> dedup (y :: rest)
    | x :: rest -> x :: dedup rest
  in
  let unique = dedup sorted in
  
  (* Step 3: Return normalized result *)
  match unique with
  | [] -> failwith "Empty union type"
  | [single] -> single  (* Single-element union = just the type *)
  | multiple -> TUnion multiple
```

#### Verification

Add tests at end of `types.ml`:

```ocaml
(* Union type tests *)

let%test "to_string union" =
  to_string (TUnion [TInt; TString]) = "Int | String"

let%test "to_string multi-member union" =
  to_string (TUnion [TInt; TString; TBool]) = "Int | String | Bool"

let%test "normalize_union dedupes" =
  normalize_union [TInt; TString; TInt] = TUnion [TInt; TString]

let%test "normalize_union flattens nested" =
  let nested = TUnion [TInt; TUnion [TString; TBool]] in
  normalize_union [nested; TFloat] = TUnion [TBool; TFloat; TInt; TString]

let%test "normalize_union single element" =
  normalize_union [TInt] = TInt

let%test "apply_substitution to union" =
  let subst = [("a", TInt); ("b", TString)] in
  let union = TUnion [TVar "a"; TVar "b"; TBool] in
  apply_substitution subst union = TUnion [TInt; TString; TBool]
```

Run: `dune runtest`

All new tests should pass. Integration tests may still fail (annotation module rejects unions).

---

### Step 1.4: Annotation Module - Convert AST.TUnion

**Duration**: 0.5 days  
**Files**: `lib/frontend/typecheck/annotation.ml`

#### Implementation

**File**: `lib/frontend/typecheck/annotation.ml`

Replace lines 49-51 (the failwith for TUnion):

```ocaml
  | Syntax.Ast.AST.TUnion type_exprs ->
      let mono_types = List.map type_expr_to_mono_type type_exprs in
      Types.normalize_union mono_types
```

Update `format_mono_type` function (add after `TFun` case around line 90):

```ocaml
  | Types.TUnion types ->
      String.concat " | " (List.map format_mono_type types)
```

Update `mono_types_equal` function (add case before final `| _ -> false`):

```ocaml
  | Types.TUnion t1s, Types.TUnion t2s ->
      List.length t1s = List.length t2s &&
      List.for_all2 mono_types_equal t1s t2s
```

#### Verification

Run: `dune runtest`

Integration tests should now fail at unification (unions not handled).

---

### Step 1.5: Unification - Union Compatibility

**Duration**: 2 days  
**Files**: `lib/frontend/typecheck/unify.ml`

#### Test (RED)

```bash
test_case "Union parameter accepts member type" \
    'let f = fn(x: int | string) -> string { "ok" }; f(42)' \
    "true"

test_case "Union parameter accepts other member" \
    'let f = fn(x: int | string) -> string { "ok" }; f("hello")' \
    "true"

test_case "Reject type not in union" \
    'let f = fn(x: int | string) -> string { "ok" }; f(true)' \
    "false" \
    "Cannot unify Bool with"
```

**Expected**: FAIL - unification doesn't handle unions

#### Implementation

**File**: `lib/frontend/typecheck/unify.ml`

Add union cases to `unify` function. Insert before the final catch-all `| _, _ -> Error (TypeMismatch ...)` (around line 47):

```ocaml
  (* Union on right - concrete type must match at least one member *)
  | concrete, TUnion members ->
      unify_concrete_with_union concrete members
  (* Union on left - same as right case (symmetrical) *)
  | TUnion members, concrete ->
      unify_concrete_with_union concrete members
  (* Both unions - check subset relationship *)
  | TUnion members1, TUnion members2 ->
      unify_union_with_union members1 members2
```

Add helper functions before `unify` (around line 25):

```ocaml
(* Helper: Check if concrete type matches any union member *)
let rec unify_concrete_with_union (concrete : mono_type) (members : mono_type list)
    : (substitution, unify_error) result =
  match members with
  | [] -> Error (TypeMismatch (concrete, TUnion members))
  | first :: rest ->
      (* Try to unify with first member *)
      (match unify concrete first with
       | Ok subst -> Ok subst
       | Error _ -> 
           (* Failed, try remaining members *)
           unify_concrete_with_union concrete rest)

(* Helper: Unify two union types (all members of left must be in right) *)
and unify_union_with_union (members1 : mono_type list) (members2 : mono_type list)
    : (substitution, unify_error) result =
  (* For simplicity: unions must be equal *)
  (* Full subtyping would check if members1 ⊆ members2 *)
  if List.length members1 = List.length members2 then
    (* Try to unify element-wise *)
    let rec unify_all subst ms1 ms2 =
      match (ms1, ms2) with
      | [], [] -> Ok subst
      | m1 :: rest1, m2 :: rest2 ->
          let m1' = apply_substitution subst m1 in
          let m2' = apply_substitution subst m2 in
          (match unify m1' m2' with
           | Ok subst' ->
               let composed = compose_substitution subst subst' in
               unify_all composed rest1 rest2
           | Error e -> Error e)
      | _ -> Error (TypeMismatch (TUnion members1, TUnion members2))
    in
    unify_all empty_substitution members1 members2
  else
    Error (TypeMismatch (TUnion members1, TUnion members2))
```

**Note**: This is simplified union unification. It handles:
- Widening: `int` → `int | string`
- Equal unions: `int | string` ↔ `int | string`

It does NOT yet handle:
- Subset unions: `int` → `int | string | bool`
- Reordering: `int | string` vs `string | int` (normalization helps here)

These can be refined later if needed.

#### Verification

Add tests to end of `unify.ml`:

```ocaml
(* Union type unification tests *)

let%test "unify concrete with union member" =
  (* int unifies with int | string *)
  match unify TInt (TUnion [TInt; TString]) with
  | Ok _ -> true
  | Error _ -> false

let%test "unify union with concrete member" =
  (* int | string unifies with int *)
  match unify (TUnion [TInt; TString]) TInt with
  | Ok _ -> true
  | Error _ -> false

let%test "fail unify concrete not in union" =
  (* bool does NOT unify with int | string *)
  fails_to_unify TBool (TUnion [TInt; TString])

let%test "unify equal unions" =
  let union1 = TUnion [TInt; TString] in
  let union2 = TUnion [TInt; TString] in
  match unify union1 union2 with
  | Ok _ -> true
  | Error _ -> false

let%test "unify union with type variable" =
  (* 'a unifies with int | string, binding 'a to union *)
  match unify (TVar "a") (TUnion [TInt; TString]) with
  | Ok subst -> 
      apply_substitution subst (TVar "a") = TUnion [TInt; TString]
  | Error _ -> false
```

Run: `dune runtest`

All new tests should pass. Integration tests may still fail (inference doesn't create unions).

---

### Step 1.6: Type Inference - Create Unions from If-Else

**Duration**: 1-2 days  
**Files**: `lib/frontend/typecheck/infer.ml`

#### Test (RED)

```bash
test_case "If-else with different types creates union" \
    'let x = if (true) { 42 } else { "hello" }; x' \
    "true"

test_case "Nested if creates multi-member union" \
    'let x = if (true) { 42 } else if (false) { "hi" } else { true }; x' \
    "true"
```

**Expected**: FAIL - inference tries to unify mismatched types and fails

#### Implementation

**File**: `lib/frontend/typecheck/infer.ml`

Locate the `If` expression case in the main `infer` function (likely around line 270-310). Current behavior:
- Infers consequence type
- Infers alternative type (if present)
- Unifies them
- Returns unified type

**Modify** to:
- Infer both types
- Try to unify
- If unification succeeds, return unified type
- If unification fails, create a union type

Find code similar to:

```ocaml
| AST.If (condition, consequence, alternative) ->
    ...
    match alternative with
    | None -> 
        ...
        Ok (subst', TNull)
    | Some alternative ->
        let* subst', alt_type = infer env' alternative in
        let subst'' = compose_substitution subst subst' in
        let cons_type' = apply_substitution subst'' cons_type in
        let alt_type' = apply_substitution subst'' alt_type in
        (* Currently: try to unify *)
        let* subst''' = Unify.unify cons_type' alt_type' in
        ...
```

**Replace** the unification with:

```ocaml
        (* Try to unify; if it fails, create a union *)
        (match Unify.unify cons_type' alt_type' with
         | Ok subst''' -> 
             (* Types unified successfully *)
             let final_subst = compose_substitution subst'' subst''' in
             let result_type = apply_substitution subst''' cons_type' in
             Ok (final_subst, result_type)
         | Error _ ->
             (* Types don't unify - create union *)
             let union_type = Types.normalize_union [cons_type'; alt_type'] in
             Ok (subst'', union_type))
```

#### Verification

Run integration tests: `./test/test_typecheck_and_codegen.sh`

Tests should progress further but still fail (no `typeof` builtin yet).

---

### Step 1.7: Add `typeof` Builtin

**Duration**: 1 day  
**Files**: `lib/frontend/typecheck/builtins.ml`, `lib/backend/interpreter/builtins.ml`, `lib/backend/vm/compiler.ml`

#### Test (RED)

```bash
test_case "typeof returns string" \
    'let x = 42; typeof(x)' \
    "true"
```

**Expected**: FAIL - `typeof` not defined

#### Implementation

**File**: `lib/frontend/typecheck/builtins.ml`

Add after the `puts` definition (around line 45):

```ocaml
    (* typeof : a -> string
       Returns string representation of type for runtime checks *)
    ("typeof", forall ["a"] (TFun (TVar "a", TString)));
```

**File**: `lib/backend/interpreter/builtins.ml`

Add implementation after `puts` (around line 60):

```ocaml
let typeof_builtin = function
  | [arg] -> (
      let type_name = match arg with
        | Value.Integer _ -> "int"
        | Value.Float _ -> "float"
        | Value.Boolean _ -> "bool"
        | Value.String _ -> "string"
        | Value.Null -> "null"
        | Value.Array _ -> "array"
        | Value.Hash _ -> "hash"
        | Value.Function _ | Value.Closure _ | Value.Builtin _ -> "function"
      in
      Value.String type_name)
  | _ -> failwith "typeof expects 1 argument"
```

Add to builtins list:

```ocaml
let builtins = [
  ...
  ("typeof", typeof_builtin);
]
```

**File**: `lib/backend/vm/compiler.ml`

Add to builtin compilation if needed (search for `compile_builtin_call`).

#### Verification

Test that `typeof(42)` returns `"int"`.

---

### Step 1.8: Type Narrowing in Conditionals

**Duration**: 2-3 days  
**Files**: `lib/frontend/typecheck/infer.ml`

This is the most complex step. It requires detecting `typeof` patterns and refining types in branches.

#### Test (RED)

```bash
test_case "Type narrowing with typeof in if" \
    'let f = fn(x: int | string) -> int {
       if (typeof(x) == "int") { 
           x + 1 
       } else { 
           len(x) 
       }
     }
     f(5)' \
    "true"

test_case "Type narrowing error without check" \
    'let f = fn(x: int | string) -> int { x + 1 }; f(5)' \
    "false" \
    "Cannot unify"
```

**Expected**: First test should eventually pass. Second should fail (can't add to union).

#### Implementation

**Strategy**:
1. Detect pattern: `typeof(identifier) == "literal"`
2. Extract the identifier and type name
3. Narrow the identifier's type in the environment for the consequence branch
4. Narrow to complement type in alternative branch

This requires:

**A. Add narrowing detection function** (add to `infer.ml`):

```ocaml
(* Detect typeof narrowing pattern: typeof(var) == "typename" *)
let detect_typeof_narrowing (expr : AST.expression) 
    : (string * string) option =
  match expr.expr with
  | AST.Infix (left, "==", right) -> (
      (* Check if left is typeof(var) *)
      match (left.expr, right.expr) with
      | AST.Call (fn_expr, [arg_expr]), AST.String type_name -> (
          match (fn_expr.expr, arg_expr.expr) with
          | AST.Identifier "typeof", AST.Identifier var_name ->
              Some (var_name, type_name)
          | _ -> None)
      | _ -> None)
  | _ -> None

(* Convert type name string to mono_type *)
let string_to_mono_type (type_name : string) : mono_type option =
  match type_name with
  | "int" -> Some TInt
  | "float" -> Some TFloat
  | "bool" -> Some TBool
  | "string" -> Some TString
  | "null" -> Some TNull
  | "array" -> None  (* Can't narrow to generic array *)
  | "hash" -> None
  | "function" -> None
  | _ -> None
```

**B. Add narrowing to environment** (modify `If` case):

```ocaml
| AST.If (condition, consequence, alternative) ->
    (* Infer condition *)
    let* subst, cond_type = infer env condition in
    (* Check for unification with Bool *)
    ...
    
    (* Check if condition is a typeof narrowing *)
    let narrowing = detect_typeof_narrowing condition in
    
    (* Create narrowed environment for consequence *)
    let env_cons = match narrowing with
      | Some (var_name, type_name) ->
          (match string_to_mono_type type_name with
           | Some narrow_type ->
               (* Look up current type of var *)
               (match Env.get env' var_name with
                | Some (Forall ([], current_type)) ->
                    (* Narrow union to specific member *)
                    (match current_type with
                     | TUnion members when List.mem narrow_type members ->
                         (* Replace var's type with narrowed type *)
                         Env.extend env' var_name (Forall ([], narrow_type))
                     | _ -> env')
                | _ -> env')
           | None -> env')
      | None -> env'
    in
    
    (* Infer consequence with narrowed environment *)
    let* subst', cons_type = infer env_cons consequence in
    ...
```

**C. Handle alternative branch** (narrow to complement):

Similar logic for alternative, but narrow to the "other" types in the union.

**Implementation note**: This is complex and may require multiple iterations. Start with simple case (single typeof check) and expand.

#### Verification

Integration tests should pass for narrowing.

---

### Step 1.9: Codegen - Union Types to Go

**Duration**: 2-3 days  
**Files**: `lib/backend/go/emitter.ml`

#### Test (RED)

```bash
test_case "Union compiles to Go interface{}" \
    'let f = fn(x: int | string) -> string { "ok" }; f(5)' \
    "true"
```

**Expected**: FAIL - emitter doesn't handle union types

#### Implementation

**File**: `lib/backend/go/emitter.ml`

**A. Union type representation**:

Find `mono_type_to_go_type` function (around line 17). Add case:

```ocaml
  | Types.TUnion _ -> "interface{}"
```

All union values are boxed in `interface{}`.

**B. Union value boxing**:

When emitting a value that goes into a union, ensure it's boxed. Find value emission code. No changes needed if Go already boxes primitives in `interface{}`.

**C. typeof check to type switch**:

Find if-expression emission. When condition is `typeof(x) == "typename"`:

```ocaml
(* Pseudo-code for emitter logic *)
if is_typeof_check condition then
  (* Generate type switch instead of if *)
  emit_type_switch var type_name consequence alternative
else
  (* Normal if *)
  emit_if condition consequence alternative
```

**D. Type switch template**:

```ocaml
let emit_type_switch var_name type_name consequence alternative =
  (* Generate:
     switch v := x.(type) {
     case int64:
         // consequence with v as variable
     default:
         // alternative
     }
  *)
  Printf.sprintf "switch %s_typed := %s.(type) {\n" var_name var_name ^
  Printf.sprintf "case %s:\n" (type_name_to_go type_name) ^
  emit_block consequence (with var renamed to var_typed) ^
  "default:\n" ^
  emit_block alternative ^
  "}"
```

Map type names:
- `"int"` → `int64`
- `"float"` → `float64`
- `"string"` → `string`
- `"bool"` → `bool`

**E. Variable substitution**:

Inside the type switch case, the variable is the typed version. Track this in the emission environment.

#### Verification

Run full integration tests:

```bash
./test/test_typecheck_and_codegen.sh
```

Should see unions compile and run correctly.

---

### Step 1.10: Exhaustiveness Checking (Basic)

**Duration**: 1-2 days  
**Files**: `lib/frontend/typecheck/checker.ml`

#### Test (RED)

```bash
test_case "Missing union case is error" \
    'let f = fn(x: int | string | bool) -> int {
       if (typeof(x) == "int") { x }
       else if (typeof(x) == "string") { len(x) }
       // Missing bool case
     }
     f(5)' \
    "false" \
    "Non-exhaustive"
```

**Expected**: Should eventually fail with exhaustiveness error

#### Implementation

**File**: `lib/frontend/typecheck/checker.ml`

Add exhaustiveness checking for union types in if-expressions:

```ocaml
(* Check if all union members are covered by typeof checks *)
let check_union_exhaustiveness 
    (union_type : Types.mono_type) 
    (checks : string list) 
    : (unit, string) result =
  match union_type with
  | Types.TUnion members ->
      let member_names = List.map (fun t ->
        match t with
        | Types.TInt -> "int"
        | Types.TString -> "string"
        | Types.TBool -> "bool"
        | Types.TFloat -> "float"
        | Types.TNull -> "null"
        | _ -> "unknown"
      ) members in
      let unchecked = List.filter (fun m -> not (List.mem m checks)) member_names in
      if unchecked = [] then
        Ok ()
      else
        Error (Printf.sprintf "Non-exhaustive typeof checks. Missing: %s" 
                 (String.concat ", " unchecked))
  | _ -> Ok ()  (* Not a union, no exhaustiveness needed *)
```

Integrate into the checker flow (after type inference).

#### Verification

Test that missing cases produce errors.

---

### Step 1.11: Edge Cases & Error Messages

**Duration**: 1-2 days  
**Files**: Various

#### Tests

```bash
test_case "Empty union is error" \
    'let f = fn() -> | { 5 }; f()' \
    "false" \
    "Empty union"

test_case "Single-member union normalizes" \
    'let f = fn(x: int | int) -> int { x }; f(5)' \
    "true"

test_case "Union in array type" \
    'let x: list[int | string] = [1, "hello"]; x' \
    "true"

test_case "Nested union flattens" \
    'let f = fn(x: (int | string) | bool) -> bool { true }; f(5)' \
    "true"

test_case "Cannot use union without narrowing" \
    'let f = fn(x: int | string) -> int { x + 1 }; f(5)' \
    "false" \
    "Cannot unify"
```

#### Implementation

1. **Empty union check** in `normalize_union`
2. **Better error messages** for union mismatches
3. **Documentation** in error messages explaining narrowing

---

## Completion Criteria

### Must Pass (100%)

**Unit tests**:
- [ ] All `types.ml` tests pass (new union tests)
- [ ] All `unify.ml` tests pass (union unification)
- [ ] All existing Phase 1-3 tests pass (no regressions)

**Integration tests** (at least 15 new tests):
- [ ] Parse union syntax
- [ ] Union parameter accepts member types
- [ ] If-else creates unions from different types
- [ ] typeof returns type names
- [ ] Type narrowing with typeof
- [ ] Narrowing in consequence branch
- [ ] Narrowing in alternative branch
- [ ] Compile unions to Go
- [ ] Run compiled code correctly
- [ ] Error on non-exhaustive checks
- [ ] Error on union member mismatch
- [ ] Edge cases (empty, single-element, nested)

### Features Working

- [ ] **Lexer**: `|` token
- [ ] **Parser**: Union type syntax in all positions
- [ ] **Type system**: `TUnion` in `mono_type`
- [ ] **Unification**: Union compatibility rules
- [ ] **Inference**: Create unions from if-else
- [ ] **Narrowing**: `typeof` checks refine types
- [ ] **Codegen**: Unions → `interface{}` + type switches
- [ ] **Runtime**: Compiled code runs correctly

### Documentation

- [ ] Update `CLAUDE.md` with Milestone 1 status
- [ ] Update `README.md` features table
- [ ] Add doc comments to all new functions
- [ ] Add inline code examples in comments

---

## Migration Path for Breaking Changes

### Array/Hash Operations Return Unions

**Breaking change**: Array indexing and hash access now return `T | null` instead of `T`.

**Before** (Phase 3):
```marmoset
let arr = [1, 2, 3]
let x = arr[0]
let y = x + 1    // ✓ OK (runtime null check)
```

**After** (Phase 4.1):
```marmoset
let arr = [1, 2, 3]
let x = arr[0]   // x: int | null
let y = x + 1    // ✗ ERROR: cannot add to union

// Must narrow first:
if (typeof(x) == "int") {
    let y = x + 1  // ✓ OK
}
```

**Migration steps**:
1. Update all existing tests to handle `| null` return types
2. Add `typeof` checks where needed
3. Run test suite, fix failures one by one
4. Document breaking changes in commit message

**Alternative**: Provide `get_unchecked` variants that return `T` but can panic. Use sparingly.

---

## Known Limitations

### Explicitly OUT OF SCOPE for Milestone 1:

1. **Full exhaustiveness analysis** - only basic typeof checks
2. **Pattern matching** - comes in Milestone 2
3. **Enum definitions** - comes in Milestone 2 (`option[a]`, `result[a,e]`)
4. **Union subtyping in function positions** - simplified rules only
5. **Variance** - covariance/contravariance deferred
6. **Intersection types** - not planned
7. **Discriminated unions** - pattern matching needed first
8. **Union distribution** - `(A | B) -> C` vs `(A -> C) | (B -> C)`

These will be addressed in future milestones or explicitly decided as won't-fix.

---

## References

**Related documents**:
- `docs/typechecker/plan.md` - Overall type system roadmap
- `docs/typechecker/approach.md` - Trait system design (Milestone 3)
- `docs/SYNTAX_DECISIONS.md` - Language syntax reference
- `CLAUDE.md` - TDD process and phase completion criteria

**Similar languages**:
- **TypeScript**: Union types, type narrowing, discriminated unions
- **Flow**: Union types, refinement
- **Gleam**: Result types, pattern matching
- **Rust**: Enums (tagged unions), pattern matching

**Academic references**:
- *Union Types for TypeScript* - Union type semantics
- *Occurrence Typing* - Type narrowing in Typed Racket
- *Refinement Types* - Dependent types with predicates

---

**Last Updated**: 2026-02-03  
**Author**: Claude (with human guidance)  
**Status**: Ready for implementation
