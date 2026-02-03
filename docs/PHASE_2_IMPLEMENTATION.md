# Phase 2 Implementation Plan: Type Annotations & Minimal Bidirectional Checking

**Phase**: 2 - Bidirectional Extensions  
**Estimated Duration**: 5-7 days  
**Status**: READY FOR IMPLEMENTATION

This plan covers ONLY Phase 2. For all syntax decisions (including Phase 3+), see `SYNTAX_DECISIONS.md`.

---

## Overview

Phase 2 adds optional type annotations to the language while keeping Hindley-Milner inference working for non-annotated code. The implementation is minimal:
- Parser accepts annotations
- AST stores annotations
- Typechecker verifies annotations match inferred types (no sophisticated bidirectional flow yet)

**Result**: Programmers can write `fn max[a: ord](x: a, y: a) -> a`, but the `ord` constraint won't actually do anything until Phase 3 implements traits. However, the parser accepts it and the typechecker prepares it for Phase 3.

---

## 1. Changes Required by Component

### 1.1 Lexer (lib/frontend/syntax/lexer.ml)

**Current**: Handles basic tokens like `fn`, `let`, numbers, identifiers.

**Changes**:
- Add tokens for new symbols:
  - `COLON` for `:`
  - `ARROW` for `->`
  - `FAT_ARROW` for `=>` (effect marker)
  - `LBRACKET`, `RBRACKET` for `[` and `]` (generics, already exists for arrays - reuse)
  - `PLUS` for `+` (already exists for arithmetic, needs context-aware handling)

**Implementation notes**:
- `[` and `]` already lex as array notation. In Phase 2, context matters:
  - `fn[a]` = generic parameter
  - `[1, 2, 3]` = array literal
  - Parser disambiguates based on position
- `+` already lexes for arithmetic. Reuse same token; parser handles context (trait composition vs addition)

**Files to modify**:
- `lib/frontend/syntax/lexer.ml` — add COLON, ARROW, FAT_ARROW tokens

---

### 1.2 Parser (lib/frontend/syntax/parser.ml)

**Current**: Parses expressions, statements, functions without type annotations.

**Changes needed**:

#### Grammar Changes

```
(* New rules for type annotations *)

type_annotation ::= ":" type_expr
return_annotation ::= "->" type_expr
effect_annotation ::= "=>" type_expr

generic_params ::= "[" generic_param ("," generic_param)* "]"
generic_param ::= identifier
                | identifier ":" trait_constraint

trait_constraint ::= identifier ("+" identifier)*

type_expr ::= identifier                          (* int, string, a, b *)
            | "[" type_expr "]"                  (* [int], [a] *)
            | "{" type_expr ":" type_expr "}"   (* {string: int} *)
            | "fn" "(" param_types? ")" "->" type_expr  (* fn(int, string) -> bool *)
            | type_expr "|" type_expr             (* int | string — Phase 3, parse but error in Phase 2 *)

param_types ::= type_expr ("," type_expr)*

(* Modified rules *)

function_expr ::= "fn" generic_params? "(" parameters? ")" return_annotation? "{" block "}"
let_stmt ::= "let" identifier type_annotation? "=" expression

parameter ::= identifier type_annotation?       (* x: int *)
```

#### Parser Functions to Add/Modify

```ocaml
(* New functions *)
val parse_type_annotation : unit -> type_expr option
val parse_type_expr : unit -> type_expr
val parse_generic_params : unit -> generic_param list
val parse_trait_constraint : unit -> string list  (* list of trait names *)

(* Modified functions *)
val parse_function_expr : unit -> expression  (* now handles generics + annotations *)
val parse_let_stmt : unit -> statement        (* now handles : type *)
val parse_parameter : unit -> parameter       (* now handles : type *)
```

**Key design decisions**:
- All annotation parsing is **optional** — missing annotations are OK
- Generic parameters `[a]` parsed before `(` in function position
- Trait constraints `[a: show + eq]` parsed as part of generic params
- Return type annotation is optional: `fn(...) { }` (no return type) is valid

**Files to modify**:
- `lib/frontend/syntax/parser.ml` — add annotation parsing rules

---

### 1.3 AST (lib/frontend/syntax/ast.ml)

**Current**: Expression and Statement types have no room for type annotations.

**Changes needed**:

```ocaml
(* New AST node for type expressions *)
type type_expr =
  | TVar of string                                 (* 'a', 'b' *)
  | TCon of string                                 (* 'int', 'string' *)
  | TApp of type_expr * type_expr                 (* [int], [a] *)
  | TArrow of type_expr list * type_expr          (* (int, string) -> bool *)
  | THash of type_expr * type_expr                (* {string: int} *)

(* Generic parameter with optional constraint *)
type generic_param =
  {
    name: string;                                  (* 'a', 'b' *)
    constraints: string list;                      (* ['show', 'eq'] from 'a: show + eq' *)
  }

(* Modified function expression *)
type expression =
  | ...
  | Function of {
      generics: generic_param list option;         (* [a: ord] *)
      params: identifier list;
      body: statement;
      return_type: type_expr option;               (* : int *)
    }

(* Modified let statement *)
type statement =
  | ...
  | Let of {
      name: identifier;
      type_annotation: type_expr option;           (* : int *)
      value: expression;
    }

(* New identifier type with optional annotation *)
type parameter =
  {
    name: identifier;
    type_annotation: type_expr option;             (* x: int *)
  }

(* Effect marker for future use *)
type effect_marker = Pure | Impure

(* Optional: store effect marker on function *)
type expression =
  | ...
  | Function of {
      generics: generic_param list option;
      params: parameter list;
      body: statement;
      return_type: type_expr option;
      effect: effect_marker option;                (* -> vs => *)
    }
```

**Migration from current AST**:
- Current `Function of (expression list * statement)` becomes `Function of { ... }`
- Current `Let of (string * expression)` becomes `Let of { name: ...; value: ...; type_annotation: ... }`
- This is a breaking change, but Phase 1 is complete and no external code uses AST

**Files to modify**:
- `lib/frontend/syntax/ast.ml` — add `type_expr`, modify `expression` and `statement`

---

### 1.4 Typechecker (lib/frontend/typecheck/)

**Current**: `Infer.infer_expression` computes types based on expressions. No awareness of annotations.

**Changes needed**:

#### New Module: `Annotation.ml`

```ocaml
(* Convert parsed type_expr to internal mono_type *)
val type_expr_to_mono_type : AST.type_expr -> Types.mono_type

(* Check that annotation matches inferred type *)
val check_annotation : expected:Types.mono_type -> inferred:Types.mono_type -> bool

(* Extract constraints from generic params *)
val extract_constraints : AST.generic_param list -> string list list
```

#### Modifications to `Infer.ml`

```ocaml
(* New checking mode — when annotation is present *)
val check_expression : 
  env:type_env -> 
  expr:AST.expression -> 
  expected:Types.mono_type ->
  (substitution * Types.mono_type, error) result

(* Modified inference to skip when annotation present *)
val infer_expression :
  env:type_env -> 
  expr:AST.expression ->
  (substitution * Types.mono_type, error) result

(* When expression has annotation, use check_expression instead *)
(* When no annotation, use existing infer_expression *)
```

#### Modifications to `Checker.ml`

```ocaml
(* New entry point for Phase 2 *)
val check_program_with_annotations :
  typed_env:Infer.type_env ->
  program:AST.program ->
  (typecheck_result, error) result

(* Modified to handle annotations in let bindings *)
val infer_let : ... -> ... option = function
  | AST.Let { name; type_annotation = Some annot; value } ->
      (* Use check mode if annotation present *)
      check_expression env value (type_expr_to_mono_type annot)
  | AST.Let { name; type_annotation = None; value } ->
      (* Use existing infer mode if no annotation *)
      infer_expression env value
```

#### New Error Types

```ocaml
type error_kind =
  | ...existing...
  | AnnotationMismatch of {
      expected: mono_type;           (* from annotation *)
      inferred: mono_type;           (* from expression *)
      location: source_loc option;
    }
  | UnboundTypeVariable of string  (* 'T' in annotation, 'T' unknown *)
  | InvalidConstraint of string    (* 'unknown_trait: show' *)
```

**Files to modify**:
- `lib/frontend/typecheck/annotation.ml` (new)
- `lib/frontend/typecheck/infer.ml` — add `check_expression` function
- `lib/frontend/typecheck/checker.ml` — add `check_program_with_annotations`

---

### 1.5 Integration

**Main entry point** (`bin/main.ml`):

```ocaml
(* Updated build pipeline *)
let compile_string source =
  match Syntax.Parser.parse source with
  | Error errors -> Error ("Parse error: " ^ ...)
  | Ok program ->
      let env = Typecheck.Builtins.prelude_env () in
      (* First pass: check annotations and infer types *)
      match Typecheck.Checker.check_program_with_annotations env program with
      | Error e -> Error (Checker.format_error source e)
      | Ok (typed_env, _) ->
          (* Second pass: generate code using typed environment *)
          Ok (Go_emitter.emit_program_with_typed_env typed_env program)
```

**No changes to codegen** — `Go_emitter` remains unchanged. It already threads the typed environment through.

---

## 2. Implementation Sequence

### Week 1

**Day 1-2: Parser**
1. Add tokens to lexer (COLON, ARROW, FAT_ARROW)
2. Add grammar rules for:
   - Type expressions: `int`, `[a]`, `fn(int) -> string`, etc.
   - Generic parameters: `[a]`, `[a: show]`, `[a: show + eq, b: eq]`
   - Function annotations: `fn[a](x: a) -> a`
   - Let annotations: `let x: int = 5`
   - Parameter annotations: `fn(x: int, y: int)`
   - Return type annotations: `fn(...) -> int`
3. Update AST nodes to store annotations
4. Update parser to populate new AST fields

**Day 3: Annotation Module**
1. Create `Annotation.ml`
2. Implement `type_expr_to_mono_type` — converts parsed types to internal representation
3. Implement error reporting for mismatches
4. Write tests for annotation parsing and conversion

**Day 4: Typechecker Integration**
1. Modify `Infer.ml` to add `check_expression` function
2. Modify `Checker.ml` to use annotations when present
3. Add error types for annotation mismatches
4. Wire up annotation checking in the pipeline

**Day 5-7: Testing & Debugging**
1. Write tests for annotated functions
2. Test that non-annotated code still works (regression tests)
3. Test constraint syntax (parsing only, no semantic checking)
4. Test error messages for mismatches
5. Integration testing

---

## 3. What to Test

### Parser Tests
```marmoset
✓ let x: int = 5
✓ fn(x: int, y: int) -> int { x + y }
✓ fn[a](x: a) -> a { x }
✓ fn[a: show](x: a) -> unit { puts(x.show()) }
✓ fn[a: show + eq, b: eq](x: a, y: b) -> bool { ... }
✓ fn(x: int) => result[int, error] { ... }
✓ (x : int)  (* type ascription *)
✓ let identity = fn[a](x: a) -> a { x }
```

### Typechecker Tests
```marmoset
✓ Annotation matches inference: let x: int = 5
✓ Annotation conflicts with inference: let x: int = "hello" (ERROR)
✓ Generic params parsed (no semantic checking yet)
✓ Constraints parsed (error if constraint name doesn't exist — optional for Phase 2)
✓ Non-annotated code still works: fn add(x, y) { x + y }
✓ Mix of annotated and non-annotated: fn(x: int, y) { x + y }
```

### Regression Tests
```
✓ All Phase 1 examples (fibonacci, hash indexing, etc.) still work
✓ Polymorphic functions without annotations still work
✓ Recursive functions work
✓ Higher-order functions (as much as currently works)
```

---

## 4. Example Test Cases

### test_phase2_annotations.ml

```ocaml
(* Test basic annotation *)
let%test "annotation matches inference" =
  match Typecheck.Checker.check_program_with_annotations 
    (Typecheck.Builtins.prelude_env ())
    (Parser.parse_string "let x: int = 5") 
  with
  | Error _ -> false
  | Ok _ -> true

(* Test annotation mismatch *)
let%test "annotation mismatch detected" =
  match Typecheck.Checker.check_program_with_annotations
    (Typecheck.Builtins.prelude_env ())
    (Parser.parse_string "let x: string = 5")
  with
  | Error _ -> true  (* Should error *)
  | Ok _ -> false

(* Test generic function *)
let%test "generic function parses" =
  match Parser.parse_string "fn[a](x: a) -> a { x }"
  with
  | Error _ -> false
  | Ok (Function { generics = Some [{ name = "a"; constraints = [] }]; ... }) -> true
  | Ok _ -> false

(* Test trait constraint *)
let%test "trait constraint parses" =
  match Parser.parse_string "fn[a: show](x: a) -> unit { ... }"
  with
  | Error _ -> false
  | Ok (Function { generics = Some [{ name = "a"; constraints = ["show"] }]; ... }) -> true
  | Ok _ -> false

(* Test mixed annotated/non-annotated *)
let%test "mixed annotations work" =
  match Typecheck.Checker.check_program_with_annotations
    (Typecheck.Builtins.prelude_env ())
    (Parser.parse_string "fn(x: int, y) { x + y }")
  with
  | Error _ -> false
  | Ok _ -> true

(* Test non-annotated code still works *)
let%test "phase 1 code still works" =
  match Typecheck.Checker.check_program_with_annotations
    (Typecheck.Builtins.prelude_env ())
    (Parser.parse_string "let id = fn(x) { x }; id(5); id(\"hello\")")
  with
  | Error _ -> false
  | Ok _ -> true
```

---

## 5. Files to Create/Modify

### Create
- `lib/frontend/typecheck/annotation.ml` — annotation handling

### Modify
- `lib/frontend/syntax/lexer.ml` — add tokens
- `lib/frontend/syntax/parser.ml` — add annotation parsing
- `lib/frontend/syntax/ast.ml` — add annotation nodes
- `lib/frontend/typecheck/infer.ml` — add check_expression
- `lib/frontend/typecheck/checker.ml` — integrate annotations
- `lib/frontend/typecheck/dune` — add annotation module to build

### No Changes
- `lib/backend/go/emitter.ml` — already works with typed environment
- `bin/main.ml` — minimal changes, mostly wiring

---

## 6. Success Criteria

Phase 2 is complete when:

1. ✓ Parser accepts all annotation syntax without errors
2. ✓ AST stores annotations correctly
3. ✓ Type annotations are checked against inferred types
4. ✓ Mismatches generate helpful error messages
5. ✓ Constraint syntax parses (even if not enforced)
6. ✓ All Phase 1 examples still compile and run
7. ✓ New annotated examples compile and run:
   - `fn max[a: ord](x: a, y: a) -> a { ... }`
   - `fn print[a: show](x: a) -> unit { ... }`
   - `fn[a](x: a) -> a { x }`
8. ✓ Type errors are clear and actionable

---

## 7. Known Limitations (Intentional)

- Trait constraints are **parsed but not enforced** (Phase 3 work)
- Effect annotations (`=>`) are **parsed but ignored** (Phase 5 work)
- Union types are **parsed but error in Phase 2** (Phase 3 work)
- No sophisticated bidirectional checking yet (Phase 2.5/3 work)
- Generic impl not yet supported (Phase 3)

---

## 8. Follow-up: Phase 2.5 (Optional)

If time permits, before Phase 3:
- Implement full bidirectional type checking
- Better error messages for constrained generics
- Type inference guided by annotations (flow-sensitive)

But not required for Phase 2 to be useful.

