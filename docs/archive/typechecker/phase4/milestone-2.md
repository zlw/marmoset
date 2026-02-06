# Phase 4, Milestone 2: Enums & Pattern Matching

**Timeline**: 3-4 weeks  
**Status**: Not started  
**Dependencies**: Milestone 1 (Union Types) complete  
**Last Updated**: 2026-02-03

---

## Overview

Enums (algebraic data types) and pattern matching enable:

- **Null safety** via `option[a]` with `some(value)` / `none`
- **Error handling** via `result[a, e]` with `success(value)` / `failure(error)`
- **Domain modeling** with custom tagged unions
- **Exhaustive case analysis** enforced by compiler
- **Destructuring** to extract values safely

**Example**:
```marmoset
enum option[a] {
    some(a)
    none
}

fn safe_divide(a: int, b: int) -> option[int] {
    if (b == 0) {
        option.none
    } else {
        option.some(a / b)
    }
}

let result = safe_divide(10, 2)
let value = match result {
    option.some(v): v
    option.none: 0
}
```

**Deliverable**: User-defined enums with pattern matching work end-to-end.

---

## Design Decisions Summary

| Feature | Decision |
|---------|----------|
| Enum syntax | Rust-style: `enum name[a] { variant1(a), variant2 }` |
| Match keyword | `match` |
| Match arm syntax | `pattern: expr` (no case keyword) |
| Arm separator | Newlines (no commas) |
| Match delimiters | `match x { ... }` with braces |
| Exhaustiveness | Compile-time error if non-exhaustive |
| Wildcard pattern | `_` |
| Constructor calls | Namespaced: `option.some(42)`, `result.failure(e)` |
| Variable binding | Implicit in patterns |
| Nested patterns | Supported |
| Multi-pattern arms | Supported: `pattern1 \| pattern2: expr` |
| Literal patterns | Supported: `42: ...`, `"hello": ...` |
| Match as expression | Yes, returns value |
| Block bodies | Allowed: `pattern: { ... }` |
| Enum scope | Top-level only |
| Empty enums | Allowed (uninhabited type) |
| Let destructuring | Not in scope (use match) |
| Guards | Deferred to later milestone |
| Tuples | Deferred to later milestone |
| Builtin types | `option[a]`, `result[a, e]` in prelude |
| option variants | `option.some(a)`, `option.none` |
| result variants | `result.success(a)`, `result.failure(e)` |
| Codegen strategy | Tagged union structs |

---

## Syntax Specification

### Enum Definition

```marmoset
// Simple enum (no data)
enum direction {
    north
    south
    east
    west
}

// Enum with data
enum option[a] {
    some(a)
    none
}

// Enum with multiple type parameters
enum result[a, e] {
    success(a)
    failure(e)
}

// Enum with multiple data fields per variant
enum http_response {
    ok(int, string)           // status code, body
    redirect(string)          // new url
    error(int, string)        // code, message
}

// Empty enum (uninhabited type, useful for "never")
enum never { }
```

**Grammar**:
```
enum_definition ::= "enum" identifier generic_params? "{" variant_list "}"
generic_params  ::= "[" identifier ("," identifier)* "]"
variant_list    ::= (variant newline)*
variant         ::= identifier variant_data?
variant_data    ::= "(" type_expr ("," type_expr)* ")"
```

### Constructor Calls

Constructors are namespaced under the enum name:

```marmoset
let x = option.some(42)
let y = option.none
let r = result.success("data")
let e = result.failure("connection failed")

// With multiple fields
let resp = http_response.ok(200, "Hello")
```

**Rationale**: Namespacing avoids collisions between enums with same variant names (e.g., user-defined `response.success` vs builtin `result.success`).

### Pattern Matching

```marmoset
// Basic match
match x {
    option.some(v): v + 1
    option.none: 0
}

// Match is an expression
let doubled = match opt {
    option.some(v): v * 2
    option.none: 0
}

// Block body allowed
match opt {
    option.some(v): {
        let x = v + 1
        let y = x * 2
        y
    }
    option.none: 0
}

// Wildcard pattern
match direction {
    direction.north: "up"
    _: "other"
}

// Multi-pattern arms
match opt {
    option.none | option.some(0): "zero or nothing"
    option.some(v): v
}

// Nested patterns
match nested_result {
    result.success(option.some(v)): v
    result.success(option.none): 0
    result.failure(e): -1
}

// Literal patterns
match n {
    0: "zero"
    1: "one"
    2: "two"
    _: "many"
}

// String literal patterns
match command {
    "quit": exit()
    "help": show_help()
    _: unknown_command()
}
```

**Grammar**:
```
match_expr    ::= "match" expression "{" match_arms "}"
match_arms    ::= (match_arm newline)*
match_arm     ::= pattern_list ":" expression
pattern_list  ::= pattern ("|" pattern)*
pattern       ::= literal_pattern 
                | wildcard_pattern 
                | variable_pattern
                | constructor_pattern
literal_pattern      ::= integer | string | "true" | "false"
wildcard_pattern     ::= "_"
variable_pattern     ::= identifier
constructor_pattern  ::= identifier "." identifier ("(" pattern_args ")")?
pattern_args         ::= pattern ("," pattern)*
```

---

## Builtin Types

### option[a]

Represents an optional value - either present (`some`) or absent (`none`).

```marmoset
// Definition (implicit in prelude)
enum option[a] {
    some(a)
    none
}

// Usage
let x: option[int] = option.some(42)
let y: option[int] = option.none

// Pattern matching
match x {
    option.some(v): v + 1
    option.none: 0
}
```

**Replaces**: Raw `null` returns from array indexing, hash access, etc.

### result[a, e]

Represents a computation that can succeed or fail.

```marmoset
// Definition (implicit in prelude)
enum result[a, e] {
    success(a)
    failure(e)
}

// Usage
fn divide(a: int, b: int) -> result[int, string] {
    if (b == 0) {
        result.failure("division by zero")
    } else {
        result.success(a / b)
    }
}

// Pattern matching
match divide(10, 2) {
    result.success(v): puts(v)
    result.failure(e): puts("Error: " + e)
}
```

---

## Type System Integration

### Enum Type Representation

Add to `Types.mono_type`:

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
  | TUnion of mono_type list      (* From Milestone 1 *)
  | TEnum of string * mono_type list  (* NEW: name, type args *)
```

**Examples**:
- `option[int]` → `TEnum ("option", [TInt])`
- `result[string, int]` → `TEnum ("result", [TString; TInt])`
- `direction` → `TEnum ("direction", [])`

### Enum Registry

Track enum definitions in a global registry:

```ocaml
type variant_def = {
    name: string;
    fields: mono_type list;  (* empty for nullary variants *)
}

type enum_def = {
    name: string;
    type_params: string list;
    variants: variant_def list;
}

(* Global registry *)
type enum_registry = (string, enum_def) Hashtbl.t
```

### Constructor Typing

Each constructor is a function that returns the enum type:

```ocaml
(* option.some : ∀a. a -> option[a] *)
(* option.none : ∀a. option[a] *)
(* result.success : ∀a e. a -> result[a, e] *)
(* result.failure : ∀a e. e -> result[a, e] *)
```

For multi-field variants:
```ocaml
(* http_response.ok : (int, string) -> http_response *)
```

### Pattern Type Checking

Pattern matching must:
1. **Check exhaustiveness**: All variants covered
2. **Bind variables**: Extract types from variant fields
3. **Unify patterns**: Pattern type must match scrutinee type
4. **Unify arms**: All arm bodies must have compatible types

---

## Current State Analysis

### What Already Exists

1. **Union types** (Milestone 1):
   - `TUnion` in type system
   - Type narrowing with `typeof`
   - Codegen to `interface{}` + type switches

2. **Keywords available**:
   - Need to add: `enum`, `match`

3. **AST infrastructure**:
   - Expression/statement patterns established
   - Type annotation system working

### What's Missing

1. ❌ **No `enum` keyword** in lexer/token
2. ❌ **No `match` keyword** in lexer/token
3. ❌ **No enum definition parsing**
4. ❌ **No match expression parsing**
5. ❌ **No pattern parsing**
6. ❌ **No `TEnum` in type system**
7. ❌ **No enum registry**
8. ❌ **No constructor resolution**
9. ❌ **No pattern type checking**
10. ❌ **No exhaustiveness checking**
11. ❌ **No codegen for enums**
12. ❌ **No codegen for match**

---

## Codegen Strategy

### Enum Representation in Go

Each enum becomes a tagged union struct:

**Marmoset**:
```marmoset
enum option[a] {
    some(a)
    none
}
```

**Generated Go** (for `option[int]`):
```go
type Option_int struct {
    Tag  int8   // 0 = none, 1 = some
    Data int64  // only valid when Tag = 1
}

// Constants for tags
const (
    Option_int_none = 0
    Option_int_some = 1
)

// Constructors
func Option_int_Some(v int64) Option_int {
    return Option_int{Tag: Option_int_some, Data: v}
}

func Option_int_None() Option_int {
    return Option_int{Tag: Option_int_none}
}
```

**For polymorphic usage** (type unknown at compile time):
```go
type Option_any struct {
    Tag  int8
    Data interface{}  // boxed
}
```

### Multi-Field Variants

**Marmoset**:
```marmoset
enum http_response {
    ok(int, string)
    error(int, string)
}
```

**Generated Go**:
```go
type HttpResponse struct {
    Tag   int8
    Data0 interface{}  // first field
    Data1 interface{}  // second field
}

// Or with specialized struct:
type HttpResponse struct {
    Tag    int8
    Int0   int64   // first int field
    Str0   string  // first string field
}
```

### Match Expression Codegen

**Marmoset**:
```marmoset
match opt {
    option.some(v): v + 1
    option.none: 0
}
```

**Generated Go**:
```go
func match_result() int64 {
    switch opt.Tag {
    case Option_int_some:
        v := opt.Data
        return v + 1
    case Option_int_none:
        return 0
    }
    panic("non-exhaustive match")  // should never reach if typechecked
}
```

### Nested Pattern Codegen

**Marmoset**:
```marmoset
match r {
    result.success(option.some(v)): v
    result.success(option.none): 0
    result.failure(e): -1
}
```

**Generated Go**:
```go
switch r.Tag {
case Result_success:
    inner := r.Data.(Option_int)
    switch inner.Tag {
    case Option_int_some:
        v := inner.Data
        return v
    case Option_int_none:
        return 0
    }
case Result_failure:
    e := r.Data.(string)
    return -1
}
```

### Literal Pattern Codegen

**Marmoset**:
```marmoset
match n {
    0: "zero"
    1: "one"
    _: "other"
}
```

**Generated Go**:
```go
switch n {
case 0:
    return "zero"
case 1:
    return "one"
default:
    return "other"
}
```

---

## Detailed Implementation Plan

### Step 2.1: Lexer - Add Keywords

**Duration**: 0.5 days  
**Files**: `lib/frontend/syntax/token.ml`, `lib/frontend/syntax/lexer.ml`

#### Test (RED)

Add to `test/test_typecheck_and_codegen.sh`:

```bash
echo ""
echo "-- PHASE 4.2: ENUM & PATTERN MATCHING TESTS --"
test_case "Lex enum keyword" \
    'enum option[a] { some(a) none }' \
    "false"  # Will fail on parsing, but lexer should work
```

#### Implementation

**File**: `lib/frontend/syntax/token.ml`

Add after line 46 (`Return`):

```ocaml
  | Enum    (* enum keyword *)
  | Match   (* match keyword *)
```

Update `lookup_ident` function:

```ocaml
let lookup_ident s =
  match s with
  | "fn" -> Function
  | "let" -> Let
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | "enum" -> Enum      (* NEW *)
  | "match" -> Match    (* NEW *)
  | _ -> Ident
```

#### Verification

Build: `dune build`

---

### Step 2.2: AST - Enum and Match Nodes

**Duration**: 1 day  
**Files**: `lib/frontend/syntax/ast.ml`

#### Implementation

**File**: `lib/frontend/syntax/ast.ml`

Add variant definition type (after `type_expr` around line 11):

```ocaml
  and variant_def = {
    variant_name: string;
    variant_fields: type_expr list;
  }
  [@@deriving show]
```

Add enum definition to `stmt_kind` (after `Block`):

```ocaml
    | EnumDef of {
        name: string;
        type_params: string list;
        variants: variant_def list;
      }
```

Add pattern type (after `expression` type):

```ocaml
  and pattern = {
    pat: pattern_kind;
    pos: int;
  }

  and pattern_kind =
    | PWildcard                              (* _ *)
    | PVariable of string                    (* x *)
    | PLiteral of literal_value              (* 42, "hello", true *)
    | PConstructor of string * string * pattern list  
        (* enum_name, variant_name, field patterns *)
        (* e.g., option.some(x) -> ("option", "some", [PVariable "x"]) *)
  [@@deriving show]

  and literal_value =
    | LInt of int64
    | LString of string
    | LBool of bool
  [@@deriving show]
```

Add match arm type:

```ocaml
  and match_arm = {
    patterns: pattern list;  (* multiple patterns for | syntax *)
    body: expression;
  }
  [@@deriving show]
```

Add match expression to `expr_kind`:

```ocaml
    | Match of expression * match_arm list
        (* match scrutinee { arm1, arm2, ... } *)
```

Add constructor call to `expr_kind`:

```ocaml
    | EnumConstructor of string * string * expression list
        (* enum_name, variant_name, arguments *)
        (* e.g., option.some(42) -> ("option", "some", [Integer 42]) *)
```

#### Verification

Build: `dune build`

---

### Step 2.3: Parser - Enum Definition

**Duration**: 2 days  
**Files**: `lib/frontend/syntax/parser.ml`

#### Test (RED)

```bash
test_case "Parse simple enum" \
    'enum direction { north south east west }
     let x = direction.north' \
    "true"

test_case "Parse generic enum" \
    'enum option[a] { some(a) none }
     let x = option.some(42)' \
    "true"
```

#### Implementation

**File**: `lib/frontend/syntax/parser.ml`

Add `parse_enum_definition` function:

```ocaml
and parse_enum_definition (p : parser) : (parser * AST.statement, parser) result =
  (* Current token is 'enum' *)
  let pos = p.curr_token.pos in
  let* p2 = expect_peek p Token.Ident in
  let name = p2.curr_token.literal in
  let p3 = next_token p2 in
  
  (* Parse optional type parameters: [a, b] *)
  let* p4, type_params = 
    if curr_token_is p3 Token.LBracket then
      parse_type_param_list (next_token p3)
    else
      Ok (p3, [])
  in
  
  (* Expect opening brace *)
  let* p5 = expect_peek p4 Token.LBrace in
  
  (* Parse variants *)
  let* p6, variants = parse_variant_list (next_token p5) in
  
  (* Expect closing brace *)
  let* p7 = 
    if curr_token_is p6 Token.RBrace then
      Ok p6
    else
      expect_peek p6 Token.RBrace
  in
  
  Ok (next_token p7, mk_stmt pos (AST.EnumDef { name; type_params; variants }))

and parse_type_param_list (p : parser) : (parser * string list, parser) result =
  let rec loop lp params =
    if curr_token_is lp Token.Ident then
      let param = lp.curr_token.literal in
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Comma then
        loop (next_token lp2) (params @ [param])
      else if curr_token_is lp2 Token.RBracket then
        Ok (next_token lp2, params @ [param])
      else
        Error (peek_error lp2 Token.RBracket)
    else if curr_token_is lp Token.RBracket then
      Ok (next_token lp, params)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_variant_list (p : parser) : (parser * AST.variant_def list, parser) result =
  let rec loop lp variants =
    if curr_token_is lp Token.RBrace then
      Ok (lp, List.rev variants)
    else if curr_token_is lp Token.Ident then
      let variant_name = lp.curr_token.literal in
      let lp2 = next_token lp in
      (* Check for variant data: some(a) *)
      let* lp3, variant_fields =
        if curr_token_is lp2 Token.LParen then
          let* lp3, fields = parse_type_expr_list (next_token lp2) in
          let* lp4 = expect_peek lp3 Token.RParen in
          Ok (next_token lp4, fields)
        else
          Ok (lp2, [])
      in
      let variant = AST.{ variant_name; variant_fields } in
      loop lp3 (variant :: variants)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []
```

Update `parse_statement` to handle enum:

```ocaml
and parse_statement (p : parser) : (parser * AST.statement, parser) result =
  match p.curr_token.token_type with
  | Token.Let -> parse_let_statement p
  | Token.Return -> parse_return_statement p
  | Token.Enum -> parse_enum_definition p  (* NEW *)
  | _ -> parse_expression_statement p
```

#### Verification

Run: `dune runtest`

---

### Step 2.4: Parser - Enum Constructor Calls

**Duration**: 1 day  
**Files**: `lib/frontend/syntax/parser.ml`

#### Test (RED)

```bash
test_case "Call enum constructor" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     let y = option.none' \
    "true"
```

#### Implementation

Handle `identifier.identifier(args)` syntax in expression parsing.

Find `parse_call_expression` or identifier parsing. When we see `identifier.identifier`, check if first identifier is an enum name:

```ocaml
(* In parse_identifier or similar *)
and parse_identifier_or_constructor (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let first_ident = p.curr_token.literal in
  let p2 = next_token p in
  
  (* Check for enum constructor: option.some *)
  if curr_token_is p2 Token.Dot then
    let* p3 = expect_peek p2 Token.Ident in
    let variant_name = p3.curr_token.literal in
    let p4 = next_token p3 in
    
    (* Check for arguments: option.some(42) *)
    if curr_token_is p4 Token.LParen then
      let* p5, args = parse_expression_list (next_token p4) Token.RParen in
      Ok (p5, mk_expr pos (AST.EnumConstructor (first_ident, variant_name, args)))
    else
      (* Nullary constructor: option.none *)
      Ok (p4, mk_expr pos (AST.EnumConstructor (first_ident, variant_name, [])))
  else
    (* Regular identifier *)
    Ok (p2, mk_expr pos (AST.Identifier first_ident))
```

**Note**: Need to ensure this doesn't break method call syntax if added later.

#### Verification

Run: `dune runtest`

---

### Step 2.5: Parser - Match Expression

**Duration**: 2 days  
**Files**: `lib/frontend/syntax/parser.ml`

#### Test (RED)

```bash
test_case "Parse match expression" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     match x {
         option.some(v): v
         option.none: 0
     }' \
    "true"

test_case "Parse match with wildcard" \
    'match 5 {
         0: "zero"
         _: "other"
     }' \
    "true"

test_case "Parse match with multi-pattern" \
    'match 5 {
         0 | 1: "small"
         _: "big"
     }' \
    "true"
```

#### Implementation

```ocaml
and parse_match_expression (p : parser) : (parser * AST.expression, parser) result =
  (* Current token is 'match' *)
  let pos = p.curr_token.pos in
  let p2 = next_token p in
  
  (* Parse scrutinee expression *)
  let* p3, scrutinee = parse_expression p2 prec_lowest in
  
  (* Expect opening brace *)
  let* p4 = expect_peek p3 Token.LBrace in
  
  (* Parse match arms *)
  let* p5, arms = parse_match_arms (next_token p4) in
  
  (* Expect closing brace *)
  let* p6 =
    if curr_token_is p5 Token.RBrace then
      Ok p5
    else
      expect_peek p5 Token.RBrace
  in
  
  Ok (next_token p6, mk_expr pos (AST.Match (scrutinee, arms)))

and parse_match_arms (p : parser) : (parser * AST.match_arm list, parser) result =
  let rec loop lp arms =
    if curr_token_is lp Token.RBrace then
      Ok (lp, List.rev arms)
    else
      let* lp2, arm = parse_match_arm lp in
      loop lp2 (arm :: arms)
  in
  loop p []

and parse_match_arm (p : parser) : (parser * AST.match_arm, parser) result =
  (* Parse patterns (possibly multiple with |) *)
  let* p2, patterns = parse_pattern_list p in
  
  (* Expect colon *)
  let* p3 = expect_peek p2 Token.Colon in
  
  (* Parse body expression *)
  let* p4, body = parse_expression (next_token p3) prec_lowest in
  
  Ok (p4, AST.{ patterns; body })

and parse_pattern_list (p : parser) : (parser * AST.pattern list, parser) result =
  let* p2, first = parse_pattern p in
  
  let rec loop lp patterns =
    if curr_token_is lp Token.Pipe then
      let* lp2, pat = parse_pattern (next_token lp) in
      loop lp2 (patterns @ [pat])
    else
      Ok (lp, patterns)
  in
  loop p2 [first]

and parse_pattern (p : parser) : (parser * AST.pattern, parser) result =
  let pos = p.curr_token.pos in
  match p.curr_token.token_type with
  (* Wildcard: _ *)
  | Token.Ident when p.curr_token.literal = "_" ->
      Ok (next_token p, AST.{ pat = AST.PWildcard; pos })
  
  (* Integer literal *)
  | Token.Int ->
      let value = Int64.of_string p.curr_token.literal in
      Ok (next_token p, AST.{ pat = AST.PLiteral (AST.LInt value); pos })
  
  (* String literal *)
  | Token.String ->
      let value = p.curr_token.literal in
      Ok (next_token p, AST.{ pat = AST.PLiteral (AST.LString value); pos })
  
  (* Boolean literals *)
  | Token.True ->
      Ok (next_token p, AST.{ pat = AST.PLiteral (AST.LBool true); pos })
  | Token.False ->
      Ok (next_token p, AST.{ pat = AST.PLiteral (AST.LBool false); pos })
  
  (* Identifier: variable binding or constructor *)
  | Token.Ident ->
      let first_ident = p.curr_token.literal in
      let p2 = next_token p in
      
      (* Check for constructor: enum_name.variant *)
      if curr_token_is p2 Token.Dot then
        let* p3 = expect_peek p2 Token.Ident in
        let variant_name = p3.curr_token.literal in
        let p4 = next_token p3 in
        
        (* Check for nested patterns: option.some(x) *)
        if curr_token_is p4 Token.LParen then
          let* p5, field_patterns = parse_pattern_list_parens (next_token p4) in
          Ok (p5, AST.{ pat = AST.PConstructor (first_ident, variant_name, field_patterns); pos })
        else
          (* Nullary constructor pattern *)
          Ok (p4, AST.{ pat = AST.PConstructor (first_ident, variant_name, []); pos })
      else
        (* Variable binding *)
        Ok (p2, AST.{ pat = AST.PVariable first_ident; pos })
  
  | _ ->
      Error (no_prefix_parse_fn_error p p.curr_token.token_type)

and parse_pattern_list_parens (p : parser) : (parser * AST.pattern list, parser) result =
  if curr_token_is p Token.RParen then
    Ok (next_token p, [])
  else
    let rec loop lp patterns =
      let* lp2, pat = parse_pattern lp in
      let new_patterns = patterns @ [pat] in
      if curr_token_is lp2 Token.Comma then
        loop (next_token lp2) new_patterns
      else if curr_token_is lp2 Token.RParen then
        Ok (next_token lp2, new_patterns)
      else
        Error (peek_error lp2 Token.RParen)
    in
    loop p []
```

Register `match` as prefix parse function:

```ocaml
(* In prefix parse function dispatch *)
| Token.Match -> parse_match_expression p
```

#### Verification

Run: `dune runtest`

---

### Step 2.6: Type System - Add TEnum

**Duration**: 1 day  
**Files**: `lib/frontend/typecheck/types.ml`

#### Implementation

**File**: `lib/frontend/typecheck/types.ml`

Add to `mono_type`:

```ocaml
  | TEnum of string * mono_type list  (* enum name, type arguments *)
```

Update `to_string`:

```ocaml
  | TEnum (name, []) -> name
  | TEnum (name, args) ->
      name ^ "[" ^ String.concat ", " (List.map to_string args) ^ "]"
```

Update `apply_substitution`:

```ocaml
  | TEnum (name, args) ->
      TEnum (name, List.map (apply_substitution subst) args)
```

Update `free_type_vars`:

```ocaml
  | TEnum (_, args) ->
      List.fold_left
        (fun acc t -> TypeVarSet.union acc (free_type_vars t))
        TypeVarSet.empty args
```

Update `collect_vars_in_order`:

```ocaml
  | TEnum (_, args) ->
      List.concat_map collect_vars_in_order args
```

#### Verification

Add tests:

```ocaml
let%test "to_string enum no args" =
  to_string (TEnum ("direction", [])) = "direction"

let%test "to_string enum with args" =
  to_string (TEnum ("option", [TInt])) = "option[Int]"

let%test "to_string result" =
  to_string (TEnum ("result", [TString; TInt])) = "result[String, Int]"
```

Run: `dune runtest`

---

### Step 2.7: Enum Registry

**Duration**: 1 day  
**Files**: `lib/frontend/typecheck/enum_registry.ml` (new file)

#### Implementation

Create new file:

```ocaml
(* Enum Registry - tracks defined enums and their variants *)

open Types

type variant_def = {
  name: string;
  fields: mono_type list;
}

type enum_def = {
  name: string;
  type_params: string list;
  variants: variant_def list;
}

(* Global mutable registry *)
let registry : (string, enum_def) Hashtbl.t = Hashtbl.create 16

let clear () = Hashtbl.clear registry

let register (def : enum_def) : unit =
  Hashtbl.replace registry def.name def

let lookup (name : string) : enum_def option =
  Hashtbl.find_opt registry name

let lookup_variant (enum_name : string) (variant_name : string) 
    : variant_def option =
  match lookup enum_name with
  | None -> None
  | Some def ->
      List.find_opt (fun v -> v.name = variant_name) def.variants

(* Get constructor type for a variant *)
let variant_type (enum_name : string) (variant_name : string) 
    (type_args : mono_type list) : mono_type option =
  match lookup enum_name with
  | None -> None
  | Some def ->
      match List.find_opt (fun v -> v.name = variant_name) def.variants with
      | None -> None
      | Some variant ->
          (* Create substitution from type params to type args *)
          let subst = List.combine def.type_params 
            (List.map (fun t -> t) type_args) in
          let subst_map = List.map (fun (p, t) -> (p, t)) subst in
          
          (* Substitute in variant field types *)
          let result_type = TEnum (enum_name, type_args) in
          
          if variant.fields = [] then
            (* Nullary: just the enum type *)
            Some result_type
          else
            (* Function from fields to enum *)
            let field_types = List.map 
              (apply_substitution subst_map) variant.fields in
            let fn_type = List.fold_right 
              (fun field ret -> TFun (field, ret)) 
              field_types result_type in
            Some fn_type

(* Register builtins *)
let init_builtins () =
  (* option[a] = some(a) | none *)
  register {
    name = "option";
    type_params = ["a"];
    variants = [
      { name = "some"; fields = [TVar "a"] };
      { name = "none"; fields = [] };
    ];
  };
  
  (* result[a, e] = success(a) | failure(e) *)
  register {
    name = "result";
    type_params = ["a"; "e"];
    variants = [
      { name = "success"; fields = [TVar "a"] };
      { name = "failure"; fields = [TVar "e"] };
    ];
  }
```

Update dune file to include new module.

#### Verification

Run: `dune build`

---

### Step 2.8: Type Inference - Enum Definitions

**Duration**: 1 day  
**Files**: `lib/frontend/typecheck/infer.ml`

#### Implementation

Add handling for `EnumDef` statement:

```ocaml
| AST.EnumDef { name; type_params; variants } ->
    (* Register the enum *)
    let variant_defs = List.map (fun v ->
      let field_types = List.map Annotation.type_expr_to_mono_type 
        v.AST.variant_fields in
      Enum_registry.{ name = v.AST.variant_name; fields = field_types }
    ) variants in
    
    Enum_registry.register {
      Enum_registry.name = name;
      type_params = type_params;
      variants = variant_defs;
    };
    
    Ok (env, TNull)  (* Enum def doesn't have a value *)
```

#### Verification

Test that enum definitions are registered correctly.

---

### Step 2.9: Type Inference - Constructor Calls

**Duration**: 1-2 days  
**Files**: `lib/frontend/typecheck/infer.ml`

#### Test (RED)

```bash
test_case "Constructor returns enum type" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     x' \
    "true"
```

#### Implementation

Add handling for `EnumConstructor` expression:

```ocaml
| AST.EnumConstructor (enum_name, variant_name, args) ->
    (* Look up enum and variant *)
    (match Enum_registry.lookup_variant enum_name variant_name with
     | None -> 
         Error (Printf.sprintf "Unknown constructor: %s.%s" 
                  enum_name variant_name)
     | Some variant ->
         (* Infer types of arguments *)
         let* subst, arg_types = infer_expr_list env args in
         
         (* Check argument count matches *)
         if List.length args <> List.length variant.fields then
           Error (Printf.sprintf "%s.%s expects %d arguments, got %d"
                    enum_name variant_name 
                    (List.length variant.fields) (List.length args))
         else
           (* Unify argument types with expected field types *)
           (* Need to instantiate type parameters *)
           let enum_def = Enum_registry.lookup enum_name |> Option.get in
           let fresh_vars = List.map (fun _ -> fresh_type_var ()) 
             enum_def.type_params in
           let param_subst = List.combine enum_def.type_params fresh_vars in
           
           let expected_types = List.map 
             (apply_substitution param_subst) variant.fields in
           
           let* subst2 = unify_many arg_types expected_types in
           let final_subst = compose_substitution subst subst2 in
           
           let result_type = TEnum (enum_name, 
             List.map (apply_substitution final_subst) fresh_vars) in
           
           Ok (final_subst, result_type))
```

#### Verification

Run integration tests.

---

### Step 2.10: Type Inference - Match Expression

**Duration**: 2-3 days  
**Files**: `lib/frontend/typecheck/infer.ml`

#### Test (RED)

```bash
test_case "Match infers correct type" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     let y = match x {
         option.some(v): v + 1
         option.none: 0
     }
     y' \
    "true"
```

#### Implementation

```ocaml
| AST.Match (scrutinee, arms) ->
    (* Infer scrutinee type *)
    let* subst, scrutinee_type = infer env scrutinee in
    let env' = apply_subst_to_env subst env in
    
    (* Check each arm and collect body types *)
    let* subst', arm_types = infer_match_arms env' scrutinee_type arms in
    let final_subst = compose_substitution subst subst' in
    
    (* All arm bodies must have same type *)
    (match arm_types with
     | [] -> Error "Match expression must have at least one arm"
     | first :: rest ->
         let* unify_subst = 
           List.fold_left (fun acc t ->
             let* s = acc in
             let first' = apply_substitution s first in
             let t' = apply_substitution s t in
             let* s2 = Unify.unify first' t' in
             Ok (compose_substitution s s2)
           ) (Ok empty_substitution) rest
         in
         let result_type = apply_substitution unify_subst first in
         Ok (compose_substitution final_subst unify_subst, result_type))

and infer_match_arms env scrutinee_type arms =
  let rec loop subst env types = function
    | [] -> Ok (subst, List.rev types)
    | arm :: rest ->
        let* subst', body_type = infer_match_arm env scrutinee_type arm in
        let new_subst = compose_substitution subst subst' in
        let env' = apply_subst_to_env subst' env in
        loop new_subst env' (body_type :: types) rest
  in
  loop empty_substitution env [] arms

and infer_match_arm env scrutinee_type arm =
  (* For each pattern, check it matches scrutinee and get bindings *)
  let* bindings = check_patterns arm.patterns scrutinee_type in
  
  (* Extend environment with pattern bindings *)
  let env' = List.fold_left 
    (fun e (name, ty) -> Env.extend e name (mono_to_poly ty)) 
    env bindings in
  
  (* Infer body type *)
  infer env' arm.body

and check_patterns patterns scrutinee_type =
  (* Check first pattern, others must match same scrutinee *)
  match patterns with
  | [] -> Error "Match arm must have at least one pattern"
  | first :: rest ->
      let* bindings = check_pattern first scrutinee_type in
      (* Verify rest match same type *)
      let* _ = List.fold_left (fun acc pat ->
        let* _ = acc in
        check_pattern pat scrutinee_type
      ) (Ok bindings) rest in
      Ok bindings

and check_pattern pattern scrutinee_type =
  match pattern.pat with
  | AST.PWildcard -> Ok []
  
  | AST.PVariable name -> 
      Ok [(name, scrutinee_type)]
  
  | AST.PLiteral lit ->
      let lit_type = match lit with
        | AST.LInt _ -> TInt
        | AST.LString _ -> TString
        | AST.LBool _ -> TBool
      in
      let* _ = Unify.unify lit_type scrutinee_type in
      Ok []
  
  | AST.PConstructor (enum_name, variant_name, field_patterns) ->
      (* Check scrutinee is the right enum type *)
      (match scrutinee_type with
       | TEnum (sname, type_args) when sname = enum_name ->
           (* Look up variant *)
           (match Enum_registry.lookup_variant enum_name variant_name with
            | None -> Error (Printf.sprintf "Unknown variant: %s.%s" 
                               enum_name variant_name)
            | Some variant ->
                if List.length field_patterns <> List.length variant.fields then
                  Error (Printf.sprintf "Pattern %s.%s expects %d fields, got %d"
                           enum_name variant_name
                           (List.length variant.fields) 
                           (List.length field_patterns))
                else
                  (* Get field types with type args substituted *)
                  let enum_def = Enum_registry.lookup enum_name |> Option.get in
                  let subst = List.combine enum_def.type_params type_args in
                  let field_types = List.map 
                    (apply_substitution subst) variant.fields in
                  
                  (* Check each field pattern and collect bindings *)
                  let* bindings_list = 
                    List.map2 check_pattern field_patterns field_types 
                    |> List.fold_left (fun acc r ->
                         let* bs = acc in
                         let* b = r in
                         Ok (bs @ b)
                       ) (Ok [])
                  in
                  Ok bindings_list)
       | _ -> 
           Error (Printf.sprintf "Pattern %s.%s doesn't match scrutinee type %s"
                    enum_name variant_name (Types.to_string scrutinee_type)))
```

#### Verification

Run integration tests.

---

### Step 2.11: Exhaustiveness Checking

**Duration**: 2 days  
**Files**: `lib/frontend/typecheck/exhaustiveness.ml` (new file)

#### Test (RED)

```bash
test_case "Non-exhaustive match is error" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     match x {
         option.some(v): v
     }' \
    "false" \
    "Non-exhaustive"

test_case "Exhaustive match passes" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     match x {
         option.some(v): v
         option.none: 0
     }' \
    "true"
```

#### Implementation

Create new file:

```ocaml
(* Exhaustiveness checking for pattern matching *)

open Types
open Syntax.Ast.AST

(* Check if patterns cover all cases of a type *)
let check_exhaustive (scrutinee_type : mono_type) (arms : match_arm list) 
    : (unit, string) result =
  
  (* Collect all patterns *)
  let all_patterns = List.concat_map (fun arm -> arm.patterns) arms in
  
  (* Check if there's a wildcard or variable pattern (catches all) *)
  let has_catchall = List.exists (fun p ->
    match p.pat with
    | PWildcard | PVariable _ -> true
    | _ -> false
  ) all_patterns in
  
  if has_catchall then
    Ok ()
  else
    match scrutinee_type with
    | TEnum (enum_name, _) ->
        (* Get all variants *)
        (match Enum_registry.lookup enum_name with
         | None -> Error (Printf.sprintf "Unknown enum: %s" enum_name)
         | Some def ->
             (* Collect covered variants *)
             let covered = List.filter_map (fun p ->
               match p.pat with
               | PConstructor (_, vname, _) -> Some vname
               | _ -> None
             ) all_patterns in
             
             (* Find uncovered variants *)
             let uncovered = List.filter (fun v ->
               not (List.mem v.Enum_registry.name covered)
             ) def.variants in
             
             if uncovered = [] then
               Ok ()
             else
               let names = List.map (fun v -> 
                 enum_name ^ "." ^ v.Enum_registry.name
               ) uncovered in
               Error (Printf.sprintf "Non-exhaustive match. Missing: %s"
                        (String.concat ", " names)))
    
    | TInt | TString ->
        (* For primitives, need wildcard unless all literals covered *)
        Error "Non-exhaustive match on primitive type. Add wildcard pattern."
    
    | TBool ->
        (* Check if true and false both covered *)
        let has_true = List.exists (fun p ->
          match p.pat with PLiteral (LBool true) -> true | _ -> false
        ) all_patterns in
        let has_false = List.exists (fun p ->
          match p.pat with PLiteral (LBool false) -> true | _ -> false
        ) all_patterns in
        if has_true && has_false then Ok ()
        else Error "Non-exhaustive match on bool. Missing true or false."
    
    | _ ->
        Error "Non-exhaustive match. Add wildcard pattern."
```

Integrate into type checker after match inference.

#### Verification

Run integration tests.

---

### Step 2.12: Codegen - Enum Types

**Duration**: 2 days  
**Files**: `lib/backend/go/emitter.ml`

#### Implementation

Generate Go struct for each enum instantiation:

```ocaml
let emit_enum_type (name : string) (type_args : Types.mono_type list) : string =
  let go_name = mangle_enum_name name type_args in
  let variants = Enum_registry.lookup name |> Option.get in
  
  (* Generate struct *)
  Printf.sprintf "type %s struct {\n\tTag int8\n\tData interface{}\n}\n\n" 
    go_name ^
  
  (* Generate tag constants *)
  String.concat "\n" (List.mapi (fun i v ->
    Printf.sprintf "const %s_%s = %d" go_name v.Enum_registry.name i
  ) variants.variants) ^
  "\n\n" ^
  
  (* Generate constructors *)
  String.concat "\n" (List.map (fun v ->
    emit_enum_constructor go_name v type_args
  ) variants.variants)

let emit_enum_constructor go_name variant type_args =
  let fn_name = Printf.sprintf "%s_%s" go_name variant.Enum_registry.name in
  if variant.fields = [] then
    Printf.sprintf "func %s() %s {\n\treturn %s{Tag: %s_%s}\n}\n"
      fn_name go_name go_name go_name variant.name
  else
    let params = List.mapi (fun i t ->
      Printf.sprintf "v%d %s" i (mono_type_to_go_type t)
    ) variant.fields |> String.concat ", " in
    Printf.sprintf "func %s(%s) %s {\n\treturn %s{Tag: %s_%s, Data: v0}\n}\n"
      fn_name params go_name go_name go_name variant.name
```

---

### Step 2.13: Codegen - Match Expression

**Duration**: 2-3 days  
**Files**: `lib/backend/go/emitter.ml`

#### Implementation

```ocaml
let emit_match scrutinee arms result_type =
  let scrutinee_code = emit_expr scrutinee in
  
  (* Determine match style based on scrutinee type *)
  match get_type scrutinee with
  | TEnum (_, _) ->
      Printf.sprintf "func() %s {\n" (mono_type_to_go_type result_type) ^
      Printf.sprintf "\tswitch %s.Tag {\n" scrutinee_code ^
      String.concat "" (List.map (emit_enum_match_arm scrutinee_code) arms) ^
      "\t}\n\tpanic(\"non-exhaustive match\")\n}()"
  
  | TInt | TString | TBool ->
      Printf.sprintf "func() %s {\n" (mono_type_to_go_type result_type) ^
      Printf.sprintf "\tswitch %s {\n" scrutinee_code ^
      String.concat "" (List.map emit_literal_match_arm arms) ^
      "\t}\n\tpanic(\"non-exhaustive match\")\n}()"
  
  | _ ->
      failwith "Unsupported match scrutinee type"

let emit_enum_match_arm scrutinee_code arm =
  (* Handle first pattern (multi-pattern generates multiple cases) *)
  String.concat "" (List.map (fun pattern ->
    match pattern.pat with
    | PConstructor (enum_name, variant_name, field_pats) ->
        let tag = Printf.sprintf "%s_%s" 
          (mangle_enum_name enum_name []) variant_name in
        Printf.sprintf "\tcase %s:\n" tag ^
        (* Extract bindings *)
        emit_pattern_bindings scrutinee_code field_pats ^
        Printf.sprintf "\t\treturn %s\n" (emit_expr arm.body)
    | PWildcard | PVariable _ ->
        Printf.sprintf "\tdefault:\n\t\treturn %s\n" (emit_expr arm.body)
    | _ -> ""
  ) arm.patterns)

let emit_pattern_bindings scrutinee field_patterns =
  List.mapi (fun i pat ->
    match pat.pat with
    | PVariable name ->
        Printf.sprintf "\t\t%s := %s.Data\n" name scrutinee
    | _ -> ""
  ) field_patterns |> String.concat ""
```

---

### Step 2.14: Integration & Polish

**Duration**: 2 days  
**Files**: Various

#### Final Integration Tests

```bash
# Full option[a] test
test_case "Option type end-to-end" \
    'enum option[a] { some(a) none }
     
     fn safe_head(arr: list[int]) -> option[int] {
         if (len(arr) > 0) {
             option.some(arr[0])
         } else {
             option.none
         }
     }
     
     let x = safe_head([1, 2, 3])
     let y = match x {
         option.some(v): v * 2
         option.none: 0
     }
     puts(y)' \
    "true"

# Full result[a, e] test
test_case "Result type end-to-end" \
    'enum result[a, e] { success(a) failure(e) }
     
     fn divide(a: int, b: int) -> result[int, string] {
         if (b == 0) {
             result.failure("division by zero")
         } else {
             result.success(a / b)
         }
     }
     
     let r = divide(10, 2)
     match r {
         result.success(v): puts(v)
         result.failure(e): puts(e)
     }' \
    "true"

# Nested patterns
test_case "Nested pattern matching" \
    'enum option[a] { some(a) none }
     enum result[a, e] { success(a) failure(e) }
     
     fn process(r: result[option[int], string]) -> int {
         match r {
             result.success(option.some(v)): v
             result.success(option.none): 0
             result.failure(e): -1
         }
     }
     
     let x = process(result.success(option.some(42)))
     puts(x)' \
    "true"

# Multi-pattern arms
test_case "Multi-pattern arm" \
    'enum option[a] { some(a) none }
     
     let x = option.some(0)
     let y = match x {
         option.none | option.some(0): "zero or none"
         option.some(v): "has value"
     }
     puts(y)' \
    "true"

# Literal patterns
test_case "Literal pattern matching" \
    'let x = 5
     let y = match x {
         0: "zero"
         1: "one"
         _: "many"
     }
     puts(y)' \
    "true"
```

---

## Completion Criteria

### Must Pass (100%)

**Unit tests**:
- [ ] All `types.ml` tests pass (TEnum)
- [ ] All `enum_registry.ml` tests pass
- [ ] All `exhaustiveness.ml` tests pass
- [ ] All existing Phase 1-3, M1 tests pass (no regressions)

**Integration tests** (at least 20 new tests):
- [ ] Parse enum definitions
- [ ] Parse match expressions
- [ ] Parse all pattern types
- [ ] Constructor calls type correctly
- [ ] Match infers correct types
- [ ] Pattern bindings work
- [ ] Nested patterns work
- [ ] Multi-pattern arms work
- [ ] Literal patterns work
- [ ] Exhaustiveness errors caught
- [ ] Exhaustive matches pass
- [ ] Builtin option works
- [ ] Builtin result works
- [ ] Compile enums to Go
- [ ] Compile match to Go
- [ ] Run compiled code correctly

### Features Working

- [ ] **Lexer**: `enum`, `match` keywords
- [ ] **Parser**: Enum definitions
- [ ] **Parser**: Match expressions
- [ ] **Parser**: All pattern types
- [ ] **AST**: Enum and match nodes
- [ ] **Type system**: `TEnum` type
- [ ] **Registry**: Enum definitions tracked
- [ ] **Inference**: Constructors typed correctly
- [ ] **Inference**: Match typed correctly
- [ ] **Patterns**: Variable bindings
- [ ] **Patterns**: Nested matching
- [ ] **Exhaustiveness**: Compile-time checking
- [ ] **Builtins**: `option[a]`, `result[a, e]`
- [ ] **Codegen**: Enum structs in Go
- [ ] **Codegen**: Match to switch
- [ ] **Runtime**: Compiled code runs

### Documentation

- [ ] Update `CLAUDE.md` with Milestone 2 status
- [ ] Update `README.md` features table
- [ ] Add doc comments to all new functions
- [ ] Update `SYNTAX_DECISIONS.md` with enum/match syntax

---

## Migration from Milestone 1

### Replace Temporary Null Safety

**Milestone 1** used `int | null` as temporary option type.

**Milestone 2** replaces with proper `option[int]`.

Migration:
1. Update array indexing to return `option[T]` instead of `T | null`
2. Update hash access similarly
3. Update `first`, `last`, `rest` builtins
4. Update tests to use pattern matching

### Typeof vs Pattern Matching

`typeof` checks (Milestone 1) are still useful for:
- Union types that aren't enums
- Quick type guards without full destructuring

Pattern matching (Milestone 2) is preferred for:
- Enums and variants
- Destructuring with variable binding
- Complex nested structures

Both coexist - they serve different purposes.

---

## Known Limitations

### Explicitly OUT OF SCOPE for Milestone 2:

1. **Guards** (`when` clauses) - deferred to later
2. **Tuple patterns** - requires tuple type first
3. **Let destructuring** - use match instead
4. **Record patterns** - requires records first
5. **Array patterns** - complex, deferred
6. **Or-patterns with different bindings** - all branches must bind same names
7. **View patterns** - not planned
8. **Active patterns** - not planned
9. **Local enum definitions** - only top-level

---

## References

**Related documents**:
- `docs/typechecker/phase4/milestone-1.md` - Union types (prerequisite)
- `docs/typechecker/plan.md` - Overall type system roadmap
- `docs/SYNTAX_DECISIONS.md` - Language syntax reference
- `CLAUDE.md` - TDD process

**Similar languages**:
- **Rust**: `enum`, `match`, exhaustiveness
- **OCaml**: Algebraic data types, pattern matching
- **Haskell**: ADTs, case expressions
- **Swift**: `enum`, `switch`, associated values
- **Gleam**: `type`, `case`, Result/Option

---

**Last Updated**: 2026-02-03  
**Author**: Claude (with human guidance)  
**Status**: Ready for implementation
