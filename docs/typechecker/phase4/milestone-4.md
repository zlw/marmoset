# Phase 4, Milestone 4: Records with Row Polymorphism

**Timeline**: 5-7 weeks  
**Status**: Not started  
**Dependencies**: Milestone 3 (Traits) complete  
**Last Updated**: 2026-02-04

---

## Overview

Records provide static, structurally-typed product types with named fields. Row polymorphism enables generic functions over records with partial field requirements.

### Goals
1. **Named product types**: `type point = { x: int, y: int }`
2. **Anonymous records**: `{ x: 10, y: 20 }` without predefined type
3. **Structural typing**: Same fields = same type
4. **Immutable updates**: `{ ...r, x: 5 }` creates new record
5. **Row polymorphism**: Functions over "any record with at least field x"
6. **Derive support**: `derive eq, show for point`

### Non-Goals (Deferred)
- Optional fields with `?` syntax (use `option[T]` instead)
- Mutable records (immutable with codegen optimization)
- Let destructuring (use match instead)

---

## Design Decisions Summary

| Feature | Decision |
|---------|----------|
| Definition syntax | `type point = { x: int, y: int }` |
| Anonymous records | Yes, allowed inline |
| Type equality | Structural (same fields = same type) |
| Mutability | Immutable (surface), optimize in codegen |
| Field access | Dot notation: `r.x` |
| Record update | Spread: `{ ...r, x: 5 }` |
| Spread semantics | Last write wins on conflicts |
| Row variable syntax | `...r` in types: `{ x: int, ...r }` |
| Row poly style | Explicit row variables in function signatures |
| Punning | Ruby style: `{ x:, y: }` |
| Empty records | Allowed: `{ }` (maps to Go empty struct) |
| Record vs Hash | Identifier key = record, string key = hash |
| Optional fields | Use `option[T]` for now (defer `?` syntax) |
| Derive | Yes, works for records (eq, show, ord, hash) |
| Pattern matching | In match expressions only (not let destructuring) |

---

## Syntax Specification

### Type Definition

```marmoset
# Named record type (type alias to anonymous record)
type point = { x: int, y: int }

type person = { 
    name: string, 
    age: int,
    email: option[string]
}

# Generic record type
type pair[a, b] = { first: a, second: b }

# Record with row variable (open record)
type has_x[r] = { x: int, ...r }

# Empty record (unit-like)
type unit = { }
```

### Record Literals

```marmoset
# Create record
let p = { x: 10, y: 20 }

# With type annotation
let p: point = { x: 10, y: 20 }

# Punning (Ruby-style) - variable name matches field name
let x = 10
let y = 20
let p = { x:, y: }  # Same as { x: x, y: y }

# Empty record
let u = { }
```

### Field Access

```marmoset
let p = { x: 10, y: 20 }

p.x     # 10
p.y     # 20
```

### Record Update (Spread)

```marmoset
let p = { x: 10, y: 20 }

# Update field (creates new record)
let p2 = { ...p, x: 5 }      # { x: 5, y: 20 }

# Add field
let p3 = { ...p, z: 30 }     # { x: 10, y: 20, z: 30 }

# Merge records (last wins on conflict)
let a = { x: 1, y: 2 }
let b = { y: 10, z: 3 }
let c = { ...a, ...b }       # { x: 1, y: 10, z: 3 }

# Order matters for conflicts
{ ...base, x: 5 }   # x is 5 (explicit field last, wins)
{ x: 5, ...base }   # x is base.x (spread last, wins)
```

### Row Polymorphism

```marmoset
# Function that works on any record with at least x: int
fn get_x[r](rec: { x: int, ...r }) -> int {
    rec.x
}

get_x({ x: 5 })                    # Works: r = {}
get_x({ x: 5, y: 10 })             # Works: r = { y: int }
get_x({ x: 5, y: 10, z: "hi" })    # Works: r = { y: int, z: string }

# Multiple required fields
fn magnitude[r](rec: { x: int, y: int, ...r }) -> int {
    rec.x * rec.x + rec.y * rec.y
}

# Preserving extra fields in return type
fn with_z[r](rec: { x: int, y: int, ...r }) -> { x: int, y: int, z: int, ...r } {
    { ...rec, z: rec.x + rec.y }
}

let p = { x: 1, y: 2, name: "point" }
let p2 = with_z(p)
# p2 = { x: 1, y: 2, z: 3, name: "point" } -- name preserved!

# Add field to any record
fn with_id[r](rec: { ...r }, id: int) -> { id: int, ...r } {
    { ...rec, id: id }
}
```

### Pattern Matching

```marmoset
let p = { x: 10, y: 20, z: 30 }

# Destructure in match
match p {
    { x:, y:, z: }: x + y + z    # Punning in pattern
}

# Partial match (row poly)
fn process[r](rec: { x: int, ...r }) -> string {
    match rec {
        { x: 0, ...rest }: "zero"
        { x:, ...rest }: "non-zero: " + show(x)
    }
}

# Nested patterns
type nested = { point: { x: int, y: int }, name: string }

match n {
    { point: { x:, y: }, name: }: show(x) + ", " + show(y) + ": " + name
}
```

### Derive

```marmoset
type point = { x: int, y: int }
derive eq, show, ord, hash for point

# Generic record derive
type pair[a, b] = { first: a, second: b }
derive eq[a: eq, b: eq], show[a: show, b: show] for pair[a, b]

# Usage
let p1 = { x: 1, y: 2 }
let p2 = { x: 1, y: 2 }
p1 == p2     # true
show(p1)     # "{ x: 1, y: 2 }"
```

---

## Record vs Hash Distinction

Records and hashes are distinguished by key syntax:

```marmoset
# RECORD - identifier keys (static, compile-time)
let point = { x: 10, y: 20 }
point.x          # Field access
type point = { x: int, y: int }

# HASH - string/expression keys (dynamic, runtime)
let h = { "x": 10, "y": 20 }
h["x"]           # Key lookup
type point_hash = hash[string, int]

# Can't mix
let invalid = { x: 10, "y": 20 }  # Error!
```

**Rule**: All keys must be either identifiers (record) or expressions (hash).

---

## Current State Analysis

### What Already Exists

1. **Hash literals** (`lib/frontend/syntax/parser.ml`):
   - `{ "key": value }` parsing exists for hashes
   - Need to extend for identifier keys

2. **Dot notation** - exists for method calls, need to add field access

3. **Type system** (`lib/frontend/typecheck/types.ml`):
   - Has `THash` for hash types
   - Need to add `TRecord` and `TRowVar`

### What's Missing

1. ❌ `type` keyword for type aliases (check if exists)
2. ❌ `...` spread token in lexer
3. ❌ Record type in AST and type system
4. ❌ Row variable representation
5. ❌ Record literal parsing (with identifier keys)
6. ❌ Punning syntax `{ x:, y: }`
7. ❌ Spread syntax `{ ...r, x: 5 }`
8. ❌ Field access `r.x` for records
9. ❌ Row unification
10. ❌ Record pattern matching
11. ❌ Record codegen to Go structs

---

## Detailed Implementation Plan

### Step 4.1: Lexer - Add Spread Token

**Duration**: 0.5 days  
**Files**: `lib/frontend/syntax/token.ml`, `lib/frontend/syntax/lexer.ml`

```ocaml
(* token.ml - add to token_type *)
| Spread  (* ... for record spread/row variables *)

(* lexer.ml - add case for ... *)
| '.' ->
    if peek_char l = '.' && peek_char (read_char l) = '.' then
      (read_char (read_char (read_char l)), Token.init ~pos Spread "...")
    else
      (* existing dot handling if any *)
      (read_char l, Token.init ~pos Dot ".")
```

**Note**: Check if `.` is already a token (for field access). If not, add `Dot` too.

---

### Step 4.2: Lexer - Add Type Keyword (if missing)

**Duration**: 0.25 days  
**Files**: `lib/frontend/syntax/token.ml`, `lib/frontend/syntax/lexer.ml`

```ocaml
(* token.ml *)
| Type  (* type keyword for type aliases *)

(* lexer.ml lookup_ident *)
| "type" -> Type
```

---

### Step 4.3: AST - Record Types and Expressions

**Duration**: 1 day  
**Files**: `lib/frontend/syntax/ast.ml`

```ocaml
(* Type expressions - add record and row types *)
type type_expr =
  | ...existing...
  | TRecord of record_type_field list * type_expr option  
    (* { x: int, y: string, ...r } - fields + optional row variable *)

and record_type_field = {
  field_name : string;
  field_type : type_expr;
}

(* Expressions - add record literal and field access *)
type expr_kind =
  | ...existing...
  | RecordLit of record_field list * expression option
    (* { x: 1, y: 2, ...base } - fields + optional spread *)
  | FieldAccess of expression * string
    (* expr.field_name *)

and record_field = {
  name : string;
  value : expression option;  (* None = punning, use variable with same name *)
}

(* Patterns - add record pattern *)
type pattern_kind =
  | ...existing...
  | PRecord of record_pattern_field list * string option
    (* { x:, y:, ...rest } - fields + optional rest variable *)

and record_pattern_field = {
  field_name : string;
  pattern : pattern option;  (* None = punning *)
}

(* Top-level declarations - add type alias *)
type declaration =
  | ...existing...
  | DTypeAlias of type_alias

and type_alias = {
  name : string;
  type_params : string list;
  body : type_expr;
}
```

---

### Step 4.4: Parser - Type Aliases

**Duration**: 1 day  
**Files**: `lib/frontend/syntax/parser.ml`

Parse: `type name[params] = type_expr`

```ocaml
and parse_type_alias (p : parser) : (parser * AST.type_alias, parser) result =
  (* Expect: type *)
  let* p2 = expect_curr p Token.Type in
  let p2 = next_token p2 in
  (* Expect: identifier *)
  let* p3, name = expect_ident p2 in
  (* Optional: [a, b, c] type parameters *)
  let* p4, type_params = 
    if curr_token_is p3 Token.LBracket then
      parse_type_param_list (next_token p3)
    else
      Ok (p3, [])
  in
  (* Expect: = *)
  let* p5 = expect_peek p4 Token.Assign in
  let p5 = next_token p5 in
  (* Parse type expression *)
  let* p6, body = parse_type_expr p5 in
  Ok (p6, { name; type_params; body })
```

---

### Step 4.5: Parser - Record Type Expressions

**Duration**: 1 day  
**Files**: `lib/frontend/syntax/parser.ml`

Parse: `{ x: int, y: string, ...r }`

```ocaml
and parse_record_type (p : parser) : (parser * AST.type_expr, parser) result =
  (* Expect: { *)
  let* p2 = expect_curr p Token.LBrace in
  let p2 = next_token p2 in
  (* Parse fields and optional row variable *)
  let rec parse_fields lp fields =
    if curr_token_is lp Token.RBrace then
      Ok (next_token lp, AST.TRecord (List.rev fields, None))
    else if curr_token_is lp Token.Spread then
      (* ...r - row variable *)
      let lp2 = next_token lp in
      let* lp3, row_var = parse_type_expr lp2 in
      let* lp4 = expect_curr lp3 Token.RBrace in
      Ok (next_token lp4, AST.TRecord (List.rev fields, Some row_var))
    else
      (* field_name: type *)
      let* lp2, field_name = expect_ident lp in
      let* lp3 = expect_peek lp2 Token.Colon in
      let lp3 = next_token lp3 in
      let* lp4, field_type = parse_type_expr lp3 in
      let field = { AST.field_name; field_type } in
      if curr_token_is lp4 Token.Comma then
        parse_fields (next_token lp4) (field :: fields)
      else
        parse_fields lp4 (field :: fields)
  in
  parse_fields p2 []
```

---

### Step 4.6: Parser - Record Literals

**Duration**: 1-2 days  
**Files**: `lib/frontend/syntax/parser.ml`

Parse: `{ x: 1, y: 2, ...base }` and punning `{ x:, y: }`

```ocaml
and parse_record_or_hash_literal (p : parser) : (parser * AST.expression, parser) result =
  (* We're at { *)
  let p2 = next_token p in
  if curr_token_is p2 Token.RBrace then
    (* Empty record { } *)
    Ok (next_token p2, mk_expr p.pos (AST.RecordLit ([], None)))
  else if curr_token_is p2 Token.String then
    (* String key = hash literal *)
    parse_hash_literal p
  else if curr_token_is p2 Token.Ident then
    (* Identifier = check if record or hash with identifier key *)
    let ident = p2.curr_token.literal in
    let p3 = next_token p2 in
    if curr_token_is p3 Token.Colon then
      let p4 = next_token p3 in
      if curr_token_is p4 Token.Comma || curr_token_is p4 Token.RBrace then
        (* Punning: { x:, } or { x: } *)
        parse_record_literal_from p ident None
      else
        (* { x: expr, ... } - record with value *)
        let* p5, value = parse_expression p4 Lowest in
        parse_record_literal_from p5 ident (Some value)
    else
      (* Just identifier, might be different syntax *)
      Error (unexpected_token p3)
  else if curr_token_is p2 Token.Spread then
    (* { ...base } or { ...base, x: 1 } *)
    parse_record_with_spread p2
  else
    Error (unexpected_token p2)
```

---

### Step 4.7: Parser - Field Access

**Duration**: 0.5 days  
**Files**: `lib/frontend/syntax/parser.ml`

Parse: `expr.field`

```ocaml
(* Add to infix parsing *)
| Token.Dot ->
    let p2 = next_token p in
    let* p3, field_name = expect_ident p2 in
    Ok (p3, mk_expr left.pos (AST.FieldAccess (left, field_name)))
```

---

### Step 4.8: Parser - Record Patterns

**Duration**: 1 day  
**Files**: `lib/frontend/syntax/parser.ml`

Parse: `{ x:, y: z, ...rest }`

---

### Step 4.9: Type System - Record and Row Types

**Duration**: 2 days  
**Files**: `lib/frontend/typecheck/types.ml`

```ocaml
type mono_type =
  | ...existing...
  | TRecord of record_field_type list * mono_type option
    (* { x: int, y: string } or { x: int, ...r } *)
  | TRowVar of string
    (* Row type variable *)

and record_field_type = {
  name : string;
  typ : mono_type;
}

(* Helper: create closed record type *)
let make_record fields = TRecord (fields, None)

(* Helper: create open record type with row variable *)
let make_open_record fields row_var = TRecord (fields, Some (TRowVar row_var))

(* to_string for records *)
let rec to_string = function
  | ...
  | TRecord (fields, row) ->
      let field_strs = List.map (fun f -> f.name ^ ": " ^ to_string f.typ) fields in
      let row_str = match row with
        | None -> ""
        | Some r -> ", ..." ^ to_string r
      in
      "{ " ^ String.concat ", " field_strs ^ row_str ^ " }"
  | TRowVar name -> name
```

---

### Step 4.10: Unification - Row Unification

**Duration**: 3-4 days  
**Files**: `lib/frontend/typecheck/unify.ml`

Row unification is the core of row polymorphism:

```ocaml
(* Unify two record types *)
let rec unify_records fields1 row1 fields2 row2 =
  (* 1. Find common fields - unify their types *)
  (* 2. Fields only in fields1 go to row2's extension *)
  (* 3. Fields only in fields2 go to row1's extension *)
  (* 4. Unify remaining rows *)
  
  let common, only1, only2 = partition_fields fields1 fields2 in
  
  (* Unify common fields *)
  let* subst1 = unify_field_list common in
  
  match (row1, row2) with
  | (None, None) ->
      (* Both closed - only1 and only2 must be empty *)
      if only1 = [] && only2 = [] then Ok subst1
      else Error (FieldMismatch (only1, only2))
  
  | (Some (TRowVar r1), None) ->
      (* r1 must equal the extra fields from fields2 *)
      let* subst2 = bind_row_var r1 only2 in
      compose_subst subst1 subst2
  
  | (None, Some (TRowVar r2)) ->
      (* r2 must equal the extra fields from fields1 *)
      let* subst2 = bind_row_var r2 only1 in
      compose_subst subst1 subst2
  
  | (Some (TRowVar r1), Some (TRowVar r2)) ->
      (* Both open - create fresh row for shared unknown part *)
      let fresh_row = fresh_row_var () in
      let* subst2 = bind_row_var r1 (only2 @ [fresh_row]) in
      let* subst3 = bind_row_var r2 (only1 @ [fresh_row]) in
      compose_subst subst1 (compose_subst subst2 subst3)
  
  | _ ->
      (* Other row types - recurse *)
      ...
```

---

### Step 4.11: Inference - Record Expressions

**Duration**: 2 days  
**Files**: `lib/frontend/typecheck/infer.ml`

```ocaml
(* Infer record literal *)
| AST.RecordLit (fields, spread) ->
    (* Infer type of each field *)
    let* env', field_types = infer_record_fields env fields in
    
    (* Handle spread if present *)
    let* env'', result_type = match spread with
      | None -> Ok (env', TRecord (field_types, None))
      | Some base_expr ->
          let* env'', base_type = infer env' base_expr in
          (* base_type must be a record, merge fields *)
          let* merged = merge_with_spread field_types base_type in
          Ok (env'', merged)
    in
    Ok (env'', result_type)

(* Infer field access *)
| AST.FieldAccess (expr, field_name) ->
    let* env', expr_type = infer env expr in
    (* expr_type must be record with field_name *)
    let* field_type = extract_field expr_type field_name in
    Ok (env', field_type)
```

---

### Step 4.12: Annotation - Record Type Conversion

**Duration**: 1 day  
**Files**: `lib/frontend/typecheck/annotation.ml`

Convert `AST.TRecord` to `Types.TRecord`.

---

### Step 4.13: Derive - Record Derivations

**Duration**: 1 day  
**Files**: `lib/frontend/typecheck/derive.ml`

Extend derive to handle records:

```ocaml
let derive_eq_for_record (record_type : type_alias) : impl_def =
  (* Generate: fn eq(a: T, b: T) -> bool { a.x == b.x && a.y == b.y && ... } *)
  ...

let derive_show_for_record (record_type : type_alias) : impl_def =
  (* Generate: fn show(r: T) -> string { "{ x: " + show(r.x) + ", ... }" } *)
  ...
```

---

### Step 4.14: Codegen - Records to Go Structs

**Duration**: 2-3 days  
**Files**: `lib/backend/go/emitter.ml`

```ocaml
(* Named record type -> Go struct *)
let emit_record_type_def (alias : type_alias) : string =
  let fields = extract_record_fields alias.body in
  let go_fields = List.map (fun f ->
    Printf.sprintf "\t%s %s" (capitalize f.name) (emit_type f.typ)
  ) fields in
  Printf.sprintf "type %s struct {\n%s\n}\n"
    (mangle_type_name alias.name)
    (String.concat "\n" go_fields)

(* Record literal -> Go struct literal *)
let emit_record_lit (fields : record_field list) (spread : expression option) : string =
  match spread with
  | None ->
      let field_strs = List.map (fun f ->
        Printf.sprintf "%s: %s" (capitalize f.name) (emit_expr f.value)
      ) fields in
      Printf.sprintf "struct{%s}{%s}" 
        (emit_anon_struct_type fields)
        (String.concat ", " field_strs)
  | Some base ->
      (* Spread: create new struct, copy base, override fields *)
      emit_spread_record base fields

(* Field access -> Go field access *)
let emit_field_access (expr : string) (field : string) : string =
  Printf.sprintf "%s.%s" expr (capitalize field)

(* Spread semantics: last write wins *)
let emit_spread_record (base : expression) (overrides : record_field list) : string =
  (* For immutable semantics, create a new struct with all fields *)
  (* Copy from base, then apply overrides *)
  ...
```

---

### Step 4.15: Integration Tests

**Duration**: 2 days

```bash
echo ""
echo "-- PHASE 4.4: RECORDS & ROW POLYMORPHISM TESTS --"

# Basic records
test_case "record literal" \
    'let p = { x: 10, y: 20 }; p.x' \
    '10'

test_case "record field access" \
    'let p = { x: 10, y: 20 }; p.x + p.y' \
    '30'

# Type alias
test_case "type alias" \
    'type point = { x: int, y: int }
     let p: point = { x: 1, y: 2 }
     p.x' \
    '1'

# Punning
test_case "punning" \
    'let x = 5
     let y = 10
     let p = { x:, y: }
     p.x + p.y' \
    '15'

# Empty record
test_case "empty record" \
    'let u = { }; 42' \
    '42'

# Spread update
test_case "spread update" \
    'let p = { x: 1, y: 2 }
     let p2 = { ...p, x: 10 }
     p2.x + p2.y' \
    '12'

# Spread merge
test_case "spread merge" \
    'let a = { x: 1 }
     let b = { y: 2 }
     let c = { ...a, ...b }
     c.x + c.y' \
    '3'

# Last write wins
test_case "spread last wins" \
    'let a = { x: 1, y: 2 }
     let b = { ...a, x: 10 }
     b.x' \
    '10'

# Row polymorphism - basic
test_case "row poly get field" \
    'fn get_x[r](rec: { x: int, ...r }) -> int { rec.x }
     get_x({ x: 5, y: 10 })' \
    '5'

# Row poly - extra fields preserved
test_case "row poly extra fields" \
    'fn get_x[r](rec: { x: int, ...r }) -> int { rec.x }
     get_x({ x: 5, y: 10, z: 20 })' \
    '5'

# Row poly - add field
test_case "row poly add field" \
    'fn with_z[r](rec: { x: int, y: int, ...r }) -> { x: int, y: int, z: int, ...r } {
       { ...rec, z: rec.x + rec.y }
     }
     let p = with_z({ x: 1, y: 2 })
     p.z' \
    '3'

# Record pattern matching
test_case "record pattern" \
    'let p = { x: 10, y: 20 }
     match p {
       { x:, y: }: x + y
     }' \
    '30'

# Derive eq
test_case "derive eq for record" \
    'type point = { x: int, y: int }
     derive eq for point
     let p1: point = { x: 1, y: 2 }
     let p2: point = { x: 1, y: 2 }
     p1 == p2' \
    'true'

# Derive show
test_case "derive show for record" \
    'type point = { x: int, y: int }
     derive show for point
     let p: point = { x: 1, y: 2 }
     show(p)' \
    '"{ x: 1, y: 2 }"'

# Record vs Hash distinction
test_case "hash with string key" \
    'let h = { "x": 10, "y": 20 }
     h["x"]' \
    '10'

# Structural equality
test_case "structural type equality" \
    'type point = { x: int, y: int }
     type vec2d = { x: int, y: int }
     let p: point = { x: 1, y: 2 }
     let v: vec2d = p
     v.x' \
    '1'
```

---

## Summary Timeline

| Step | Duration | Cumulative |
|------|----------|------------|
| 4.1 Lexer - Spread token | 0.5 days | 0.5 days |
| 4.2 Lexer - Type keyword | 0.25 days | 0.75 days |
| 4.3 AST nodes | 1 day | 1.75 days |
| 4.4 Parser - Type aliases | 1 day | 2.75 days |
| 4.5 Parser - Record types | 1 day | 3.75 days |
| 4.6 Parser - Record literals | 1.5 days | 5.25 days |
| 4.7 Parser - Field access | 0.5 days | 5.75 days |
| 4.8 Parser - Record patterns | 1 day | 6.75 days |
| 4.9 Types - Record/Row types | 2 days | 8.75 days |
| 4.10 Unification - Row unification | 4 days | 12.75 days |
| 4.11 Inference - Records | 2 days | 14.75 days |
| 4.12 Annotation conversion | 1 day | 15.75 days |
| 4.13 Derive for records | 1 day | 16.75 days |
| 4.14 Codegen | 3 days | 19.75 days |
| 4.15 Tests | 2 days | 21.75 days |

**Total: ~5-6 weeks**

---

## Checklist

### Lexer/Parser
- [ ] `...` (Spread) token
- [ ] `.` (Dot) token for field access
- [ ] `type` keyword (if not exists)
- [ ] Type alias parsing
- [ ] Record type expression parsing
- [ ] Row variable in type (`...r`)
- [ ] Record literal parsing
- [ ] Punning syntax `{ x:, y: }`
- [ ] Spread in literals `{ ...base, x: 5 }`
- [ ] Field access parsing `r.x`
- [ ] Record pattern parsing

### Type System
- [ ] `TRecord` type constructor
- [ ] `TRowVar` for row variables
- [ ] Record field type representation
- [ ] Row unification algorithm
- [ ] Structural type equality
- [ ] Record subtyping via rows

### Inference
- [ ] Record literal inference
- [ ] Field access inference
- [ ] Spread expression inference
- [ ] Row variable instantiation
- [ ] Row variable generalization
- [ ] Type alias resolution

### Derive
- [ ] `eq` for records
- [ ] `show` for records
- [ ] `ord` for records
- [ ] `hash` for records
- [ ] Generic record derivation

### Codegen
- [ ] Record type → Go struct
- [ ] Record literal → Go struct literal
- [ ] Anonymous records → Go anonymous struct
- [ ] Field access → Go field access
- [ ] Spread → struct copy with updates

### Tests
- [ ] All integration tests pass
- [ ] Row polymorphism tests
- [ ] Derive tests for records
- [ ] Error case tests

---

## Open Questions / Future Work

1. **Structural vs Nominal**: Currently structural. Consider adding `newtype` for nominal wrappers?

2. **Optional fields**: Deferred `?` syntax. Currently use `option[T]`.

3. **Let destructuring**: Deferred. Use `match` for now.

4. **Efficient codegen**: Currently copies on spread. Future: escape analysis for in-place mutation.

5. **Field ordering**: Do fields have a canonical order? (Affects `show`, `eq`, etc.)

6. **Recursive records**: `type node = { value: int, next: option[node] }` - should work but verify.

---

**Last Updated**: 2026-02-04  
**Author**: Claude Code + User collaboration
