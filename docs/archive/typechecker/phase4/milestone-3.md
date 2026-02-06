# Phase 4, Milestone 3: Traits System

**Timeline**: ~5 weeks  
**Status**: Not started  
**Dependencies**: Milestone 2 (Enums & Pattern Matching) complete  
**Last Updated**: 2026-02-04

---

## Overview

Traits provide:
1. **Ad-hoc polymorphism**: Same function name, different implementations per type
2. **Type constraints**: Generic functions can require capabilities (`fn[a: show](x: a)`)
3. **Row constraints**: Traits can require fields, enabling structural constraints (`fn[t: { name: string }]`)
4. **Operator overloading**: `eq` trait enables `==` and `!=` operators
5. **Derive mechanism**: Automatic implementations for simple cases

### Non-Goals (Deferred)
- Associated types (use type parameters instead)
- Static methods / default trait
- Higher-kinded types

---

## Design Decisions Summary

| Feature | Decision |
|---------|----------|
| Trait syntax | `trait eq[a] { fn eq(x: a, y: a) -> bool }` |
| Field syntax | Yes: `trait named { name: string }` |
| Impl syntax | `impl eq for point { ... }` |
| Self convention | Haskell-style type parameter: `trait eq[a]` |
| Supertraits | Yes: `trait ord[a]: eq { ... }` |
| Multiple supertraits | `+` separator: `trait foo[a]: bar + baz` |
| Orphan rules | Allow with warnings (Haskell-style) |
| Default impls | Yes - traits can provide default method bodies |
| Conditional impls | Yes: `impl eq[a: eq] for list[a]` |
| Trait solver | Full solver for conditional impl resolution |
| Operators | Traits enable operators (eq → ==, ord → <) |
| Derive | Standalone: `derive eq, show for color` |
| Derivable traits | Builtin only: eq, show, ord, hash |
| Trait scope | Top-level only |
| Impl scope | Top-level only |
| Primitive impls | Compiler provides |
| Dispatch modes | Static (`[t: T]`) and Dynamic (`T` as type) |
| Static dispatch | Monomorphization, direct field/method access |
| Dynamic dispatch | Go interface with auto-generated getters |

---

## Builtin Traits

### Trait Definitions

```marmoset
# Equality
trait eq[a] {
    fn eq(x: a, y: a) -> bool
}

# Ordering (requires eq)
trait ord[a]: eq {
    fn compare(x: a, y: a) -> ordering
}

# String representation
trait show[a] {
    fn show(x: a) -> string
}

# Hashing
trait hash[a] {
    fn hash(x: a) -> int
}

# Arithmetic (binary ops)
trait num[a] {
    fn add(x: a, y: a) -> a
    fn sub(x: a, y: a) -> a
    fn mul(x: a, y: a) -> a
    fn div(x: a, y: a) -> a
}

# Negation (unary minus) - separate trait like Rust
trait neg[a] {
    fn neg(x: a) -> a
}

# Ordering result enum
enum ordering {
    less
    equal
    greater
}
```

### Trait Fields

Traits can require fields, not just methods. This enables using traits for both ad-hoc polymorphism (methods) and row constraints (fields).

```marmoset
# Field-only trait
trait named {
    name: string
}

# Mixed fields and methods
trait printable {
    name: string
    fn format(self) -> string
}

# Using field constraint
fn greet[t: named](x: t) -> string {
    "Hello, " + x.name
}

# Implementation must provide the field
impl named for person {
    # Field 'name' must exist in person type
}
```

**Field Access:**
- **Static dispatch** (`[t: named]`): Direct field access (monomorphized)
- **Dynamic dispatch** (`named` as type): Auto-generated getter method

### Operator Desugaring

| Operator | Desugars To |
|----------|-------------|
| `x == y` | `eq(x, y)` |
| `x != y` | `!eq(x, y)` |
| `x < y` | `compare(x, y) == ordering.less` |
| `x > y` | `compare(x, y) == ordering.greater` |
| `x <= y` | `compare(x, y) != ordering.greater` |
| `x >= y` | `compare(x, y) != ordering.less` |
| `x + y` | `add(x, y)` |
| `x - y` | `sub(x, y)` |
| `x * y` | `mul(x, y)` |
| `x / y` | `div(x, y)` |
| `-x` | `neg(x)` |

### Compiler-Provided Impls

| Type | Traits |
|------|--------|
| `int` | show, eq, ord, hash, num, neg |
| `float` | show, eq, ord, num, neg (NO hash - precision issues) |
| `bool` | show, eq, ord, hash |
| `string` | show, eq, ord, hash |
| `list[a]` | show (if a: show), eq (if a: eq), ord (if a: ord), hash (if a: hash) |
| `hash[k, v]` | show (if k: show, v: show), eq (if k: eq, v: eq) |

---

## Syntax Specification

### Trait Definition

```marmoset
# Simple trait
trait show[a] {
    fn show(x: a) -> string
}

# Trait with multiple methods
trait eq[a] {
    fn eq(x: a, y: a) -> bool
}

# Trait with supertrait
trait ord[a]: eq {
    fn compare(x: a, y: a) -> ordering
}

# Multiple supertraits
trait hashable[a]: eq + hash {
    # Requires both eq and hash
}

# Trait with default implementation
trait eq[a] {
    fn eq(x: a, y: a) -> bool
    fn neq(x: a, y: a) -> bool {
        !eq(x, y)  # default impl
    }
}
```

**Grammar**:
```
trait_def     ::= "trait" identifier type_params supertrait_clause? "{" trait_member* "}"
trait_member  ::= field_decl | method_sig
field_decl    ::= identifier ":" type_expr
type_params   ::= "[" identifier ("," identifier)* "]"
supertrait_clause ::= ":" trait_ref ("+" trait_ref)*
trait_ref     ::= identifier type_args?
type_args     ::= "[" type_expr ("," type_expr)* "]"
method_sig    ::= "fn" identifier "(" param_list ")" "->" type_expr method_body?
method_body   ::= "{" expression "}"   # default implementation
```

### Trait Implementation

```marmoset
# Basic impl
impl show for int {
    fn show(x: int) -> string {
        int_to_string(x)
    }
}

# Impl for user-defined type
impl show for color {
    fn show(x: color) -> string {
        match x {
            color.red: "red"
            color.green: "green"
            color.blue: "blue"
        }
    }
}

# Conditional impl (requires constraint on type parameter)
impl eq[a: eq] for list[a] {
    fn eq(xs: list[a], ys: list[a]) -> bool {
        if (len(xs) != len(ys)) { return false }
        # Element-wise comparison using a's eq
        ...
    }
}

impl eq[a: eq] for option[a] {
    fn eq(x: option[a], y: option[a]) -> bool {
        match (x, y) {
            (option.some(v1), option.some(v2)): eq(v1, v2)
            (option.none, option.none): true
            _: false
        }
    }
}
```

**Grammar**:
```
impl_def      ::= "impl" impl_params? trait_ref "for" type_expr "{" method_impl* "}"
impl_params   ::= "[" param_constraint ("," param_constraint)* "]"
param_constraint ::= identifier ":" trait_ref ("+" trait_ref)*
method_impl   ::= "fn" identifier "(" param_list ")" "->" type_expr "{" expression "}"
```

### Derive

```marmoset
# Derive for simple enum
derive eq, show for color

# Derive with constraints for generic type
derive eq[a: eq], show[a: show] for option[a]

# Multiple traits
derive eq, show, ord, hash for ordering
```

**Grammar**:
```
derive_def    ::= "derive" derive_trait_list "for" type_expr
derive_trait_list ::= derive_trait ("," derive_trait)*
derive_trait  ::= identifier impl_params?
```

### Using Traits

```marmoset
# Trait method called as function
show(42)              # "42"
eq(color.red, color.blue)  # false

# Operators desugared to trait methods
42 == 42              # desugars to: eq(42, 42)
"a" != "b"            # desugars to: !eq("a", "b")
5 < 10                # desugars to: compare(5, 10) == ordering.less

# Generic function with constraint
fn print[a: show](x: a) -> unit {
    puts(show(x))
}

print(42)             # works - int: show
print(color.red)      # works - color: show (if derived)

# Multiple constraints
fn debug[a: show + eq](x: a, y: a) -> string {
    if (eq(x, y)) {
        show(x) + " equals " + show(y)
    } else {
        show(x) + " differs from " + show(y)
    }
}
```

---

## Dispatch Modes

Traits support two dispatch modes based on how they are used:

### Static Dispatch (Monomorphization)

When a trait is used as a **constraint** on a type parameter:

```marmoset
fn greet[t: named](x: t) -> string {
    "Hello, " + x.name
}

greet(person)   # Generates: greet_person with direct field access
greet(company)  # Generates: greet_company with direct field access
```

**Characteristics:**
- Zero runtime overhead
- Direct field/method access
- Function duplicated per concrete type
- No vtable, no indirection

### Dynamic Dispatch (Trait Objects)

When a trait is used as a **type** directly (not as a constraint):

```marmoset
let items: list[named] = [person, company, robot]
for item in items {
    puts(item.name)  # Dynamic dispatch via getter
}
```

**Characteristics:**
- Enables heterogeneous collections
- Open/extensible (foreign types can implement)
- Field access via auto-generated getters
- Method calls via Go interface dispatch
- Small overhead (getter call, interface lookup)
- **Performance note**: Go's aggressive inliner typically optimizes trivial getters to direct field access, so dynamic dispatch often approaches static performance in practice

### Codegen Strategy

| Usage | Field Access | Go Codegen | Performance |
|-------|--------------|------------|-------------|
| `[t: named]` | `x.name` → `x.Name` | Direct struct field | Zero overhead |
| `named` as type | `x.name` → `x.GetName()` | Interface method | Getter call + interface dispatch |

**Note:** Go's compiler inlines trivial getters and eliminates unused ones via dead code elimination, minimizing overhead.

### Syntax Determines Dispatch

The distinction is purely syntactic:

```marmoset
# Static dispatch - 't' is constrained by 'named'
fn static_example[t: named](x: t) -> string { x.name }

# Dynamic dispatch - parameter type is the trait itself
fn dynamic_example(x: named) -> string { x.name }

# Heterogeneous collection requires dynamic dispatch
let mixed: list[named] = [person, company, robot]
```

---

## Current State Analysis

### What Already Exists

1. **Parser supports constraint syntax** (`lib/frontend/syntax/ast.ml`):
   ```ocaml
   and generic_param = {
     name : string;
     constraints : string list;  (* trait names like "show", "eq" *)
   }
   ```
   ✅ Parser already handles `fn[a: show + eq](x: a) -> string`

2. **Annotation module has constraint extraction** (`lib/frontend/typecheck/annotation.ml`):
   ```ocaml
   let extract_constraints (params : AST.generic_param list) : string list list =
     List.map (fun p -> p.AST.constraints) params
   ```
   ✅ Infrastructure exists but unused

3. **Operator handling exists** (`lib/frontend/typecheck/infer.ml`):
   - Operators like `==`, `<`, `+` are handled
   - Currently hardcoded, needs to desugar to trait calls

### What's Missing

1. ❌ No `trait`, `impl`, `derive` keywords in lexer
2. ❌ No AST nodes for trait/impl/derive definitions
3. ❌ No parser for trait/impl/derive syntax
4. ❌ No trait registry (storage for trait definitions and impls)
5. ❌ No trait solver (resolving "does T implement Tr?")
6. ❌ No constraint checking in type inference
7. ❌ No operator desugaring to trait methods
8. ❌ No derive code generation
9. ❌ No codegen for trait method dispatch

---

## Detailed Implementation Plan

### Step 3.1: Lexer - Add Keywords

**Duration**: 0.5 days  
**Files**: `lib/frontend/syntax/token.ml`, `lib/frontend/syntax/lexer.ml`

Add keywords:
- `trait`
- `impl`
- `derive`

```ocaml
(* token.ml - add to token_type after Return *)
| Trait
| Impl
| Derive

(* token.ml - add to lookup_ident *)
| "trait" -> Trait
| "impl" -> Impl
| "derive" -> Derive
```

**Verification**:
```bash
dune build && dune runtest
```

---

### Step 3.2: AST - Add Trait/Impl/Derive Nodes

**Duration**: 1 day  
**Files**: `lib/frontend/syntax/ast.ml`

```ocaml
(* New top-level declarations *)
type trait_def = {
  name : string;
  type_param : string;  (* The 'a' in trait eq[a] *)
  supertraits : string list;  (* trait ord[a]: eq + hash *)
  fields : field_decl list;  (* Required fields *)
  methods : method_sig list;
}

and field_decl = {
  name : string;
  typ : type_expr;
}

and method_sig = {
  name : string;
  params : (string * type_expr) list;
  return_type : type_expr;
  default_impl : expression option;
}

type impl_def = {
  type_params : generic_param list;  (* impl eq[a: eq] for list[a] *)
  trait_name : string;
  for_type : type_expr;
  methods : method_impl list;
}

and method_impl = {
  name : string;
  params : (string * type_expr option) list;
  return_type : type_expr option;
  body : expression;
}

type derive_def = {
  traits : derive_trait list;
  for_type : type_expr;
}

and derive_trait = {
  name : string;
  constraints : generic_param list;  (* derive eq[a: eq] for option[a] *)
}

(* Extend program to include these *)
type declaration =
  | DLet of string * expression
  | DEnum of enum_def
  | DTrait of trait_def
  | DImpl of impl_def
  | DDerive of derive_def

type program = declaration list
```

---

### Step 3.3: Parser - Trait Definitions

**Duration**: 1-2 days  
**Files**: `lib/frontend/syntax/parser.ml`

Parse: `trait name[a]: supertrait1 + supertrait2 { fields and methods... }`

```ocaml
(* parse_trait_def : parser -> (parser * trait_def, parser) result *)
and parse_trait_def (p : parser) : (parser * AST.trait_def, parser) result =
  (* Expect: trait *)
  let* p2 = expect_curr p Token.Trait in
  (* Expect: identifier *)
  let* p3, name = expect_ident (next_token p2) in
  (* Expect: [a] *)
  let* p4 = expect_peek p3 Token.LBracket in
  let* p5, type_param = expect_ident (next_token p4) in
  let* p6 = expect_peek p5 Token.RBracket in
  (* Optional: : supertrait + supertrait *)
  let* p7, supertraits = 
    if curr_token_is (next_token p6) Token.Colon then
      parse_supertrait_list (next_token (next_token p6))
    else
      Ok (next_token p6, [])
  in
  (* Expect: { fields and methods } *)
  let* p8 = expect_curr p7 Token.LBrace in
  let* p9, (fields, methods) = parse_trait_members (next_token p8) in
  let* p10 = expect_curr p9 Token.RBrace in
  Ok (next_token p10, { name; type_param; supertraits; fields; methods })

(* Parse trait members (fields and methods) *)
and parse_trait_members (p : parser) : (parser * (field_decl list * method_sig list), parser) result =
  let rec loop lp fields methods =
    if curr_token_is lp Token.RBrace then
      Ok (lp, (List.rev fields, List.rev methods))
    else if curr_token_is lp Token.Fn then
      (* Method signature *)
      let* lp2, method_sig = parse_method_sig lp in
      loop lp2 fields (method_sig :: methods)
    else if curr_token_is lp Token.Ident then
      (* Could be field: name: type *)
      let* lp2, field = parse_field_decl lp in
      loop lp2 (field :: fields) methods
    else
      Error (unexpected_token lp)
  in
  loop p [] []
```

---

### Step 3.4: Parser - Impl Blocks

**Duration**: 1-2 days  
**Files**: `lib/frontend/syntax/parser.ml`

Parse: `impl trait[constraints] for type { fn methods... }`

---

### Step 3.5: Parser - Derive Statements

**Duration**: 0.5 days  
**Files**: `lib/frontend/syntax/parser.ml`

Parse: `derive eq, show for type`

---

### Step 3.6: Type System - Trait Registry

**Duration**: 2 days  
**Files**: `lib/frontend/typecheck/traits.ml` (new)

```ocaml
(* Trait definitions storage *)
type trait_info = {
  def : AST.trait_def;
  methods : (string * Types.poly_type) list;
}

type impl_info = {
  def : AST.impl_def;
  constraints : (string * string list) list;  (* type var -> required traits *)
  for_type : Types.mono_type;
}

type trait_registry = {
  traits : (string, trait_info) Hashtbl.t;
  impls : impl_info list ref;
}

(* Create empty registry *)
val create : unit -> trait_registry

(* Register a trait definition *)
val register_trait : trait_registry -> AST.trait_def -> unit

(* Register an impl *)
val register_impl : trait_registry -> AST.impl_def -> unit

(* Check if a type implements a trait *)
val implements : trait_registry -> Types.mono_type -> string -> bool

(* Find the impl for a type+trait pair *)
val find_impl : trait_registry -> Types.mono_type -> string -> impl_info option

(* Get method type for a trait *)
val get_method_type : trait_registry -> string -> string -> Types.poly_type option
```

---

### Step 3.7: Trait Solver

**Duration**: 3-4 days  
**Files**: `lib/frontend/typecheck/solver.ml` (new)

The trait solver answers: **"Does type T implement trait Tr?"**

#### Data Structures

```ocaml
(* Query result *)
type solve_result =
  | Solved of impl_info
  | NotImplemented
  | Ambiguous of impl_info list

(* Solve context to prevent cycles *)
type solve_ctx = {
  registry : trait_registry;
  seen : (Types.mono_type * string) list;  (* Cycle detection *)
}
```

#### Resolution Algorithm

```ocaml
(* Main solver entry point *)
let rec solve (ctx : solve_ctx) (ty : Types.mono_type) (trait : string) : solve_result =
  (* Check for cycles *)
  if List.mem (ty, trait) ctx.seen then
    NotImplemented
  else
    let ctx' = { ctx with seen = (ty, trait) :: ctx.seen } in
    (* Find matching impls *)
    let matching = find_matching_impls ctx'.registry ty trait in
    match matching with
    | [] -> NotImplemented
    | [impl] -> 
        if check_constraints ctx' impl ty then
          Solved impl
        else
          NotImplemented
    | impls -> 
        (* Multiple matches - check for most specific *)
        match find_most_specific impls with
        | Some impl -> Solved impl
        | None -> Ambiguous impls

(* Check if impl constraints are satisfied *)
and check_constraints (ctx : solve_ctx) (impl : impl_info) (ty : Types.mono_type) : bool =
  (* Get substitution from unifying impl.for_type with ty *)
  match Unify.unify impl.for_type ty with
  | Error _ -> false
  | Ok subst ->
      (* Check each constraint *)
      List.for_all (fun (var, traits) ->
        let concrete = Types.apply_substitution subst (Types.TVar var) in
        List.for_all (fun trait -> 
          match solve ctx concrete trait with
          | Solved _ -> true
          | _ -> false
        ) traits
      ) impl.constraints

(* Also check supertraits *)
and check_supertraits (ctx : solve_ctx) (ty : Types.mono_type) (trait : string) : bool =
  match Hashtbl.find_opt ctx.registry.traits trait with
  | None -> false
  | Some info ->
      List.for_all (fun supertrait ->
        match solve ctx ty supertrait with
        | Solved _ -> true
        | _ -> false
      ) info.def.supertraits
```

#### Example Resolution

```
impl eq[a: eq] for list[a] { ... }

Query: does list[int] implement eq?

solve(ctx, list[int], eq):
  1. Find impl: impl eq[a: eq] for list[a]
  2. Unify: list[a] ~ list[int] → subst = {a → int}
  3. Check constraints: a: eq under subst
     → does int implement eq?
     → find impl eq for int (compiler builtin)
     → yes!
  4. Return Solved
```

---

### Step 3.8: Inference - Constraint Checking

**Duration**: 2 days  
**Files**: `lib/frontend/typecheck/infer.ml`

When a generic function is called, verify constraints are satisfied:

```ocaml
(* In infer_call, after unifying argument types *)
| AST.Call (func_expr, args) ->
    let* env', func_type = infer env func_expr in
    (* ... existing argument inference ... *)
    
    (* NEW: Check trait constraints *)
    let* () = check_trait_constraints env' func_expr arg_types in
    (* ... rest of inference ... *)

and check_trait_constraints env func_expr arg_types =
  match get_generic_constraints func_expr with
  | None -> Ok ()
  | Some constraints ->
      (* For each (type_var, required_traits) in constraints *)
      List.iter (fun (var, traits) ->
        let concrete_type = resolve_type_var env var in
        List.iter (fun trait ->
          match Solver.solve (solver_ctx_of_env env) concrete_type trait with
          | Solver.Solved _ -> ()
          | _ -> 
              Error (Printf.sprintf "Type %s does not implement trait %s"
                (Types.to_string concrete_type) trait)
        ) traits
      ) constraints;
      Ok ()
```

---

### Step 3.9: Inference - Operator Desugaring

**Duration**: 1 day  
**Files**: `lib/frontend/typecheck/infer.ml`

Transform operators to trait method calls during inference:

```ocaml
and infer_infix env left op right =
  let* env', left_type = infer env left in
  let* env'', right_type = infer env' right in
  
  match op with
  (* Equality operators -> eq trait *)
  | "==" ->
      (* Desugar to: eq(left, right) *)
      let* () = require_trait env'' left_type "eq" in
      let* subst = Unify.unify left_type right_type in
      Ok (subst, Types.TBool)
  
  | "!=" ->
      (* Desugar to: !eq(left, right) *)
      let* () = require_trait env'' left_type "eq" in
      let* subst = Unify.unify left_type right_type in
      Ok (subst, Types.TBool)
  
  (* Comparison operators -> ord trait *)
  | "<" | ">" | "<=" | ">=" ->
      (* Desugar to: compare(left, right) == ordering.X *)
      let* () = require_trait env'' left_type "ord" in
      let* subst = Unify.unify left_type right_type in
      Ok (subst, Types.TBool)
  
  (* Arithmetic operators -> num trait *)
  | "+" | "-" | "*" | "/" ->
      let* () = require_trait env'' left_type "num" in
      let* subst = Unify.unify left_type right_type in
      let result_type = Types.apply_substitution subst left_type in
      Ok (subst, result_type)

and infer_prefix env op operand =
  let* env', operand_type = infer env operand in
  match op with
  | "-" ->
      (* Desugar to: neg(operand) *)
      let* () = require_trait env' operand_type "neg" in
      Ok (Types.empty_substitution, operand_type)
  | "!" ->
      let* subst = Unify.unify operand_type Types.TBool in
      Ok (subst, Types.TBool)
```

---

### Step 3.10: Derive Implementation

**Duration**: 2 days  
**Files**: `lib/frontend/typecheck/derive.ml` (new)

Generate impl blocks for derivable traits:

```ocaml
(* Generate impl for a trait *)
val derive_impl : trait_registry -> string -> AST.enum_def -> AST.impl_def

(* Derive eq for an enum *)
let derive_eq (enum : AST.enum_def) : AST.impl_def =
  let eq_body = 
    (* Generate: match (x, y) { (V1, V1): true, (V2, V2): true, ..., _: false } *)
    generate_eq_match enum.variants
  in
  {
    type_params = [];  (* Or with constraints for generic enums *)
    trait_name = "eq";
    for_type = AST.TCon enum.name;
    methods = [{
      name = "eq";
      params = [("x", Some (AST.TCon enum.name)); ("y", Some (AST.TCon enum.name))];
      return_type = Some (AST.TCon "bool");
      body = eq_body;
    }];
  }

(* Derive show for an enum *)
let derive_show (enum : AST.enum_def) : AST.impl_def =
  let show_body =
    (* Generate: match x { V1: "V1", V2(a): "V2(" + show(a) + ")", ... } *)
    generate_show_match enum.name enum.variants
  in
  { ... }

(* Derive ord for an enum *)
let derive_ord (enum : AST.enum_def) : AST.impl_def =
  (* Compare by variant tag, then by fields *)
  { ... }

(* Derive hash for an enum *)
let derive_hash (enum : AST.enum_def) : AST.impl_def =
  (* Combine tag hash with field hashes *)
  { ... }
```

---

### Step 3.11: Builtin Trait Impls

**Duration**: 1 day  
**Files**: `lib/frontend/typecheck/builtins.ml`

Provide impls for primitives:

```ocaml
let register_builtin_impls (registry : Traits.trait_registry) =
  (* int impls *)
  Traits.register_builtin_impl registry "show" Types.TInt;
  Traits.register_builtin_impl registry "eq" Types.TInt;
  Traits.register_builtin_impl registry "ord" Types.TInt;
  Traits.register_builtin_impl registry "hash" Types.TInt;
  Traits.register_builtin_impl registry "num" Types.TInt;
  Traits.register_builtin_impl registry "neg" Types.TInt;
  
  (* float impls - NO hash *)
  Traits.register_builtin_impl registry "show" Types.TFloat;
  Traits.register_builtin_impl registry "eq" Types.TFloat;
  Traits.register_builtin_impl registry "ord" Types.TFloat;
  Traits.register_builtin_impl registry "num" Types.TFloat;
  Traits.register_builtin_impl registry "neg" Types.TFloat;
  
  (* bool impls - NO num, NO neg *)
  Traits.register_builtin_impl registry "show" Types.TBool;
  Traits.register_builtin_impl registry "eq" Types.TBool;
  Traits.register_builtin_impl registry "ord" Types.TBool;
  Traits.register_builtin_impl registry "hash" Types.TBool;
  
  (* string impls - NO num, NO neg *)
  Traits.register_builtin_impl registry "show" Types.TString;
  Traits.register_builtin_impl registry "eq" Types.TString;
  Traits.register_builtin_impl registry "ord" Types.TString;
  Traits.register_builtin_impl registry "hash" Types.TString;
  
  (* list[a] conditional impls *)
  Traits.register_conditional_impl registry "show" 
    (Types.TArray (Types.TVar "a")) [("a", ["show"])];
  Traits.register_conditional_impl registry "eq" 
    (Types.TArray (Types.TVar "a")) [("a", ["eq"])];
  Traits.register_conditional_impl registry "ord" 
    (Types.TArray (Types.TVar "a")) [("a", ["ord"])];
  Traits.register_conditional_impl registry "hash" 
    (Types.TArray (Types.TVar "a")) [("a", ["hash"])];
  
  (* hash[k, v] conditional impls *)
  Traits.register_conditional_impl registry "show" 
    (Types.THash (Types.TVar "k", Types.TVar "v")) 
    [("k", ["show"]); ("v", ["show"])];
  Traits.register_conditional_impl registry "eq" 
    (Types.THash (Types.TVar "k", Types.TVar "v")) 
    [("k", ["eq"]); ("v", ["eq"])];
```

---

### Step 3.12: Codegen - Static Dispatch

**Duration**: 2-3 days  
**Files**: `lib/backend/go/emitter.ml`

Monomorphize trait method calls:

```ocaml
(* Mangle trait method name *)
let mangle_trait_method (trait : string) (method_name : string) (ty : Types.mono_type) : string =
  Printf.sprintf "%s_%s_%s" method_name trait (mangle_type ty)
  (* eq_eq_int, show_show_color, add_num_int, etc. *)

(* Or simpler: just method_type *)
let mangle_method (method_name : string) (ty : Types.mono_type) : string =
  Printf.sprintf "%s_%s" method_name (mangle_type ty)
  (* eq_int, show_color, add_int, etc. *)

(* Emit trait method implementations *)
let emit_trait_impl (impl : impl_info) (concrete_type : Types.mono_type) : string =
  let go_type = emit_type concrete_type in
  let methods = List.map (fun m ->
    let mangled = mangle_method m.name concrete_type in
    Printf.sprintf "func %s(%s) %s {\n%s\n}\n"
      mangled
      (emit_params m.params go_type)
      (emit_type m.return_type)
      (emit_expr m.body)
  ) impl.methods in
  String.concat "\n" methods

(* Emit builtin impls *)
let emit_builtin_impls () : string =
  {|
func show_int(x int64) string {
    return strconv.FormatInt(x, 10)
}

func eq_int(x, y int64) bool {
    return x == y
}

func compare_int(x, y int64) Ordering {
    if x < y { return Ordering{Tag: 0} }  // less
    if x > y { return Ordering{Tag: 2} }  // greater
    return Ordering{Tag: 1}                // equal
}

func add_int(x, y int64) int64 { return x + y }
func sub_int(x, y int64) int64 { return x - y }
func mul_int(x, y int64) int64 { return x * y }
func div_int(x, y int64) int64 { return x / y }
func neg_int(x int64) int64 { return -x }

// ... similar for float, bool, string ...
|}

(* Transform operator expressions to trait calls *)
let emit_infix (left : string) (op : string) (right : string) (ty : Types.mono_type) : string =
  match op with
  | "==" -> Printf.sprintf "eq_%s(%s, %s)" (mangle_type ty) left right
  | "!=" -> Printf.sprintf "!eq_%s(%s, %s)" (mangle_type ty) left right
  | "<" -> Printf.sprintf "compare_%s(%s, %s).Tag == 0" (mangle_type ty) left right
  | ">" -> Printf.sprintf "compare_%s(%s, %s).Tag == 2" (mangle_type ty) left right
  | "<=" -> Printf.sprintf "compare_%s(%s, %s).Tag != 2" (mangle_type ty) left right
  | ">=" -> Printf.sprintf "compare_%s(%s, %s).Tag != 0" (mangle_type ty) left right
  | "+" -> Printf.sprintf "add_%s(%s, %s)" (mangle_type ty) left right
  | "-" -> Printf.sprintf "sub_%s(%s, %s)" (mangle_type ty) left right
  | "*" -> Printf.sprintf "mul_%s(%s, %s)" (mangle_type ty) left right
  | "/" -> Printf.sprintf "div_%s(%s, %s)" (mangle_type ty) left right
  | _ -> failwith ("Unknown operator: " ^ op)

let emit_prefix (op : string) (operand : string) (ty : Types.mono_type) : string =
  match op with
  | "-" -> Printf.sprintf "neg_%s(%s)" (mangle_type ty) operand
  | "!" -> Printf.sprintf "!%s" operand
  | _ -> failwith ("Unknown prefix operator: " ^ op)
```

---

### Step 3.13: Codegen - Dynamic Dispatch (Trait Objects)

**Duration**: 2-3 days  
**Files**: `lib/backend/go/emitter.ml`

Generate Go interfaces and auto-getters for dynamic dispatch:

```ocaml
(* Generate Go interface for trait *)
let emit_trait_interface (trait : trait_def) : string =
  (* Fields → getter methods *)
  let field_getters = List.map (fun f ->
    Printf.sprintf "\tGet%s() %s" 
      (capitalize f.name) 
      (emit_type f.typ)
  ) trait.fields in
  
  (* Methods → interface methods *)
  let methods = List.map (fun m ->
    Printf.sprintf "\t%s(%s) %s" 
      (capitalize m.name)
      (emit_params m.params) 
      (emit_type m.return_type)
  ) trait.methods in
  
  Printf.sprintf "type %s interface {\n%s\n}\n"
    (capitalize trait.name)
    (String.concat "\n" (field_getters @ methods))

(* Auto-generate getter for implementing type *)
let emit_field_getter (type_name : string) (field : field_decl) : string =
  Printf.sprintf "func (x %s) Get%s() %s {\n\treturn x.%s\n}\n"
    type_name
    (capitalize field.name)
    (emit_type field.typ)
    (capitalize field.name)

(* Detect if trait is used as type (not just constraint) *)
let trait_needs_interface (trait_name : string) (program : AST.program) : bool =
  (* Check if trait_name appears as a type (not in [t: trait_name]) *)
  (* This is a simplified check - real implementation would traverse AST *)
  true  (* For now, generate interfaces for all traits *)

(* Transform field access on trait-typed value *)
let emit_trait_field_access (expr : string) (field : string) (is_trait_typed : bool) : string =
  if is_trait_typed then
    (* Dynamic dispatch: x.name → x.GetName() *)
    Printf.sprintf "%s.Get%s()" expr (capitalize field)
  else
    (* Static dispatch: x.name → x.Name *)
    Printf.sprintf "%s.%s" expr (capitalize field)
```

**Example output:**

Marmoset:
```marmoset
trait named { name: string }

type person = { name: string, age: int }
impl named for person

let items: list[named] = [person1, person2]
```

Generated Go:
```go
// Trait interface
type Named interface {
	GetName() string
}

// Person struct
type Person struct {
	Name string
	Age  int64
}

// Auto-generated getter
func (x Person) GetName() string {
	return x.Name
}

// Usage in code
var items []Named = []Named{person1, person2}
// Field access: items[0].name → items[0].GetName()
```

**Optimization notes:**
- Go's compiler inlines trivial getters
- Dead code elimination removes unused getters
- Interface dispatch has minimal overhead

---

### Step 3.14: Integration Tests

**Duration**: 2 days

Add to `test/test_typecheck_and_codegen.sh`:

```bash
echo ""
echo "-- PHASE 4.3: TRAIT SYSTEM TESTS --"

# TEST 22: Basic trait impl
test_case "show for int" \
    'show(42)' \
    '"42"'

# TEST 23: Operator desugaring - equality
test_case "eq via ==" \
    '42 == 42' \
    'true'

# TEST 24: Operator desugaring - inequality
test_case "neq via !=" \
    '42 != 43' \
    'true'

# TEST 25: Comparison operators
test_case "lt via <" \
    '1 < 2' \
    'true'

test_case "gt via >" \
    '2 > 1' \
    'true'

test_case "lte via <=" \
    '2 <= 2' \
    'true'

test_case "gte via >=" \
    '2 >= 1' \
    'true'

# TEST 26: Arithmetic operators
test_case "add via +" \
    '1 + 2' \
    '3'

test_case "sub via -" \
    '5 - 3' \
    '2'

test_case "mul via *" \
    '3 * 4' \
    '12'

test_case "div via /" \
    '10 / 2' \
    '5'

# TEST 27: Negation
test_case "neg via unary -" \
    '-5' \
    '-5'

# TEST 28: Custom impl for enum
test_case "show for enum" \
    'enum color { red green blue }
     impl show for color { 
       fn show(c: color) -> string { 
         match c {
           color.red: "RED"
           color.green: "GREEN"
           color.blue: "BLUE"
         }
       } 
     }
     show(color.red)' \
    '"RED"'

# TEST 29: Generic function with constraint
test_case "generic with show constraint" \
    'fn stringify[a: show](x: a) -> string { show(x) }
     stringify(42)' \
    '"42"'

# TEST 30: Multiple constraints
test_case "multiple constraints" \
    'fn check[a: show + eq](x: a, y: a) -> string { 
       if (x == y) { show(x) } else { "different" } 
     }
     check(5, 5)' \
    '"5"'

# TEST 31: Conditional impl - list
test_case "list[int] has eq" \
    '[1, 2, 3] == [1, 2, 3]' \
    'true'

test_case "list[int] not equal" \
    '[1, 2] == [1, 3]' \
    'false'

# TEST 32: Derive eq
test_case "derive eq for enum" \
    'enum color { red green blue }
     derive eq for color
     color.red == color.red' \
    'true'

test_case "derive eq inequality" \
    'enum color { red green blue }
     derive eq for color
     color.red == color.blue' \
    'false'

# TEST 33: Derive show
test_case "derive show for enum" \
    'enum color { red green blue }
     derive show for color
     show(color.green)' \
    '"color.green"'

# TEST 34: Derive for generic enum
test_case "derive eq for option" \
    'enum option[a] { some(a) none }
     derive eq[a: eq] for option[a]
     option.some(42) == option.some(42)' \
    'true'

# TEST 35: Constraint error (should fail typecheck)
test_should_fail "constraint not satisfied" \
    'fn needs_hash[a: hash](x: a) -> int { hash(x) }
     needs_hash(3.14)' \
    "does not implement.*hash"

# TEST 36: Supertrait requirement
test_case "ord implies eq" \
    '5 < 10 && 5 == 5' \
    'true'

# TEST 37: String operations
test_case "string eq" \
    '"hello" == "hello"' \
    'true'

test_case "string ord" \
    '"a" < "b"' \
    'true'

# TEST 38: Float operations (no hash)
test_case "float eq" \
    '3.14 == 3.14' \
    'true'

test_case "float arithmetic" \
    '1.5 + 2.5' \
    '4.0'

# TEST 39: Trait with field constraint (static dispatch)
test_case "trait field static dispatch" \
    'trait named { name: string }
     fn greet[t: named](x: t) -> string { "Hello, " + x.name }
     greet({ name: "Alice" })' \
    '"Hello, Alice"'

# TEST 40: Field-only trait implementation
test_case "field only trait" \
    'trait named { name: string }
     type person = { name: string, age: int }
     impl named for person
     fn get_name[t: named](x: t) -> string { x.name }
     get_name({ name: "Bob", age: 30 })' \
    '"Bob"'

# TEST 41: Mixed field and method trait
test_case "mixed fields and methods" \
    'trait printable { 
       name: string
       fn format(self) -> string { "Name: " + self.name }
     }
     type item = { name: string, price: int }
     impl printable for item
     fn display[t: printable](x: t) -> string { format(x) }
     display({ name: "Widget", price: 10 })' \
    '"Name: Widget"'

# TEST 42: Heterogeneous collection with trait object (dynamic dispatch)
test_case "trait object heterogeneous list" \
    'trait named { name: string }
     type person = { name: string, age: int }
     type company = { name: string, employees: int }
     impl named for person
     impl named for company
     let items: list[named] = [
       { name: "Alice", age: 30 },
       { name: "ACME Corp", employees: 100 }
     ]
     items[0].name' \
    '"Alice"'
```

---

## Summary Timeline

| Step | Duration | Cumulative |
|------|----------|------------|
| 3.1 Lexer keywords | 0.5 days | 0.5 days |
| 3.2 AST nodes | 1 day | 1.5 days |
| 3.3 Parser - trait | 1.5 days | 3 days |
| 3.4 Parser - impl | 1.5 days | 4.5 days |
| 3.5 Parser - derive | 0.5 days | 5 days |
| 3.6 Trait registry | 2 days | 7 days |
| 3.7 Trait solver | 4 days | 11 days |
| 3.8 Inference - constraints | 2 days | 13 days |
| 3.9 Inference - operators | 1 day | 14 days |
| 3.10 Derive impl | 2 days | 16 days |
| 3.11 Builtin impls | 1 day | 17 days |
| 3.12 Codegen - static | 3 days | 20 days |
| 3.13 Codegen - dynamic | 2.5 days | 22.5 days |
| 3.14 Tests | 2 days | 24.5 days |

**Total: ~5 weeks**

---

## Checklist

### Parsing
- [ ] `trait` keyword lexed
- [ ] `impl` keyword lexed
- [ ] `derive` keyword lexed
- [ ] Trait definition parsed
- [ ] Field declarations in traits parsed
- [ ] Supertrait clause parsed
- [ ] Method signatures parsed
- [ ] Default implementations parsed
- [ ] Impl block parsed
- [ ] Conditional impl constraints parsed
- [ ] Derive statement parsed

### Type System
- [ ] Trait registry created
- [ ] Trait definitions registered (with fields)
- [ ] Field requirements tracked
- [ ] Impl blocks registered
- [ ] Solver handles direct impls
- [ ] Solver handles conditional impls
- [ ] Solver handles supertraits
- [ ] Solver detects cycles
- [ ] Solver handles ambiguity
- [ ] Orphan warnings emitted

### Inference
- [ ] Constraint checking at call sites
- [ ] Field access on constrained types (static)
- [ ] Field access on trait-typed values (dynamic)
- [ ] `==` desugars to `eq`
- [ ] `!=` desugars to `!eq`
- [ ] `<`, `>`, `<=`, `>=` desugar to `compare`
- [ ] `+`, `-`, `*`, `/` desugar to num methods
- [ ] `-x` desugars to `neg`
- [ ] Error messages for unsatisfied constraints

### Derive
- [ ] `eq` derivation for enums
- [ ] `show` derivation for enums
- [ ] `ord` derivation for enums
- [ ] `hash` derivation for enums
- [ ] Conditional derive for generic enums

### Builtins
- [ ] int: show, eq, ord, hash, num, neg
- [ ] float: show, eq, ord, num, neg (NO hash)
- [ ] bool: show, eq, ord, hash
- [ ] string: show, eq, ord, hash
- [ ] list[a]: conditional impls
- [ ] hash[k,v]: conditional impls

### Codegen - Static Dispatch
- [ ] Builtin method implementations
- [ ] User method implementations
- [ ] Operator desugaring in emitter
- [ ] Derived implementations
- [ ] Direct field access for constrained types

### Codegen - Dynamic Dispatch
- [ ] Detect when trait is used as type
- [ ] Generate Go interface for trait
- [ ] Field → getter method in interface
- [ ] Auto-generate getters for implementing types
- [ ] Field access → getter call transformation
- [ ] Method dispatch via Go interface

### Tests
- [ ] All 38+ integration tests pass
- [ ] Unit tests for solver
- [ ] Error case tests
- [ ] Static dispatch field access tests
- [ ] Dynamic dispatch trait object tests

---

## Open Questions / Future Work

1. **Numeric literals**: Should `1` have type `int` or be polymorphic (`num a => a`)?
   - Current: Literals are monomorphic (`1` is `int`, `1.0` is `float`)
   - Future: Polymorphic literals would allow `1 + 1.0` to work

2. **String concatenation**: Is `+` for strings part of `num` or a separate trait?
   - Current: `+` is `num`, so strings need special handling
   - Option: Keep `+` for strings as built-in, not trait-based
   - Option: Add `concat` trait for string-like types

3. **Derive for records**: When records are added (M4), extend derive to work with them

4. **Associated types**: Deferred, but may be needed for Iterator pattern

5. **Higher-kinded types**: Deferred, but needed for Functor/Monad

6. **Go optimizer reliance**: We generate getters aggressively and rely on Go's inliner and dead code elimination. This seems to work well based on Go's track record, but if binary size becomes an issue, we could add analysis to only generate getters when trait is actually used as a type (not just constraint).

---

**Last Updated**: 2026-02-04  
**Author**: Claude Code + User collaboration
