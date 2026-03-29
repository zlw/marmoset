# Pre-Modules Semantics Foundation Plan

## Maintenance

- Last verified: 2026-03-28
- Implementation status: Planning (rewritten around accepted direction)
- Prerequisites:
  - `docs/plans/done/language/02_syntax-rework.md`
  - `docs/plans/done/language/03_syntax-rework-followup.md`

## Summary

Before modules, prelude, FFI, and stdlib freeze public APIs, Marmoset needs one stable semantics story for:

- named types vs transparent aliases,
- structural shapes vs nominal traits,
- effects and purity,
- unions vs named sums.

This milestone no longer treats the already-landed `Dyn[...]`, intersection, and user-trait-derive work as unsettled design space. Those features remain part of the background constraints for this plan, but the plan now focuses on the still-unresolved semantics that modules would otherwise have to build on top of.

The accepted direction for this milestone is:

```mr
type User = { first_name: Str, last_name: Str }
type UserId = Int
type Color = { Red, Blue, Green }
type UserExternalId = { IntId(Int), StringId(Str) }

alias Users = List[User]
shape HasName = { first_name: Str }

trait Named[T] = {
  fn name(self: T) -> Str
}
```

In this model:

- `type` always introduces a distinct named type with identity,
- `alias` is the only transparent synonym form,
- `shape` is the named open structural record form,
- `trait` is method-only nominal behavior,
- unions remain structural type expressions (`Int | Str`),
- named sums use `type Name = { Variant, Variant(T), ... }`,
- `enum Name = { ... }` remains accepted as syntax sugar for the same named-sum form,
- there are no field-only or mixed traits.

## In Scope

- freeze the declaration-role split between `type`, `alias`, `shape`, and `trait`
- make `type` the identity-bearing declaration for:
  - named product types,
  - named sum types,
  - opaque-by-identity wrappers over an existing representation
- make `alias` the sole transparent type-synonym form
- make `shape` the named open structural record form backed by existing row-polymorphic machinery
- remove field-only and mixed traits in favor of shapes + method-only traits
- freeze explicit construction / no-implicit-coercion rules for named `type`s
- keep `=>` as "may be effectful", not "must be effectful"
- enforce purity uniformly across all callable forms, including method calls
- freeze real union narrowing for `if` and `match`, including alias/path-based narrowing cases
- migrate diagnostics, docs, fixtures, LSP/editor tooling, and lowering/typechecking/codegen behavior to the new model

## Out Of Scope

- module/import/export syntax or multi-file privacy rules
- post-module true representation hiding beyond the pre-module "distinct by identity" behavior of `type`
- prelude, FFI, or stdlib API design work
- a general effect algebra
- literal types
- pattern guards
- general boolean-expression narrowing (`&&`, `||`, negated SAT-style reasoning)
- broad hardening / parity sweeps that belong to `02_pre-modules-parity-and-hardening.md`

## Accepted Direction And Binding Decisions

### 1. Declaration Roles

The language surface is split into four distinct declaration roles:

- `type Name = ...`
  Introduces a distinct named type with identity.
- `alias Name = TypeExpr`
  Introduces a transparent synonym with no identity.
- `shape Name = { ... }`
  Introduces a named open structural record expectation.
- `trait Name[T] = { ... }`
  Introduces nominal method behavior only.

This plan explicitly retires the old "traits can be field-only or mixed" model. Named sums are owned canonically by `type`, but existing `enum Name = { ... }` surface syntax remains accepted as syntax sugar and lowers to the same canonical sum representation.

There is no backward-compatibility requirement for the old surface. This is a WIP language and the implementation should hard-switch to the new forms once the milestone lands.

### 2. `type` Always Creates Identity

`type` is the language construct that owns type identity. The RHS determines representation category, not aliasing behavior.

Accepted `type` forms in this milestone:

- exact named product type:
  `type User = { first_name: Str, last_name: Str }`
- exact named sum type:
  `type Color = { Red, Blue, Green }`
- exact named sum type with payloads:
  `type UserExternalId = { IntId(Int), StringId(Str) }`
- opaque-by-identity wrapper over an existing representation:
  `type UserId = Int`

Consequences:

- `type User = { ... }` and `type Admin = { ... }` are distinct even when their fields match exactly.
- `type UserId = Int` is distinct from `Int`.
- `type Name = SomeTypeExpr` is never a transparent alias.
- no implicit coercion exists:
  - from an anonymous structural value into a named `type`,
  - from a named `type` back to its representation,
  - between two named `type`s with the same shape,
  - between a named sum and a raw union that happens to mention the same payload types.

### 3. Explicit Construction For Named `type`s

Named `type` values are constructed explicitly. The accepted direction for record and wrapper construction is constructor-call syntax:

```mr
let user = User(first_name: "Ada", last_name: "Lovelace")
let id = UserId(42)
```

Anonymous structural records remain available:

```mr
let anon = { first_name: "Ada", last_name: "Lovelace" }
```

But anonymous structural records do not implicitly become `User`.

For named sums, this plan keeps the current constructor/pattern behavior as close as possible to the existing compiler model during the first implementation pass:

- canonical named-sum ownership moves from `enum` to `type`,
- constructor values and match patterns should continue to use an explicit constructor identity owned by the named sum type,
- the implementation may preserve current qualified-constructor behavior initially to minimize migration churn.

### 4. `alias` Is The Only Transparent Synonym Form

`alias` is the sole transparent naming construct:

```mr
alias Users = List[User]
alias Handler = (Request, Context) => Response
alias NameRec = { first_name: Str }
```

Binding consequences:

- `alias Users = List[User]` is exactly `List[User]`
- `alias` does not create a new runtime or static identity
- `alias` is the form used for naming unions, function types, nested generics, and exact anonymous record expressions when the user wants readability but not nominal distinction

`alias` does not own behavior:

- no derives on `alias`
- no inherent impls on `alias`
- no trait impl targets whose identity depends on the alias name

### 5. `shape` Is The Named Open Structural Record Form

`shape` is the named open structural contract form:

```mr
shape HasName = { first_name: Str }
shape HasFullName = { first_name: Str, last_name: Str }
```

Binding consequences:

- `shape` is backed by the existing row-polymorphic machinery already present internally
- `shape` means "has at least these fields"
- plain anonymous record type expressions stay exact product types
- `alias NameRec = { first_name: Str }` is exact
- `shape HasName = { first_name: Str }` is open

Generic shapes are in scope for this milestone. The implementation should allow the same generic use cases that current generic field-only traits support, but through `shape` instead.

`shape` does not own behavior:

- no derives
- no inherent methods
- no impl blocks
- no `Dyn[...]`

Shapes are satisfied structurally, not via registration.

### 6. `trait` Becomes Method-Only Nominal Behavior

Traits are method-only after this milestone:

```mr
trait Named[T] = {
  fn name(self: T) -> Str
}
```

Accepted policy:

- field-only trait declarations are removed
- mixed trait declarations are removed
- structural field expectations move to `shape`
- nominal method behavior remains in `trait`

Traits may depend on:

- other traits,
- shapes.

Constraint conjunction remains available:

```mr
trait Renderable[T]: HasName & Show = {
  fn render(self: T) -> Str
}

fn greet[T: HasName](x: T) -> Str = x.first_name
fn describe[T: HasName & Named](x: T) -> Str = x.name()
```

The resolver and checker must distinguish shape constraints from trait constraints. Only traits contribute method obligations and trait-object behavior.

### 7. Trait Objects And Derives Stay Trait-Only

The existing `Dyn[...]` / derive work remains in place, but the new binding rules are:

- `Dyn[...]` accepts trait names only
- shape names are invalid inside `Dyn[...]`
- derive clauses mention traits only
- shapes never participate in derive

This keeps trait objects as a nominal-behavior feature and prevents shapes from reintroducing the field-only trait-object confusion this milestone is explicitly removing.

### 8. Ownership Rules For Behavior

Behavioral ownership is frozen as:

- derives: only on `type`
- inherent methods: only on `type`
- trait impl targets: only on `type` and builtins
- aliases: no behavior ownership
- shapes: no behavior ownership

Examples:

```mr
type User = { first_name: Str, last_name: Str } derive Show
type UserId = Int

impl User = {
  fn full_name(self: User) -> Str = self.first_name + " " + self.last_name
}

impl Show[UserId] = {
  fn show(self: UserId) -> Str = "..."
}
```

Rejected:

```mr
alias Users = List[User]
impl Users = { ... }           # reject

shape HasName = { first_name: Str }
impl Named[HasName] = { ... }  # reject
```

### 9. Effects And Purity

The minimal effect model is frozen as:

- `->` means pure
- `=>` means may be effectful

This milestone explicitly rejects the stricter "declared effectful functions must actually perform an effect" rule. That rule is too brittle for:

- trait impls whose body is pure even though the trait contract is effect-capable,
- test stubs,
- temporary implementations,
- API-stable signatures.

The accepted purity rules are:

- a `->` callable may not perform effectful operations
- a `=>` callable may perform effectful operations but is not required to do so
- pure callable types remain subtypes of effect-capable callable slots where that is already the accepted function-subtyping policy

Purity enforcement must become uniform across:

- top-level functions,
- local helpers,
- explicit lambdas,
- placeholder-rewritten callbacks,
- trait methods,
- inherent methods,
- default methods,
- higher-order calls through variables and aliases,
- method calls,
- dynamic trait-object dispatch when the method is effectful.

This phase explicitly owns the currently inconsistent "effectful function call is caught, effectful method call slips through" class of bug.

### 10. Unions Remain Structural, Named Sums Remain Nominal

The language keeps both:

- structural unions:
  `Int | Str`
- named sums:
  `type UserExternalId = { IntId(Int), StringId(Str) }`

These are not interchangeable concepts.

Accepted policy:

- raw unions remain ordinary type expressions
- named sums are closed nominal types owned by `type`
- `type Name = Int | Str` is not the preferred named-domain form in this milestone
- if the user only wants a name for a union, use `alias`
- if the user wants a distinct named multi-form domain type, use a sum-form `type`

### 11. Real Union Narrowing Scope

This milestone explicitly broadens and freezes the supported narrowing matrix for structural unions.

Required supported cases:

- `if (x is T)` narrowing
- `match x { ... }` on union-typed scrutinees
- alias narrowing:
  `let y = x; if (y is Int) ...`
- field-path narrowing:
  `if (box.value is Int) ...`
- narrowing after projection alias:
  `let v = box.value; if (v is Int) ...`
- complement narrowing when the remaining members are known

This plan does not promise:

- arbitrary boolean-combinator reasoning,
- pattern guards,
- fully general SAT-style narrowing.

The checker and emitter must agree on the supported matrix. The current state, where `if (x is T)` has partial checker support but `match` on a union still falls through to codegen-internal failure paths, is explicitly in scope for repair here.

### 12. Migration Policy

There is no compatibility track for the old surface. This milestone hard-switches the language to the new declaration-role model.

Old forms removed in this milestone:

- field-only traits
- mixed traits
- old assumptions that named record declarations behave as anonymous structural records without explicit construction

The implementation should migrate:

- parser tests,
- lowering tests,
- checker tests,
- integration fixtures,
- syntax docs,
- editor grammars,
- LSP/editor integrations,
- tree-sitter grammar,
- any user-facing examples

in the same milestone, not later.

## Shared Code Touchpoints

- `lib/frontend/syntax/ast.ml`
- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/lower.ml`
- `lib/frontend/typecheck/types.ml`
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/trait_solver.ml`
- `lib/frontend/typecheck/resolution_artifacts.ml`
- `lib/backend/go/emitter.ml`
- `docs/SYNTAX.md`
- `docs/features/traits.md`
- `docs/features/records.md`
- `docs/features/unions.md`
- `docs/features/type-annotations-and-aliases.md`
- `docs/features/enums.md`
- `docs/features/pattern-matching.md`
- `README.md`
- `tools/tree-sitter-marmoset/**`
- `tools/lsp/**`
- `tools/nvim-marmoset/**`
- `tools/vscode-marmoset/**`
- `tools/zed-marmoset/**`
- `tools/jetbrains-marmoset/**`
- `test/fixtures/**`

## Work Order

### Phase P0. Freeze The Semantic Matrix

Purpose:

- turn the accepted direction above into one explicit language contract before implementation churn starts
- avoid re-arguing the role split while parser/typechecker work is underway

Tasks:

- rewrite `docs/SYNTAX.md` to state:
  - `type` owns identity,
  - `alias` is transparent,
  - `shape` is open structural,
  - `trait` is method-only
- write the accepted migration examples for:
  - named record type,
  - named sum type,
  - named opaque-by-identity wrapper,
  - alias,
  - shape,
  - trait + shape composition,
  - union vs named sum
- document the no-implicit-coercion rule for named `type`s
- document the accepted `->` / `=>` semantics
- document the required union-narrowing matrix for this milestone
- explicitly record the removed forms:
  - field-only traits,
  - mixed traits,
  - direct field-bearing traits as a language category
- document that `enum Name = { ... }` is preserved as syntax sugar over the canonical named-sum `type` form

Likely files:

- `docs/SYNTAX.md`
- `docs/features/traits.md`
- `docs/features/records.md`
- `docs/features/unions.md`
- `docs/features/type-annotations-and-aliases.md`
- `docs/features/enums.md`
- `docs/features/pattern-matching.md`
- this plan

Exit criteria:

- the declaration-role split is written normatively
- no remaining doc describes field-only/mixed traits as part of the active language
- no remaining doc treats `type Name = Int` as a transparent alias

### Phase P1. Surface AST, Grammar, And Lowering

Purpose:

- teach the parser/lowering pipeline the new declaration roles and remove the retired surface

Tasks:

- add parser support for:
  - `alias Name = TypeExpr`
  - `shape Name = { ... }`
  - `type Name = { field: Type, ... }`
  - `type Name = { Variant, Variant(Type), ... }`
  - `type Name = TypeExpr` wrapper form
- add or preserve parser support for:
  - `enum Name = { Variant, Variant(Type), ... }` as sugar over the same sum form
- remove parser support for:
  - field members inside trait declarations
  - mixed trait declarations
- lower the new declaration forms into canonical ownership-distinct AST nodes:
  - named product type
  - named sum type
  - wrapper/opaque-by-identity type
  - alias
  - shape
- lower `enum Name = { ... }` to the same canonical named-sum form as `type Name = { ... }`
- keep grammar disambiguation deterministic:
  - record product members use `name: Type`
  - sum variants use bare constructors or constructor payload form
- update tree-sitter and editor grammars in the same phase

Likely files:

- `lib/frontend/syntax/ast.ml`
- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/lower.ml`
- `tools/tree-sitter-marmoset/**`
- `tools/lsp/**`
- `tools/nvim-marmoset/**`
- `tools/vscode-marmoset/**`
- `tools/zed-marmoset/**`
- `tools/jetbrains-marmoset/**`

Required tests:

- parser tests for every accepted declaration form
- parser tests rejecting removed forms
- lowering tests preserving the declaration-role split
- grammar/tooling tests for syntax highlighting and parsing

Exit criteria:

- the parser no longer accepts the retired surface
- all accepted declaration forms lower to distinct canonical ownership categories

### Phase P2. Named-Type Identity, Construction, And Representation

Purpose:

- make the typechecker and emitter treat named `type`s as identity-bearing and aliases as transparent

Tasks:

- represent named product types, named sums, and wrapper types as distinct named types internally
- keep anonymous structural record types available as exact record expressions separate from named product types
- implement explicit construction for:
  - named product types
  - wrappers
  - named sums / payload variants
- reject implicit conversion:
  - anonymous record -> named product type
  - named product type -> anonymous record
  - representation type -> wrapper type
  - wrapper type -> representation type
  - same-shape named type -> same-shape named type
- make `alias` expand transparently in annotation conversion and checking
- enforce ownership rules:
  - derives only on `type`
  - inherent impls only on `type`
  - no behavior ownership on `alias` or `shape`
- keep named sums separate from structural unions
- reject or defer raw-union wrapper forms such as `type Name = Int | Str`

Likely files:

- `lib/frontend/typecheck/types.ml`
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/backend/go/emitter.ml`
- `docs/features/type-annotations-and-aliases.md`
- `docs/features/records.md`
- `docs/features/unions.md`

Required tests:

- named product construction succeeds only with explicit construction
- anonymous structural record no longer silently typechecks as a named product
- wrapper types require explicit construction
- same-shape named product types are distinct
- aliases remain transparent
- named sums stay distinct from unions

Exit criteria:

- the compiler enforces identity for every `type`
- transparent behavior lives only in `alias`

### Phase P3. Shapes And Method-Only Traits

Purpose:

- replace the current field-only/mixed trait model with a clean split between structural shapes and nominal behavior

Tasks:

- introduce canonical shape registration and checking
- split current trait solver responsibilities into:
  - structural shape satisfaction,
  - nominal trait satisfaction
- remove field members from trait AST / trait registry data
- allow traits to constrain over shapes and traits
- keep shape satisfaction structural and transitive where accepted
- make `Dyn[...]` reject shapes explicitly
- update derive validation so shapes cannot appear in derive clauses
- migrate old field-only / mixed trait fixtures to shape + trait equivalents

Likely files:

- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/trait_solver.ml`
- `lib/frontend/typecheck/infer.ml`
- `docs/features/traits.md`
- `test/fixtures/traits/**`
- `test/fixtures/cross_feature/**`

Required tests:

- shape constraints accept wider structural records
- exact aliases stay exact and do not become open by accident
- trait methods resolve only from nominal trait constraints
- shape names are rejected in `Dyn[...]`
- mixed/field-only trait syntax is rejected with clear diagnostics

Exit criteria:

- structural fields and nominal methods no longer share one declaration category
- old trait-kind branching is gone from the active language model

### Phase P4. Effects And Purity

Purpose:

- freeze the minimal effect model and enforce it consistently

Tasks:

- keep `->` as pure guarantee
- keep `=>` as may-effectful, not must-effectful
- audit every callable path so purity checks include:
  - direct function calls,
  - method calls,
  - qualified calls,
  - dynamic trait-object dispatch,
  - higher-order callbacks through aliases and locals,
  - default methods,
  - trait methods,
  - inherent methods,
  - lambdas / placeholder callbacks
- preserve the accepted callable-subtyping rule where pure callables are usable in effect-capable slots
- make diagnostics name the actual callable form that introduced the effect

Likely files:

- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/resolution_artifacts.ml`
- `docs/SYNTAX.md`
- `docs/features/traits.md`

Required tests:

- pure function calling effectful method is rejected
- pure trait method / inherent method calling effectful operations is rejected
- effect-capable signatures with pure bodies remain accepted
- higher-order paths remain consistent for calls and method calls

Exit criteria:

- purity behavior is uniform across callable forms
- the current method-call purity hole is closed

### Phase P5. Real Unions And Narrowing

Purpose:

- finish the checker/codegen contract for structural unions without conflating them with named sums

Tasks:

- keep raw unions as structural type expressions
- implement the required narrowing matrix for:
  - `if`,
  - `match`,
  - aliases,
  - field paths,
  - projection aliases
- add codegen support for `match` on union-typed scrutinees
- remove AST-shape-dependent narrowing behavior where a feature only works for a plain identifier but not for an equivalent alias/path form
- keep complement narrowing deterministic
- make diagnostics explain whether the user needs:
  - a structural union,
  - a named sum type,
  - or an alias to one of those forms

Likely files:

- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/annotation.ml`
- `lib/backend/go/emitter.ml`
- `docs/features/unions.md`
- `test/fixtures/unions/**`
- `test/fixtures/codegen/**`
- `test/fixtures/codegen_stress/**`

Required tests:

- union narrowing in `if`
- union narrowing in `match`
- alias narrowing
- field-path narrowing
- projection-alias narrowing
- codegen for union `match`

Exit criteria:

- `if` and `match` both work for the accepted narrowing matrix
- unions are no longer checker-only features that fail in codegen for common cases

### Phase P6. Fixture, Doc, And Tooling Migration

Purpose:

- finish the semantic switch cleanly so later milestones do not inherit dual models

Tasks:

- migrate fixtures from old field-only/mixed trait forms to `shape`
- keep existing `enum` fixtures valid while adding focused coverage for the canonical named-sum `type` form
- migrate docs/examples to explicit named-type construction
- update generated-go snapshots or focused integration fixtures where the new named-type identity changes lowering
- update LSP/editor integrations so diagnostics, hovers, symbols, completions, and semantic-token classification reflect the new declaration-role split
- remove stale docs describing aliases as the meaning of `type Name = Int`
- decide doc policy for surface examples:
  - either prefer canonical `type` in docs while keeping `enum` accepted,
  - or explicitly document both spellings as equivalent

Likely files:

- `docs/**`
- `tools/lsp/**`
- `tools/nvim-marmoset/**`
- `test/fixtures/**`
- `tools/zed-marmoset/**`
- `examples/**`
- `README.md`

Required tests:

- focused integration fixtures for:
  - named product construction
  - named wrapper construction
  - named sum constructors
  - shape constraints
  - trait impls on named types
  - real union narrowing
- focused LSP/editor regressions for:
  - diagnostics on removed trait forms
  - hover / symbol output for `type`, `alias`, `shape`, and `trait`
  - semantic-token / grammar classification for the new declaration forms

Exit criteria:

- no first-party doc or fixture still teaches the retired model
- the module-system milestone can assume the new declaration-role split without fallback paths

## Exit Criteria

- the language has one stable pre-module semantics story for:
  - `type`,
  - `alias`,
  - `shape`,
  - `trait`,
  - effects,
  - unions
- named types have identity and require explicit construction
- aliases are transparent and behaviorless
- shapes own structural record expectations
- traits own nominal method behavior
- purity behavior is uniform across callable forms
- unions are real checker + codegen features, including `match`
- `docs/SYNTAX.md`, feature docs, tests, and tooling all describe the same model
- the module-system plan can proceed without inheriting unresolved "is this structural, nominal, alias, or trait?" questions

## Related Plans

- `docs/plans/done/language/03_syntax-rework-followup.md` owns the already-landed `Dyn[...]`, intersections, and derive follow-up work that this plan now treats as background constraints
- `docs/plans/todo/language/02_pre-modules-parity-and-hardening.md` hardens the semantics frozen here
- `docs/plans/todo/language/03_module-system.md` should build on this declaration-role split instead of defining its own workaround semantics

## PROGRESS

### Status

- Complete

### Completed

- Read `CLAUDE.md` commit/testing rules before implementation
- Read and executed the full plan across parser, lowering, checker, emitter, docs, tooling, and fixture layers
- Landed the declaration-role split:
  - `alias` is transparent and behaviorless
  - `type` owns nominal named products, wrappers, and sums
  - `shape` owns structural field expectations
  - `trait` is method-only
- Added explicit constructor handling for named products and wrappers through parsing, lowering, inference, and Go emission
- Added nominal type representation in the typechecker via `Types.TNamed` plus named-type registry support
- Moved structural superconstraint satisfaction to shapes instead of field-only / mixed traits
- Removed field members from trait definitions and updated derive expansion, impl validation, constrained field access, and method resolution to reflect the new model
- Preserved canonical named sums through `enum` while documenting `type` as the canonical nominal declaration story
- Migrated focused fixtures from the retired field-only / mixed-trait model to `shape` / nominal-type semantics
- Updated first-party docs and examples to teach:
  - explicit named-type construction
  - `shape` for structural constraints
  - method-only `trait`
  - transparent `alias`
- Updated LSP/editor and tree-sitter grammar/token classification for the declaration-role split
- Added / updated focused checker coverage for:
  - enum references inside named-product and shape fields
  - derive expansion over shape superconstraints satisfied by named products
- Reworked `examples/new-syntax-upcase.mr` so every showcased construct compiles under the final semantics

### Findings

- The existing `enum` machinery was strong enough to preserve accepted sum-type behavior while moving the rest of the language onto canonical nominal `type` semantics
- Named products and wrappers needed materially more work than named sums because nominal identity had to be carried through annotation conversion, constraint solving, method lookup, and Go codegen
- Top-level registration order matters:
  - enums must register before named products / shapes whose field annotations mention them
- Derive expansion needs a fallback path that works both before full registration and in partially registered test setups
- Record patterns remain structural:
  - they do not destructure nominal `type` values directly
  - the showcase example had to use a closed structural record annotation for the pattern demo
- Field-function fallback codegen needed an additional named-product lookup path for `TNamed` receivers in mixed dot / qualified-call chains

### Caveats

- `enum` remains accepted for named sums; docs now treat `type` as the canonical nominal surface while preserving `enum` compatibility
- Derive expansion still carries a source-level fallback for shapes / named products before the full registration pass; that is intentional and covered by focused tests
- The final semantics intentionally keep structural record-pattern behavior separate from nominal named-type behavior

### Verification

- `dune runtest --root /Users/zlw/src/marmoset/marmoset lib/frontend/typecheck --force`
- `dune runtest --root /Users/zlw/src/marmoset/marmoset lib/backend/go --force`
- `dune build --root /Users/zlw/src/marmoset/marmoset bin/main.exe`
- `./test/integration.sh traits_field`
- `./test/integration.sh traits`
- `./test/integration.sh cross_feature`
- `./test/integration.sh traits_impl symbols codegen_mono vnext_canary`
- `./test/integration.sh function_model records operators runtime traits_inherent`
- `cd /Users/zlw/src/marmoset/marmoset/tools/tree-sitter-marmoset && npm run generate`
- `cd /Users/zlw/src/marmoset/marmoset/tools/tree-sitter-marmoset && npm test`
- `./_build/default/bin/main.exe /Users/zlw/src/marmoset/marmoset/examples/records-typed.mr`
- `./_build/default/bin/main.exe /Users/zlw/src/marmoset/marmoset/examples/traits-typed.mr`
- `./_build/default/bin/main.exe /Users/zlw/src/marmoset/marmoset/examples/user.mr`
- `./_build/default/bin/main.exe /Users/zlw/src/marmoset/marmoset/examples/new-syntax-upcase.mr`
