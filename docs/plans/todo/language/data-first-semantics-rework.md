# Data-First Semantics Rework Plan

## Maintenance

- Last verified: 2026-03-29
- Implementation status: Planning (design reset under evaluation)
- Prerequisites: None

## Summary

The pre-modules semantics pass cleaned up several real problems:

- shapes and traits are now distinct concepts,
- field-only and mixed traits are gone,
- purity and narrowing behavior are materially better,
- the typechecker has cleaner typed internal constraints.

But it also pushed named product records into a strongly nominal model that no longer feels like the original Marmoset direction. The result is a hybrid surface:

- structural record syntax and operations still exist,
- shapes still express row-like structural constraints,
- traits still act as ad-hoc behavioral capabilities,
- but named product records now behave more like Rust/Swift nominal structs.

This plan resets that part of the language toward a data-first model while keeping the cleanup that was actually good. The target is:

- records are structural exact product types,
- shapes remain named open structural constraints,
- traits remain method-only behavioral capabilities,
- nominality comes from constructors, not from the `type` keyword alone,
- APIs are function-first and module-qualified,
- dot syntax and "inherent methods" are revisited explicitly instead of being inherited from a Rust-like mental model.

## In Scope

- redefine record, wrapper, and sum semantics around constructor-based nominality
- remove the separate `alias` keyword and collapse transparent naming into plain `type` forms
- keep `shape` as the named row-constraint surface
- keep `trait` as the behavioral/ad-hoc-polymorphism surface
- redesign or remove inherent methods in a way that matches structural records
- use UFCS-style dot sugar over qualified function calls
- preserve and reuse the good implementation work from the pre-modules semantics branch wherever it still fits
- update docs, fixtures, parser/lowering/typechecker/codegen, tree-sitter, and LSP to match the new model

## Out Of Scope

- module syntax redesign
- extern / FFI surface redesign
- ownership / mutation redesign
- associated types or effect algebra work
- changing the accepted shape/trait split back toward mixed or field-only traits
- general open-row syntax for user annotations beyond the existing named `shape` surface

## Locked Decisions

### 1. Records Return To Being Structural

Exact product records are structural again, whether they are anonymous or named:

```mr
type Point = { x: Int, y: Int }

let a: Point = { x: 1, y: 2 }
let b = { x: 1, y: 2 }
```

`a` and `b` have the same exact record type. A named record declaration does not, by itself, create nominal identity.

Consequences:

- field access works on exact structural records,
- spread/update works on exact structural records,
- record patterns work on exact structural records,
- named and anonymous exact records with the same fields are interchangeable,
- extra fields do not fit exact record types; that remains the job of `shape`.

### 2. Constructors Create Nominality

Nominality comes from constructor-bearing type forms, not from the declaration keyword alone.

Examples:

```mr
type Handler = (Request, Context) => Response
type Point = { x: Int, y: Int }
type UserId = UserId(Int)
type User = User({ id: UserId, name: Str, bananas: Int })
type Auth = { Password(Str), Token(Str) }
type Expr = { Num(Int), Add(Expr, Expr) }
type Event = { Click({ x: Int, y: Int }), Rename({ id: UserId, name: Str }), Quit }
```

Rules:

- plain type expressions with no constructors are transparent/exact,
- one-constructor forms are nominal wrappers,
- multi-constructor forms are nominal sums,
- constructor-bearing wrappers and variants may carry record payloads,
- constructor calls and patterns are the source of explicit nominal identity.

This keeps wrappers and sums nominal without forcing product records into the same category.

Consequences:

- constructor-bearing values participate in `match` through constructor patterns,
- constructor-bearing values whose payload is a record support explicit constructor-aware rebuild/update forms,
- structural record syntax on a constructor-bearing value explicitly projects to the payload shape and yields a structural record result.
- nominal wrappers do not implicitly satisfy exact structural record functions, shapes, impls, or trait impls of their payload type; reaching any structural surface requires explicit structural projection or an explicit wrapper-side impl.
- constructor-pattern matching is the preferred unwrap/destructuring surface for nominal wrappers and sums; structural projection syntax remains available but is secondary.

Examples:

```mr
let user = User({ id: UserId(7), name: "milo", bananas: 3 })
let user1 = User(...user, name: "captain milo")   # nominal User
let user2 = { ...user, name: "captain milo" }     # exact structural record
let evt = Click({ x: 1, y: 2 })

match user {
  case User(name:, ...rest): name
}

match evt {
  case Click(x:, y:): x + y
  case Rename(id:, name:): name
  case Quit: "bye"
}
```

This means a wrapper such as `type VerySpecificPoint = VerySpecificPoint({ a: Int })` does not automatically gain `impl Point = { ... }` behavior for `type Point = { a: Int }`, and it does not automatically satisfy matching `shape` constraints either. Qualified calls, shape-constrained functions, and UFCS sugar all follow the same rule: the wrapper must either project explicitly to structural form or provide its own delegating impl.

Rules:

- `User(...user, name: ...)` is the nominal-preserving rebuild/update form,
- `{ ...user, name: ... }` is explicitly structural and yields a plain exact record,
- constructor patterns with record payloads use flattened payload sugar such as `User(name:, ...rest)` and `Click(x:, y:)`,
- the flattened constructor-payload pattern sugar is equivalent to matching the constructor and then matching its record payload.
- when code wants to read or destructure nominal payloads, `match` with constructor patterns should be the primary style in docs and examples.

### 3. `shape` Stays The Named Open Structural Constraint Form

`shape` continues to mean "has at least these fields" and remains backed by row-polymorphic machinery:

```mr
shape HasName = { name: Str }
shape HasBananas = { bananas: Int }
```

`shape` is not behavior:

- no derives,
- no direct impl blocks,
- no trait objects,
- no methods owned by the shape itself.

It remains the way to express open structural constraints in generics and trait superconstraints.

Wrapper boundary rule:

- constructor-bearing nominal wrappers do not satisfy `shape` constraints through their payload implicitly,
- shape-constrained code must see a structural value, so wrappers must project explicitly before they enter that surface,
- the semantic rule is locked as "explicit projection required"; the current examples use structural spread syntax for that projection, but a dedicated projection/unwrap spelling could be introduced later if the surface proves too awkward.

Example:

```mr
shape HasA = { a: Int }

fn read_a[x: HasA](x: x) -> Int = x.a

type VerySpecificPoint = VerySpecificPoint({ a: Int, b: Int })

read_a({ a: 1, b: 2 })                            # ok
read_a(VerySpecificPoint({ a: 1, b: 2 }))        # error
read_a({ ...VerySpecificPoint({ a: 1, b: 2 }) }) # ok
```

### 4. `trait` Stays Method-Only Behavior

Traits remain the behavioral/ad-hoc-polymorphism feature:

```mr
trait Show[a] = {
  fn show(self: a) -> Str
}

trait Banner[a]: Show & HasName = {
  fn banner(self: a) -> Str = self.name + " / " + Show.show(self)
}
```

Traits:

- may depend on traits and shapes,
- may have default methods,
- remain explicit opt-in capabilities,
- are still the only source of trait objects and behavioral dispatch.

Field-only and mixed traits stay removed.

Trait method kinds:

- a method with no body is required,
- a method with a body is default-backed,
- default-backed methods may call other trait methods and shape-provided fields through the trait's own constraints.

Override policy:

- an explicit `impl Trait[Type] = { ... }` may provide required methods and may override default-backed methods,
- overriding a default-backed method should continue to use the explicit `override fn ...` marker,
- omitted default-backed methods come from the trait definition,
- omitted required methods are an error.

Coherence policy:

- there is at most one impl slot per `(trait, exact type)` pair,
- for structural record types, "exact type" means the normalized exact structural record type, not the alias/name used to refer to it,
- shapes never receive methods implicitly just because a value happens to satisfy the shape.

### 5. Derive Remains Explicit Opt-In

Builtin and user-trait derive remain explicit. If a trait has only default methods and all superconstraints are satisfied, it may be derived:

```mr
type User = { name: Str, bananas: Int } derive Show, Banner
```

Required-method traits still need an explicit impl:

```mr
impl Show[User] = {
  fn show(self) -> Str = self.name
}
```

Default-only traits may be derived instead of written out as empty impls:

```mr
trait Banner[a]: Show & HasName = {
  fn banner(self: a) -> Str = self.name + " / " + Show.show(self)
}

type User = { name: Str, bananas: Int } derive Show, Banner
```

Traits with required methods still need an explicit impl, and impls may override defaults selectively:

```mr
trait Render[a]: HasName = {
  fn prefix(self: a) -> Str
  fn render(self: a) -> Str = self.prefix() + self.name
}

impl Render[User] = {
  fn prefix(self) -> Str = "user:"
  override fn render(self) -> Str = "@" + self.name
}
```

Derive policy:

- builtin derives remain explicit opt-in,
- a user trait may be derived only when every missing method is default-backed after substitution,
- all superconstraints must already be satisfied or be part of the same derive closure where that is well-defined,
- `derive` is still an explicit capability choice, not automatic "if it fits, it gets it" behavior,
- an explicit `derive TraitName` that cannot be satisfied is a compile-time error,
- failed derives must produce a concrete reason such as:
  - missing required methods,
  - unsatisfied superconstraints,
  - unsupported builtin derive target shape.

For structural record types, the derive/impl slot belongs to the exact structural type, not to a nominal product owner. Two names for the same exact record type share the same behavioral slot.

### 6. Qualified Functions Are The Canonical API Surface

Marmoset stays function-first at the API level. Qualified function calls are the canonical form:

```mr
json.parse(text)
user.rename(person, "milo")
Show.show(person)
```

Unqualified `parse(...)` should not be the default assumption across modules. The language should bias toward explicit qualification at API boundaries.

## Core Surface Model

This plan keeps the surface small by making `type` the general "name this type expression" form and moving nominality to constructor-bearing RHS forms:

```mr
type Point = { x: Int, y: Int }              # exact structural record
type Handler = (Request, Context) => Response # transparent alias
type UserId = UserId(Int)                    # nominal wrapper
type Maybe[a] = { Some(a), None }            # nominal sum
```

Locked policy:

- `alias` is removed from the target surface,
- plain `type Name = TypeExpr` is the only transparent naming form,
- constructor-bearing RHS forms remain the only source of nominal identity.

## Behavior Model

Dot calls use a UFCS-style model. Current `impl Type = { ... }` blocks are reinterpreted as exact-type extension functions rather than nominal owner behavior.

The intended mental model is UFCS:

- qualified calls are the real API surface,
- the receiver is just the first argument,
- dot syntax is only alternate call spelling,
- dot syntax does not imply type-owned behavior.

Example:

```mr
# geom.mr

type Point = { x: Int, y: Int }

impl Point = {
  fn move(self: Point, dx: Int, dy: Int) -> Point =
    { ...self, x: self.x + dx, y: self.y + dy }
}

fn parse_point(text: Str) -> Point = {
  let payload = json.parse(text)
  { x: payload.x, y: payload.y }
}
```

Call sites:

```mr
let p: geom.Point = { x: 1, y: 2 }

let a = geom.move(p, 3, 4)
let b = p.move(3, 4)
let c = Show.show(p)
let d = p.show()
```

Rules:

- `module.fn(value, ...)` is canonical,
- `value.fn(...)` is UFCS sugar for a qualified call that takes `value` as argument 0,
- `impl Point = { ... }` on an exact structural type registers extension-style functions for that exact type,
- extension methods target exact types only, not open shapes,
- ambiguous dot calls must be rejected with a qualification hint,
- trait methods also use the same UFCS sugar: `value.show()` as sugar for `Show.show(value)`,
- top-level functions and exact-type `impl` functions behave consistently: if a function accepts the exact `Point` type, any other exact same-shape type also fits.

This keeps much of the scripting/Ruby feel without forcing nominal ownership onto records, but the docs must be explicit that dot calls are UFCS sugar, not owner lookup.

## Layering And Boundaries

Data-first does not mean "no boundaries." It means boundaries are explicit and selective.

The recommended layering model is:

- structural records for plain data and DTO-like values,
- constructor-bearing nominal wrappers for domain identities and protected domain values,
- shapes for open structural ports/constraints,
- traits for behavioral ports and ad-hoc polymorphism,
- mapping functions at module boundaries.

Example:

```mr
# domain/user.mr

type UserId = UserId(Int)
type User = User({ id: UserId, name: Str, bananas: Int })

fn create(id: UserId, name: Str) -> User =
  User({ id:, name:, bananas: 0 })
```

```mr
# infra/user_row.mr

type UserRow = { id: Int, name: Str, bananas: Int }
```

```mr
# infra/user_mapper.mr

fn from_row(row: infra.user_row.UserRow) -> domain.user.User =
  domain.user.User({
    id: domain.user.UserId(row.id),
    name: row.name,
    bananas: row.bananas,
  })
```

This preserves strong domain boundaries where needed without making every product record nominal by default.

## Implementation Strategy

### Phase D0. Freeze The Semantic Pivot

- Mark the current nominal-product direction as provisional rather than foundational.
- Treat `01_pre-modules-semantics-foundation.md` as implemented background work whose good internal cleanup will be reused, while this plan supersedes its nominal-product-record decisions on a separate branch.
- Stop adding more product-record-nominality-specific features until this direction is chosen.

### Phase D1. Reframe The Surface Grammar

- Change exact record declarations back to structural semantics.
- Introduce or lock constructor-bearing wrapper syntax such as `type UserId = UserId(Int)`.
- Keep named-sum syntax constructor-based.
- Remove `alias` and migrate transparent naming to plain `type Name = TypeExpr`.
- Update docs/examples first so the new model is visible before the checker work.

### Phase D2. Refactor The Internal Type Model

- Keep typed shape-vs-trait constraints and structural helpers from the current branch.
- Retain the shape/trait split and improved narrowing/purity work.
- Remove nominal named-product identity from the internal type model.
- Keep nominal identity only for constructor-bearing wrappers and sums.
- Simplify record compatibility logic now that exact records are structural again.

### Phase D3. Rework Behavior Attachment

- Reinterpret current `impl Type = { ... }` blocks as exact-type extension-method registration under a UFCS model.
- Preserve default methods, explicit trait impls, and explicit derives.
- Keep ambiguity reporting and qualification hints first-class.

### Phase D4. Reconcile Resolution And Codegen

- Reuse the existing typed call-resolution and narrowing cleanup where possible.
- Rework dot-call resolution around either extension sugar or trait-only dot calls.
- Keep module-qualified calls canonical in both checker and emitter.
- Ensure structural records still lower to efficient Go structs without runtime row dictionaries.
- Ensure constructor-bearing wrappers remain zero-cost or near-zero-cost in the generated Go representation.

### Phase D5. Migrate The Surface

- Rewrite README and feature docs to match the new semantics.
- Rewrite fixtures that currently assume nominal named products.
- Keep the focused good regressions from the current branch: purity, narrowing, trait/shape split.
- Update tree-sitter and LSP output so naming and hover info reflect the new model.

### Recommended Migration Sequence

1. Align the plan/docs first:
   - update this plan,
   - add a short status note to the relevant feature docs,
   - make `match` the primary unwrap/destructuring story for wrappers,
   - present bare structural projection like `{ ...wrapper }` as secondary and mainly useful when reshaping/updating payload data.
2. Reframe the parser and surface AST next:
   - make `type Name = { ... }` structural again,
   - keep constructor-bearing wrappers and sums nominal,
   - add wrapper constructor patterns and flattened payload-pattern sugar,
   - keep UFCS call syntax accepted as surface sugar,
   - update tree-sitter/LSP syntax expectations in the same pass.
3. Refactor the internal type model before broad checker edits:
   - remove nominal named-product identity,
   - keep nominal identity only for wrappers and sums,
   - make wrapper-to-structural access explicit only,
   - keep typed shape/trait constraints and shared structural helpers from the current branch.
4. Then update the checker/inference pipeline in one focused pass:
   - record compatibility,
   - shape satisfaction,
   - wrapper constructor matching,
   - nominal rebuild/update,
   - UFCS resolution,
   - narrowing and exhaustiveness.
5. After the checker is stable, update Go codegen:
   - structural named records should lower the same way as anonymous exact records,
   - wrappers/sums stay nominal,
   - explicit structural projection should lower predictably,
   - wrapper constructor matches must lower like enum/sum constructor matches.
6. Migrate fixtures in semantic clusters instead of all at once:
   - parser/syntax fixtures,
   - records/shapes fixtures,
   - traits/function-model fixtures,
   - pattern-matching/enums/wrapper fixtures,
   - codegen/integration examples.
7. Only after the compiler behavior is stable, do the broad doc/example rewrite:
   - README,
   - feature docs,
   - examples,
   - module-plan assumptions.
8. Finish with the full validation matrix and compare the result against the current nominal-product implementation before deciding what reaches `main`.

## Exit Criteria

- The language has one coherent answer to "what makes a type nominal?"
- Records, shapes, traits, wrappers, and sums have clearly different jobs.
- The docs no longer read like Rust while the semantics feel like a structural/FP language.
- API examples read naturally in one chosen call style instead of mixing incompatible models.
- The module-system plan can build on a stable data-first semantics story.

## Reuse From The Current Branch

The following work from the pre-modules semantics branch should be preserved if possible:

- typed internal shape/trait constraints,
- shared structural helper logic,
- improved union/path narrowing,
- purity enforcement for method-like calls,
- record-pattern and record-spread correctness fixes,
- removal of field-only and mixed traits.

These are implementation-quality improvements, not reasons to keep nominal product records.

## Risks

- If `type` remains overloaded without crisp docs, the language may still feel unclear even if the semantics improve.
- If extension-method sugar is too permissive, it may recreate the same ambiguity and "where did this method come from?" problems as the current hybrid.
- If pipe-only ergonomics arrive without good partial-application ergonomics, the language may feel too verbose.
- If derive/impl rules over exact structural types are not explained carefully, users may expect shape-wide behavior to exist implicitly.

## Related Plans

- `docs/plans/todo/language/01_pre-modules-semantics-foundation.md`
- `docs/plans/todo/language/02_pre-modules-parity-and-hardening.md`
- `docs/plans/todo/language/03_module-system.md`

Relationship notes:

- this plan revisits and supersedes the product-record-nominality portion of `01_pre-modules-semantics-foundation.md`
- `03_module-system.md` should depend on the semantics chosen here, not the other way around

## PROGRESS

### Progress

- 2026-03-30: Read `CLAUDE.md`, confirmed worktree/branch state, and mapped the parser, lowering, type registry, inference, and codegen paths that currently enforce the `alias` versus nominal-`type` split.
- 2026-03-30: Started the plan's migration sequence with docs alignment. Updating the plan and feature notes before checker work so the target surface is visible in-repo while implementation lands.
- 2026-03-30: Completed the docs-alignment pass and verified the compiler unit suite still passes before the first commit.

### Findings

- The current parser/lowering pipeline still treats `type Name = { ... }` as a nominal named product and `alias Name = ...` as the only transparent naming surface.
- Trait and inherent registries already canonicalize impl targets by resolved type, so once transparent `type` forms resolve to exact structural records, behavior slots can naturally key off exact structural types.
- Wrapper boundaries are only partially enforced today: named-product field access/spread/pattern matching are structural by special case, while wrapper projection and constructor-pattern sugar are still missing.

### Caveats

- The branch still documents and tests nominal named products in many places; those clusters will flip incrementally rather than all at once.
- `enum` syntax currently coexists with `type`-based constructor syntax. The semantic rework is targeting the plan's `type`-first surface while preserving compatibility until the migration is complete.

### Verification

- `make unit compiler`
