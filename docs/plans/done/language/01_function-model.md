# Function Model Rework Plan (Codex)

## Maintenance

- Last verified against codebase: 2026-03-07
- Implementation status: Implemented
- Type: Historical implementation plan
- Owner areas:
  - frontend syntax + AST
  - frontend typecheck + registries
  - backend emitter + monomorphization
  - integration harness + fixtures
  - LSP compatibility (no new LSP features)

## Why This Revision Exists

This revision keeps the original goal unchanged: **all callables behave consistently**.

The confusion from prior review was scope wording, not goal disagreement. The required direction is:

1. Keep one unified typing mechanism for callables.
2. Use thin adapters for top-level functions, trait methods, and inherent methods.
3. Add prerequisites needed so that one mechanism can actually be implemented without hidden breakage.
4. Shape the resolution/plumbing work so the next milestones (modules, then FFI) can plug into it without replacing the checker/emitter contract again.

## Core Architecture Decision (Locked)

### One engine + thin adapters

- Engine: `type_callable` (single implementation of callable typing rules).
- Adapters:
  - top-level function adapter
  - trait-impl method adapter
  - inherent method adapter
- Adapters only provide context and AST payload; they do not implement separate typing logic.

### One resolution layer + extensible artifacts

- Resolution is not just a method-typing concern. This plan will introduce one central qualified-call/member classification model now.
- Checker/emitter plumbing must be based on a general `call_resolution` artifact family, not a narrow `method_resolution` side channel.
- This plan only needs to populate current single-file callable cases:
  - method dispatch
  - enum constructor dispatch
  - field-function fallback
- The artifact family must still reserve namespace-qualified variants so modules/FFI can plug in later without changing the checker/emitter API shape.

### No semantic AST rewrite of qualified dots

- We are **not** adding a semantic rewrite path that resolves `a.b` / `a.b(...)` by rewriting it into unrelated nodes before the checker sees it.
- Future module/import work may still desugar already-resolved names, but it must reuse the same classifier semantics rather than bypass them with a second resolution mechanism.

### No AST rewrite pass in this project

- We are **not** adding a whole-program normalization pass in this plan.
- We will still update AST node shapes where needed (stable method ids, method generics, method call type args, method effect marker, method body representation) so adapters and artifacts can feed one engine correctly.

### Explicit non-goals for this plan

1. No UFCS syntax (`<Type as Trait>::m`).
2. No new LSP capabilities (no new go-to-definition feature, no new completion feature classes).
3. No module-system or FFI semantics beyond compatibility hooks required by this rework.

## Outcome (Definition of Done)

When complete, all of these are true:

1. `x.m(...)` resolves with inherent-first precedence.
2. `Trait.m(x, ...)` and `Type.m(x, ...)` are first-class call forms.
3. Enum-qualified dot dispatch remains variant-first, then enum inherent-qualified method.
4. Method-level generics (`fn name[b](...)`) work in trait signatures, impl methods, inherent methods, and method call sites.
5. Inherent-vs-trait same-name collisions are resolved at call sites, not rejected at definition time.
6. Lowering (emitter) consumes typed resolution metadata; backend does not re-resolve method source.
7. One callable typing engine exists and is used by all callable forms with bodies.
8. Trait impl methods can omit parameter/return annotations and infer from trait signatures.
9. Inherent methods can omit annotations and infer like top-level functions (receiver constrained by impl target type).
10. Purity checks (`->` vs `=>`) are enforced consistently for all callable forms with bodies.
11. Method-level and impl-level generics monomorphize together without key collisions.
12. Large fixture corpus exists (simple + edge + weird), starts red via xfail, and ends fully green.
13. No global mutable metadata side-channel is required between checker and emitter.
14. Callable and call-site artifacts are keyed by stable identities that remain valid when later multi-file compilation lands.
15. Resolution artifacts are extensible to namespace-qualified calls (`module`, `extern`) without replacing the checker/emitter API shape.
16. LSP behavior does not regress for existing supported features.

## Locked Semantics

1. Qualifier token remains `.`.
2. Qualified-LHS classification order for bare `a.b` / `a.b(...)` remains: value > namespace > enum > trait > type alias.
3. Namespace bucket means:
   - module namespace
   - extern qualifier
4. Value dot calls (`x.m(...)`) resolve in this order:
   - callable field only
5. Receiver-first UFCS candidate collection across inherent methods, trait methods, and free functions was later removed in the direct-surface follow-up.
6. Method generics syntax is `fn name[b](...)`.
7. Method call explicit type args syntax is `.name[type_args](...)`.
8. Disambiguation rule for `.name[ ... ]`:
   - `.name[type_args](...)` is always method type arguments.
   - If user intends runtime index-then-call, they must write parentheses: `(x.name[i])(arg)`.
9. Callable effect markers are consistent for callable forms with bodies:
   - `->` means pure
   - `=>` means effectful
   - omitted marker means infer effectfulness from body
10. Resolution/lowering artifacts use one general `call_resolution` family, not a method-only family.
11. No semantic AST rewrite may bypass the central classifier for qualified dot forms.
12. No new LSP features will be added in this plan; only compatibility fixes required by semantic changes.

## Dependency Graph (Read Before Coding)

This ordering is mandatory to keep phase boundaries coherent and avoid backtracking:

1. Phase 0: Harness xfail/XPASS foundation.
2. Phase 1: Massive fixture addition (many red/xfail allowed).
3. Phase 2: Syntax + AST + stable identity prerequisites.
4. Phase 3: Registry/type-model prerequisites + stable keys + method generic instantiation.
5. Phase 4: Resolution model switch + typed lookup outcomes.
6. Phase 5: Call-resolution metadata plumbing (checker -> emitter explicit artifacts).
7. Phase 6: Emitter parity + monomorphization identity fixes.
8. Phase 7: Callable unification (one engine + adapters).
9. Phase 8: LSP compatibility updates (no feature expansion).
10. Phase 9: De-xfail + docs sync.

Operational rule:

- temporary red inside a phase is acceptable while moving a coupled compiler slice
- each phase exit must restore the targeted gates to green before starting the next phase

Do not swap Phases 5 and 6.
Do not run Phase 7 before Phase 6 removes emitter dependence on method annotations.

---

## Phase 0: Integration Harness Foundation (`xfail`/`XPASS`)

### Goal

Allow fixture-first rollout with red target semantics while keeping CI green.

### Files

- `test/integration.sh`
- `test/integration/common.sh`
- `test/integration/09_harness_canaries.sh`

### Exact `xfail` spec (locked)

1. Annotation syntax: `# xfail: <reason>`
2. Scope: file-wide.
3. Placement rule: must appear in leading comment block before first non-comment, non-empty source line.
4. Cardinality: max one `xfail` annotation per fixture.
5. Reason is required and must be non-empty.

### Result classification matrix (locked)

Let `expected_success` mean fixture passes under its mode (`run`, `reject`, `build-only`).

1. No `xfail` + `expected_success = true` -> `PASS`
2. No `xfail` + `expected_success = false` -> `FAIL`
3. Has `xfail` + `expected_success = false` -> `XFAIL`
4. Has `xfail` + `expected_success = true` -> `XPASS`

Exit behavior:

1. `FAIL` contributes failure.
2. `XPASS` contributes failure (forces stale-xfail cleanup).
3. `PASS` and `XFAIL` are non-failing.

### Implementation steps

1. Extend fixture parser in `parse_fixture`:
   - parse and validate `xfail` header.
   - store `XFAIL_ENABLED=1/0`, `XFAIL_REASON`.
2. Update `run_fixture` result mapping to produce one of `PASS|FAIL|XFAIL|XPASS`.
3. Add global counters in harness:
   - `PASS`, `FAIL`, `XFAIL`, `XPASS`, `TOTAL`.
4. Update summary print in suite-end paths:
   - print all four counters.
   - fail suite if `FAIL > 0` or `XPASS > 0`.
5. Parallel mode contract:
   - each worker log prints normalized final token line, e.g. `__RESULT__:XFAIL`.
   - parent aggregator parses token and increments corresponding counter.
   - worker process exit code must be non-zero only for `FAIL|XPASS`.
6. Add canary tests for all four outcomes.

### Phase 0 exit criteria

1. Harness accepts valid `# xfail:` annotations and rejects malformed ones.
2. XPASS fails suite deterministically in serial and parallel modes.
3. Summary includes `PASS/FAIL/XFAIL/XPASS`.
4. Existing non-xfail suites remain green.

---

## Phase 1: Massive Fixture Expansion (Simple + Edge + Weird)

### Goal

Create a large target-semantics safety net first, then drive red->green by phase.

### Files / folders

- New: `test/fixtures/function_model/`
- Extend existing groups:
  - `test/fixtures/traits/`
  - `test/fixtures/traits_impl/`
  - `test/fixtures/traits_inherent/`
  - `test/fixtures/records/`
  - `test/fixtures/codegen/`
  - `test/fixtures/codegen_mono/`

### Required minimum

- Add **at least 120** new fixtures in this phase.
- Recommended target: 140-160 to reduce later backfill.

### Naming convention

- Prefix new function-model fixtures with `fm` + 3 digits:
  - `fm001_...mr`, `fm002_...mr`, etc.
- Keep one behavior family per fixture.
- Keep fixture names behavior-descriptive, not implementation-descriptive.

### Distribution (minimum)

1. Resolution precedence + fallback: 25
2. Qualified calls (trait/type/enum): 20
3. Ambiguity diagnostics + hints: 20
4. Method generics parse/infer/constraints: 25
5. Collision behavior migration: 10
6. Weird/abusive edge cases: 20

### Mandatory weird edge cases

All of these must have fixtures:

1. Value shadows enum name in dotted access.
2. Value shadows trait name in dotted call.
3. Enum variant and inherent method same member name (constructor wins on enum qualifier).
4. `x.m()` where fallback field exists but is non-callable.
5. `x()` where `x` is not callable (type error, not Go build error).
6. Trait ambiguity with 2+ candidates including supertrait-expanded candidates.
7. Constrained TVar ambiguity with explicit qualification hints.
8. Method generics with constraints and nested generic calls.
9. Method generics with explicit + inferred type args in same file.
10. Mixed chain: dot call + qualified call + field fallback in one function.
11. Generic inherent impl overlap ambiguity.
12. Receiver target types with aliases, records, enums, unions.
13. Calls inside match/if/closure with narrowed receiver.
14. Methods named like builtins (`show`, `hash`, etc.).
15. `.name[type_args](...)` vs `(x.name[i])(arg)` disambiguation.

### Characterization anchors (must stay green)

- `test/fixtures/records/r012_record_fn_field_called_with_dot_syntax.mr`
- `test/fixtures/traits_impl/ti31_two_traits_same_method_name_both_impls_causes_ambiguous_disp.mr`
- `test/fixtures/traits_inherent/th37_generic_inherent_impl_target_on_enum_type_works.mr`

### Legacy collision fixtures to migrate later

- `test/fixtures/traits/t09_inherent_method_colliding_with_trait_method_is_rejected.mr`
- `test/fixtures/traits_inherent/th19_inherent_method_same_name_as_trait_method_is_ambiguity_error.mr`
- `test/fixtures/traits_inherent/th21_inherent_method_collides_with_builtin_show_on_int.mr`

### Phase 1 exit criteria

1. >=120 new fixtures committed.
2. New fixtures can be xfailed while semantics are in-flight.
3. `./test/integration.sh function_model` works once group exists.
4. Main integration remains green.

---

## Phase 2: Syntax + AST + Stable Identity Prerequisites

### Goal

Add syntax/AST support required for one callable engine, stable callable identities, and method generic call sites.

### Files

- `lib/frontend/syntax/ast.ml`
- `lib/frontend/syntax/parser.ml`
- parser tests in `parser.ml` test section
- LSP walkers that pattern-match changed AST constructors

### Phase 2 migration note

- Expect broad compiler breakage while the AST shape changes land. This phase touches parser, typechecker, emitter, tests, and LSP walkers.
- Temporary red is acceptable while these call sites are being updated together.
- Phase 2 exit still requires the parser/AST layer and directly affected compiler code to be green again.

### 2.1 AST changes (locked)

1. Add shared method-effect representation:
   - recommended type: `effect_annotation = Pure | Effectful`
   - trait method signatures use required effect annotation
   - impl/inherent methods use optional effect annotation (`None` = infer from body)
2. Extend trait method signature AST:
   - `method_sig_id : int`
   - `method_generics : generic_param list option`
   - `method_effect : effect_annotation`
3. Extend impl/inherent method AST:
   - `impl_method_id : int`
   - `impl_method_generics : generic_param list option`
   - `impl_method_effect : effect_annotation option`
   - change `impl_method_body : expression` -> `impl_method_body : statement`
4. Extend method-call AST payload to carry optional explicit type args:
   - migrate `MethodCall` shape to include `method_type_args : type_expr list option`.
   - recommended representation: record payload variant for readability.
5. Stable-id rule:
   - method ids are parser-assigned local ids, unique within one parsed file
   - ids are not reconstructed from names later
   - ids must survive parser -> checker -> emitter plumbing unchanged

### 2.2 Parser changes: method declarations

1. `parse_method_sig`:
   - allocate `method_sig_id` via parser fresh-id path.
   - after method name, parse optional generic list.
   - parse params as typed params.
   - parse required effect marker + return type (`-> T` or `=> T`).
2. `parse_method_impl`:
   - allocate `impl_method_id` via parser fresh-id path.
   - after method name, parse optional generic list.
   - parse params with optional annotations.
   - parse optional effect marker + optional return annotation (same policy as top-level function literals).
   - parse method body with `parse_block_statement`, not expression-only parser.

### 2.3 Parser changes: method call type args and disambiguation

Implement in `parse_dot_expression` as a single parse unit.

Algorithm:

1. Parse `.member_name`.
2. If next token is `[`:
   - parse type argument list using type parser (`parse_type_expr`), not expression parser.
   - require closing `]`.
   - require immediate `(` after `]`.
   - if `(` missing, emit deterministic error with hint:
     - "method type arguments must be followed by call; use `(x.member[i])(arg)` for runtime index-then-call"
3. Parse call args and emit `MethodCall` with `method_type_args = Some ...`.
4. Else if next token is `(`:
   - parse normal method call with `method_type_args = None`.
5. Else:
   - parse `FieldAccess`.

### 2.4 Parser helper cleanup

Current code has overlapping generic-list helpers. Consolidate to avoid cursor bugs.

1. Keep one declaration-side helper for `[a: show + eq, b]` after names.
2. Add one call-site type-arg helper for `[Int, Result[a, b]]`.
3. Do not use expression list parser for type args.
4. Remove duplicated declaration-side generic helpers once the new path is in.

### 2.5 Required parser tests

Add tests for all of these:

1. Trait method generic + effect marker parse.
2. Impl method generic + optional annotation parse.
3. Inherent method generic + optional annotation parse.
4. Method body as block statement (let/return/multi-statement).
5. `.name[type](...)` method call parse.
6. `.name[t1, t2](...)` parse.
7. Nested type args parse.
8. `.name[type]` without call -> deterministic parser error.
9. `(x.name[i])(arg)` stays runtime index-then-call parse.
10. Malformed generic lists (`[`, trailing `+`, missing ident, missing `]`).
11. Trait method signatures receive stable parser ids.
12. Impl/inherent methods receive stable parser ids.

### 2 exit criteria

1. Parser accepts all locked syntax.
2. Parser disambiguation for method type args is deterministic.
3. Method bodies parse as statements.
4. Method sig/impl ids are present and stable per parse.
5. All parser tests green.

---

## Phase 3: Registry + Type Model Prerequisites

### Goal

Carry new method metadata through type model and support method generic instantiation.

### Files

- New (recommended): `lib/frontend/typecheck/resolution_artifacts.ml`
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/inherent_registry.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/trait_solver.ml`
- `lib/frontend/typecheck/builtins.ml`

### 3.1 Introduce shared key types (locked)

Add a small shared type module for checker/emitter/LSP artifacts.

Recommended shapes:

```ocaml
type expr_key = {
  file_id : string option;
  expr_id : int;
}

type callable_key =
  | UserCallable of {
      file_id : string option;
      callable_id : int;
    }
  | SyntheticCallable of string
```

Rules:

1. Do not expose bare `int` ids across checker/emitter/LSP API boundaries.
2. User-defined method signatures and method impls use parser ids from Phase 2.
3. Builtins and derive-generated methods use `SyntheticCallable`.
4. Module-system work must be able to reuse these keys unchanged in multi-file builds.

### 3.2 Extend internal method signature model

Update internal `method_sig` in registries with:

1. `method_key`
2. `method_generics`
3. `method_effect`

Update every constructor path:

1. trait declaration conversion
2. impl registration conversion
3. inherent registration conversion
4. builtin trait method definitions
5. derive-generated methods
6. tests creating method signatures directly

### 3.3 Validate impl signatures with new fields

`validate_impl_signature` must compare:

1. method name set
2. parameter count and types
3. return type
4. effect marker compatibility (`->` vs `=>`)
5. method generic arity and constraint compatibility

Parameter matching remains positional (names are not semantic for compatibility).

### 3.4 Method generic instantiation at call site

Support both implicit and explicit method type args.

Required order:

1. Bind impl-level params from receiver/impl target match.
2. Apply impl-level substitution to method signature.
3. Handle method-level params:
   - explicit type args -> bind directly
   - no explicit args -> fresh TVars
4. Apply method-level substitution.
5. Unify call arguments.
6. Enforce method-level constraints via trait solver.

### 3.5 Scope isolation rules (locked)

1. Impl-level and method-level type variables must never share namespace.
2. Use distinct fresh naming or separate scope maps.
3. No method TVar leakage into impl registry entries.

### 3.6 Callable checks for zero-arg invocation

Ensure non-callable invocation is rejected in typecheck even for zero-arg calls.

### 3 exit criteria

1. Method generics work end-to-end in typechecker.
2. Explicit method type args typecheck and influence inference.
3. Shared artifact keys exist (`expr_key`, `callable_key`) and are used by new artifacts.
4. Generic scope leakage tests are green.
5. Builtin method behavior unchanged unless intentionally updated.

---

## Phase 4: Resolution Model Switch + Typed Lookup Outcomes

### Goal

Implement new qualified-call/member resolution semantics with typed outcomes (no string-prefix branching).

### Files

- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/inherent_registry.ml`

### 4.1 Introduce typed lookup outcomes (locked)

Define typed lookup outcomes for classifier/member-resolution steps such as:

1. `Missing`
2. `Found of found_payload`
3. `Ambiguous of ambiguity_payload`
4. `ResolutionError of diagnostic_payload`

Rules:

1. Payload must include enough data for diagnostics without parsing message strings.
2. Payloads must preserve source tags needed by later lowering.
3. Outcome types must be extensible to future namespace-qualified cases, even if this plan does not populate those variants yet.

### 4.2 Receiver classifier design

Add one shared classifier used by both `FieldAccess` and `MethodCall` inference.

Requirements:

1. ordered lookup chain (extensible)
2. explicit source tags for `Value | Namespace of Module|Extern | Enum | Trait | TypeAlias`
3. no hardcoded one-off branch trees that cannot extend
4. no second semantic path that rewrites qualified dots before this classifier runs
5. future module/FFI work must be able to reuse this classifier unchanged

### 4.3 Value-dot method resolution algorithm (locked)

`x.m(args...)`:

1. infer `x` type.
2. try inherent method first.
3. if none, try trait candidates.
4. if 0 trait candidates, run field-function fallback.
5. if >1 trait candidates, emit ambiguity with qualification hints.

Important: if inherent exists, choose it; do not produce inherent-vs-trait ambiguity.

### 4.4 Qualified call paths

Implement and typecheck:

1. trait-qualified: `Trait.m(x, ...)`
2. type-qualified: `Type.m(x, ...)`
3. enum-qualified:
   - variant constructor path first
   - enum inherent-qualified path second

Compatibility hook:

4. namespace-qualified (`module`, `extern`) call variants are **not** implemented in this plan, but classifier/result types must already have a slot for them so later plans can plug in without replacing the artifact family.

### 4.5 Constrained TVar path

For receiver TVars with trait constraints:

1. expand supertraits
2. collect method providers
3. 1 provider -> resolve
4. >1 providers -> ambiguity with trait list + qualification hints

### 4.6 Remove definition-time collision rejection

Remove old registration-time errors for inherent-vs-trait method name collisions.

### 4 exit criteria

1. Inherent-first dot semantics hold.
2. Qualified call forms typecheck correctly.
3. Typed lookup outcomes replace string-prefix checks.
4. Classifier API already accommodates namespace bucket variants.
5. Collision-migration fixtures updated and green/xfail as planned.

---

## Phase 5: Call-Resolution Metadata Plumbing (Checker -> Emitter Explicit)

### Goal

Remove fragile global resolution side channel and pass typed call-resolution metadata explicitly.

### Files

- `lib/frontend/typecheck/resolution_artifacts.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/checker.ml`
- `lib/backend/go/emitter.ml`
- call sites using `Checker.check_*` + emitter APIs

### 5.1 Replace global metadata contract

Current problem: emitter reads global method-resolution store; extra inference passes can clobber it.

New contract:

1. infer/checker produce typed per-program `call_resolution_map` artifact.
2. checker result includes artifact.
3. emitter API accepts artifact explicitly.
4. emitter never reads global mutable method-resolution state.
5. artifact/API names are general (`call_resolution`, not `method_resolution`) so future namespace-qualified calls fit without another rename pass.

### 5.2 Metadata payload schema (locked)

Each call entry keyed by `expr_key` must be one of:

1. `MethodDispatch` with:
   - selected source:
     - trait method
     - inherent method
   - trait name when applicable
   - effective receiver type after substitutions
   - impl-level concrete type args used for specialization
   - method-level concrete type args used for specialization
   - runtime receiver source:
     - from dot receiver
     - from first argument (qualified forms)
2. `FieldFunctionFallback` with:
   - effective receiver type after substitutions
   - callee type after substitution when available
3. `EnumConstructor` with:
   - enum name
   - variant name
   - concrete enum type args
4. `NamespaceCall` with:
   - `namespace_kind = Module | Extern`
   - qualified member name/path payload
   - reserved now for compatibility; do not populate in this plan

### 5.3 Thread artifact through APIs

1. Extend `typecheck_result` in checker.
2. Update `check_program` and `check_string` to expose metadata artifact.
3. Update emitter entrypoint signatures to receive metadata artifact.
4. Update CLI/build pipeline call sites accordingly.

### 5.4 Add typed method-definition artifact (required for annotation-free methods)

Emitter currently requires method parameter/return annotations from AST.

To support optional annotations, add a second checker artifact:

1. typed method-definition map keyed by `callable_key` (not bare `int`).
2. payload includes:
   - parameter names
   - inferred/constrained parameter mono types
   - inferred/constrained return mono type
   - effectfulness
   - method body AST reference identity

Emitter uses this artifact as source-of-truth for method signatures.

### 5 exit criteria

1. No emitter dependency on global method-resolution store.
2. Emitter has explicit typed metadata and typed method definitions.
3. Artifact/API names are general enough that later namespace-qualified call variants fit without signature churn.
4. Existing end-to-end builds remain green.

---

## Phase 6: Emitter Refactor + Monomorphization Identity Fixes

### Goal

Use typed metadata everywhere and monomorphize method generics safely.

### Files

- `lib/backend/go/emitter.ml`
- related tests/fixtures under `codegen` and `codegen_mono`

### 6.1 Metadata-first lowering

Update all current method-call consumers:

1. `collect_insts_expr`
2. `emit_expr`
3. `collect_inherent_call_sites`

Rules:

1. Trust checker metadata for source selection.
2. Do not infer source from AST shape except as fallback for old fixtures during migration window.
3. Remove enum-constructor-vs-method heuristics that conflict with new classifier semantics.
4. Keep lowering logic shaped around `call_resolution` variants so namespace variants can plug in later.

### 6.2 Qualified call runtime argument handling

Use metadata `runtime_receiver_source`:

1. dot call -> pass receiver expr as arg0
2. qualified call -> receiver is first explicit argument from AST args

### 6.3 Use typed method-definition artifact

Replace annotation-required paths in emitter:

1. impl template registration
2. inherent method emission
3. call-site type extraction paths

Do not fail codegen on missing AST method annotations once Phase 7 starts.

### 6.4 Monomorphization identity update (locked)

Instantiation identity must include both:

1. namespace/module slot
   - current single-file builds use constant `"main"`
   - the slot must still exist now; do not design new keys/helper names that assume it can never vary
2. impl-level type args
3. method-level type args

Apply this in both specialization families:

1. trait impl method instantiation path
2. inherent method instantiation path

Caching keys and helper names must include:

1. namespace/module slot
2. method generic fingerprint
3. impl generic fingerprint

This prevents both current generic collisions and the next module-system naming rewrite.

### 6.5 Required tests

1. Dot trait call and equivalent trait-qualified call emit equivalent helper family + runtime args.
2. Dot inherent call and equivalent type-qualified call emit equivalent helper family + runtime args.
3. Enum constructor behavior remains correct.
4. Field-function fallback remains correct.
5. Method generic specialization works with impl generics (e.g., `Result[Int, Str].map[Bool]`).
6. Distinct method type args produce distinct specializations and cache keys.

### 6 exit criteria

1. Emitter no longer depends on method annotations.
2. Dot/qualified equivalence tests pass.
3. Method generic monomorphization is collision-free.
4. No backend re-resolution of method source.
5. New key/helper-name construction does not require a later module-system redesign.

---

## Phase 7: Callable Unification (One Engine + Adapters)

### Goal

Replace duplicated callable typing paths with one engine while preserving origin-specific context.

### Files

- New: `lib/frontend/typecheck/callable_model.ml`
- `lib/frontend/typecheck/infer.ml`
- (if needed) small shared type module for callable artifacts

### 7.1 One-engine model (locked)

Implement `type_callable` and route all callable forms with bodies through it.

Recommended payload shape:

```ocaml
type callable_payload = {
  generics : AST.generic_param list option;
  params : (string * AST.type_expr option) list;
  return_annotation : AST.type_expr option;
  effect_annotation : effect_annotation option;  (* Pure | Effectful | None *)
  body : AST.statement;
  origin : callable_origin;
  source_span : span;
}

and callable_origin =
  | TopLevel
  | TraitImpl of { trait_name : string; for_type : mono_type; trait_sig : Trait_registry.method_sig }
  | InherentImpl of { for_type : mono_type }
```

### 7.2 Adapter contract (important)

Adapters only gather context and call `type_callable`.

1. Top-level adapter:
   - no known param/return types
   - no known effectfulness
2. Trait impl adapter:
   - obtain substituted trait method signature
   - pass known param types **by index**
   - pass known return type
   - pass known effectfulness from trait contract
3. Inherent adapter:
   - pass known param type for receiver at index 0 from impl target
   - no forced return type unless annotation/usage constrains it

### 7.3 Known-type mapping rule (locked)

Known param types are positional (`index -> type`), not name-based.

Reason: trait signature compatibility is positional; impl parameter names are not semantic.

### 7.4 Unified behavior rules in `type_callable`

Must handle all below for every callable origin:

1. Parameter type source priority:
   - explicit annotation
   - known positional type from context
   - fresh TVar
2. Return type source priority:
   - explicit annotation
   - known return type from context
   - inferred from body
3. Effectfulness source priority:
   - explicit `=>` or `->`
   - known effectfulness from context (trait signature)
   - inferred from body
4. Purity enforcement:
   - pure callables cannot call effectful operations
5. Return consistency:
   - collect/validate return statements and final expression result
6. Method generics:
   - instantiate/freshen/enforce constraints exactly once in shared code

### 7.5 Implementation extraction plan

1. Start from existing top-level function inference logic.
2. Extract into `type_callable` with minimal semantic changes.
3. Replace top-level site with adapter call.
4. Replace trait impl site with adapter call.
5. Replace inherent impl site with adapter call.
6. Delete old duplicated method-typing branches after migration tests pass.

### 7.6 Method body shape note

This phase assumes Phase 2 already moved method bodies to `statement` in AST/parser.
If temporarily mixed shapes exist during migration, adapters may wrap expression bodies into a synthetic statement block only as an intermediate step, then remove once migration complete.

### 7 exit criteria

1. One callable typing engine exists.
2. No duplicated method-vs-function typing logic remains.
3. Trait impl/inherent method annotations are optional.
4. Purity and return checks are uniform across callable origins.
5. Existing annotated code still works (annotations optional, not forbidden).

---

## Phase 8: LSP Compatibility (No New Features)

### Goal

Keep existing LSP features correct under new AST and semantics without adding new capabilities.

### Files

- `tools/lsp/lib/doc_state.ml`
- AST walkers in:
  - `hover.ml`
  - `inlay_hints.ml`
  - `folding_ranges.ml`
  - `selection_ranges.ml`
  - `semantic_tokens.ml`
  - `signature_help.ml`

### Locked scope

1. No new request handlers.
2. No enabling dot-trigger method completion.
3. No go-to-definition feature addition.

### Required compatibility work

1. Update all AST walkers for method body statement representation and updated `MethodCall` shape.
2. If checker now returns call-resolution artifacts and typed method-definition artifacts, store them in `analysis_result` as needed for existing features.
3. Keep `doc_state` reset semantics safe with new checker artifacts.
4. Update `signature_help` resolution path:
   - first prefer checker-provided call-resolution metadata for selected source/signature
   - fallback to existing lookup if metadata missing

### Testing requirements

1. Existing LSP tests remain green.
2. Add regression tests for:
   - hover on expressions inside method bodies
   - signature help on inherent-first-changed call sites
3. Manual smoke checks after Phases 4, 6, 7:
   - hover, signature help, diagnostics stability

### 8 exit criteria

1. No LSP feature regression in currently supported features.
2. No new LSP capabilities introduced.

---

## Phase 9: De-Xfail, Documentation, and Closure

### Goal

Finish rollout, remove temporary expectations, and sync docs.

### Files

- `docs/features/functions-and-polymorphism.md`
- `docs/features/traits.md`
- `docs/features/inherent-methods.md`
- `docs/ARCHITECTURE.md`
- this plan (keep as historical record)

### Steps

1. Convert xfailed fixtures to passing fixtures as phases land.
2. Remove all `xfail` in function-model target suite.
3. Update docs to exact implemented behavior:
   - inherent-first precedence
   - qualified forms
   - method generics at declaration and call sites
   - callable consistency rules
4. Add migration notes for old collision behavior.

### 9 exit criteria

1. `xfail` count in function model areas is 0.
2. Full unit + integration suites pass.
3. Docs match implementation exactly.

---

## Junior-Friendly Step-by-Step Worklist (Exact Order)

This is the operational checklist. Execute top to bottom.

### Step A: Harness groundwork

1. Implement `xfail` parser and classification.
2. Implement counters (`PASS/FAIL/XFAIL/XPASS`) and summary.
3. Add harness canaries.
4. Run:
   - `./test/integration.sh harness`
   - `./test/integration.sh`

### Step B: Add fixtures first

1. Create `function_model` fixture directory.
2. Add first 40 fixtures covering baseline precedence + ambiguity.
3. Add next 40 fixtures covering generics and qualified calls.
4. Add final 40+ weird edge fixtures.
5. Mark expected-red fixtures with `xfail`.
6. Run:
   - `./test/integration.sh function_model`

### Step C: Syntax + AST updates

1. Add stable parser ids for trait methods and impl/inherent methods.
2. Update AST fields for method generics/effect/body/type args.
3. Update parser for method declaration generics/effects.
4. Update parser for `.name[type_args](...)` disambiguation.
5. Update parser tests.
6. Update AST pattern matches across codebase (`rg "MethodCall|impl_method_body|method_sig"`).
7. Run:
   - `make unit`

### Step D: Type model + registry updates

1. Add shared key/artifact types (`expr_key`, `callable_key`).
2. Extend internal method signatures.
3. Update conversion/registration paths.
4. Add method generic instantiation in call inference.
5. Add typed lookup outcomes.
6. Remove string-prefix branches.
7. Run:
   - `make unit`
   - `./test/integration.sh function_model traits traits_impl traits_inherent`

### Step E: Resolution switch

1. Implement shared receiver classifier chain with `Namespace` bucket.
2. Switch value-dot to inherent-first.
3. Implement qualified call paths.
4. Remove definition-time collision rejection.
5. Ensure no separate semantic rewrite path bypasses the classifier.
6. Improve diagnostics.
7. Run target fixture suites.

### Step F: Checker->emitter metadata plumbing

1. Add `call_resolution_map` artifact to checker result.
2. Add typed method-definition artifact keyed by `callable_key`.
3. Update emitter API and call sites.
4. Remove emitter reads of global method store.
5. Keep artifact/API naming general (`call_resolution`, not `method_resolution`).
6. Run unit + integration.

### Step G: Emitter parity + monomorphization

1. Use metadata-first lowering in all method call paths.
2. Move method signature source-of-truth to typed artifact.
3. Include namespace/module slot in new helper keys/names even though current builds use `"main"`.
4. Include method type args in instantiation keys/names.
5. Ensure both trait and inherent specialization paths are covered.
6. Run `codegen` and `codegen_mono` suites.

### Step H: Callable unification engine

1. Add `callable_model.ml` with `type_callable`.
2. Add top-level adapter.
3. Add trait-impl adapter.
4. Add inherent adapter.
5. Remove duplicated legacy method typing logic.
6. Run unit + integration suites.

### Step I: LSP compatibility

1. Update AST walkers.
2. Keep feature set unchanged.
3. Fix signature help method-source accuracy.
4. Run LSP tests + manual smoke checks.

### Step J: Cleanup

1. Remove remaining xfails.
2. Update docs.
3. Full test run.

---

## File-by-File Checklist

### Syntax / AST

- [ ] `lib/frontend/syntax/ast.ml`
  - add stable method ids
  - add method generic/effect fields
  - add method call type args payload
  - migrate method body to statement
- [ ] `lib/frontend/syntax/parser.ml`
  - allocate stable method ids
  - method sig generics + effect marker
  - method impl generics + effect marker + statement body
  - dot-call type args parsing with disambiguation
  - parser tests

### Typecheck

- [ ] `lib/frontend/typecheck/resolution_artifacts.ml` (new)
  - `expr_key`
  - `callable_key`
  - `call_resolution` variants
  - typed method-definition payloads
- [ ] `lib/frontend/typecheck/trait_registry.ml`
  - extend `method_sig`
  - carry `method_key`
  - impl signature validation includes effect/generics
  - typed lookup outcomes
- [ ] `lib/frontend/typecheck/inherent_registry.ml`
  - mirror typed method signatures/outcomes
- [ ] `lib/frontend/typecheck/infer.ml`
  - method generic instantiation order
  - shared receiver classifier chain
  - inherent-first resolution
  - typed lookup outcome usage
  - explicit call-resolution artifact construction
  - typed method-definition artifact construction
  - route callables through `type_callable`
- [ ] `lib/frontend/typecheck/trait_solver.ml`
  - method generic constraints enforcement support
- [ ] `lib/frontend/typecheck/builtins.ml`
  - initialize new method signature fields
  - assign synthetic callable keys where needed
- [ ] `lib/frontend/typecheck/checker.ml`
  - return new artifacts in typecheck result
- [ ] `lib/frontend/typecheck/callable_model.ml` (new)
  - one callable typing engine

### Backend emitter

- [ ] `lib/backend/go/emitter.ml`
  - consume explicit call-resolution artifact
  - consume typed method-definition artifact
  - metadata-first method lowering
  - remove annotation hard requirements for methods
  - key/helper-name construction includes namespace/module slot
  - monomorphization keys include method type args
  - tests for dot/qualified equivalence and generic specialization

### Harness + fixtures

- [ ] `test/integration.sh`
  - xfail parsing + XPASS failure + result counters
- [ ] `test/integration/common.sh`
  - summary supports xfail/xpass counters
- [ ] `test/integration/09_harness_canaries.sh`
  - canaries for PASS/FAIL/XFAIL/XPASS
- [ ] `test/fixtures/function_model/`
  - 120+ fixtures
- [ ] migrate old collision fixtures

### LSP compatibility

- [ ] `tools/lsp/lib/doc_state.ml`
  - thread new checker artifacts as needed
- [ ] `tools/lsp/lib/signature_help.ml`
  - call-resolution source/signature from metadata first
- [ ] AST walker files listed in Phase 8

### Docs

- [ ] feature docs + architecture docs + superseded plan updates

---

## Commands to Run Repeatedly

Run in this cadence every phase:

1. `make unit`
2. `./test/integration.sh function_model`
3. `./test/integration.sh traits traits_impl traits_inherent records codegen codegen_mono`
4. `./test/integration.sh` (full)

For parser-heavy changes, run unit tests immediately after each parser commit.
For emitter changes, run `codegen` and `codegen_mono` before full integration.

---

## Rollback Strategy

### General rule

Each phase should land as revertable commit(s). Do not mix unrelated phase work in one commit.

### Phase-specific guidance

1. Phases 0-1: low risk, additive. Keep.
2. Phase 2: parser/AST risk medium. If parser destabilizes broadly, revert Phase 2 as unit.
3. Phases 3-4: semantic core switch risk high. If systemic failures appear, expand xfail first; if unresolved quickly, revert whole phase commit.
4. Phase 5: plumbing risk medium-high. If emitter mismatch appears, revert plumbing commit and re-land with stricter tests.
5. Phase 6: codegen risk medium-high. Revert if helper naming/key collisions, namespace-slot mistakes, or incorrect dispatch appear.
6. Phase 7: unification risk medium. If callable inference regresses broadly, revert Phase 7 only; earlier semantic wins remain.
7. Phase 8: LSP compatibility risk low-medium. Revert isolated LSP fixes if they destabilize server.

Do not partially revert atomic semantic phases (especially Phase 4).

---

## Final Acceptance Criteria

All must be true:

1. Locked semantics implemented exactly.
2. One callable typing engine is the single source of callable body typing behavior.
3. Adapters are thin and only inject context; no duplicated typing logic remains.
4. Method generics work with constraints and explicit/inferred call-site type args.
5. Dot/qualified call emission equivalence is verified by tests.
6. Method-level and impl-level generic monomorphization is correct and collision-free.
7. Emitter does not depend on AST method annotations.
8. Trait impl and inherent method annotations are optional and validated when present.
9. Purity enforcement is consistent across callable forms with bodies.
10. Checker/emitter/LSP artifacts use stable wrapped keys, not raw bare ids.
11. Resolution artifacts are general enough that namespace-qualified call variants can plug in later without API replacement.
12. Large fixture corpus is fully green with no remaining xfail.
13. Full unit + integration suite passes.
14. LSP existing features do not regress; no new LSP capabilities were added.
