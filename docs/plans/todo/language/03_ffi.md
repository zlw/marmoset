# FFI Plan

## Maintenance

- Last verified: 2026-04-28
- Implementation status: Planning (reviewed, not started)
- Update trigger: Any extern syntax, module import resolution, call-resolution artifact, Go emitter import, or stdlib wrapper change

## Context

Marmoset compiles to Go source code. In this milestone, "FFI" means trusted declarations for Go package functions so Marmoset wrapper modules can type-check calls to those functions and the Go backend can emit package imports plus small adapter wrappers.

There is no ABI boundary, no CGO, no reflection, and no automatic Go signature discovery. An `extern` declaration is a compiler-trusted contract written by the wrapper author.

The prerequisites are now landed:

- Module system: `docs/plans/done/language/06_module-system.md`
- Prelude and core stdlib modules: `docs/plans/done/language/07_prelude.md`
- `std/prelude.mr`, `std/option.mr`, and `std/result.mr` are auto-loaded through the normal project pipeline
- Purity annotations already distinguish pure `->` and effectful `=>` function types

Current architecture matters for this plan:

- Parser output is `Surface_ast` first, then `lower.ml` creates canonical `Ast`.
- Module-qualified value calls are currently rewritten by `lib/frontend/import_resolver.ml` before typechecking.
- The current "call resolution" plumbing is still `Infer.method_resolution` keyed by expression id, not a general artifact family.
- Project emission receives merged artifacts through `Compiler.project_resolution_artifacts`; data needed by the emitter must flow through that path, not through ad hoc global registry state.

## Goals

1. Add top-level `extern` blocks for declaring Go package functions inside Marmoset wrapper modules.
2. Type-check direct extern-qualified calls such as `strings.ToUpper(s)` inside the declaring module.
3. Keep extern functions private to their declaring module. Users import exported Marmoset wrappers, not Go package qualifiers.
4. Emit deterministic Go imports and generated adapter functions for only the extern functions that are actually called.
5. Preserve Marmoset effect checking: pure wrappers cannot call effectful extern functions.
6. Keep v1 small enough to implement safely before the full stdlib milestone.
7. Establish a clear extension path for Go values that should not become raw Marmoset surface concepts, such as pointers, `any`, and channels.

## Non-Goals

1. No Go signature discovery or `go doc` integration.
2. No CGO, ABI marshaling, reflection, or runtime dynamic calls.
3. No Go methods, interfaces, channels, `any`, or variadic Marmoset surface syntax. A fixed-arity extern declaration may still call a Go variadic function when Go accepts the emitted arguments.
4. No automatic `(T, error) -> Result[T, Str]` or `(T, bool) -> Option[T]` translation in v1.
5. No first-class extern function values in v1. `let f = strings.ToUpper` is rejected; only direct call syntax is supported.
6. No migration of builtin functions or primitive trait impls out of OCaml in this milestone.
7. No raw pointer, raw channel, or untyped `Any` Marmoset surface.

## Locked Decisions

1. **Wrapper module pattern.** Extern declarations may appear in any module, but the intended API is a dedicated wrapper module that exports ordinary Marmoset functions.
2. **Top-level syntax.** `extern "go/import/path" = { fn Name(param: Type, ...) -> Type }` and `extern "path" as alias = { ... }`.
3. **Extern blocks are declarations, not imports.** Go import paths never become `Module_context.import_info`, discovery dependencies, or Marmoset module ids.
4. **Extern functions are private.** `export ToUpper` for an extern-only function is still invalid. A module must define and export a Marmoset wrapper function.
5. **Qualifier is stored once.** The effective qualifier is the explicit alias or the last import-path segment. It is stored on the extern block during parsing/lowering and not recomputed later. If the derived segment is not a plain valid identifier, the user must write `as alias`.
6. **Qualifier merging is narrow.** Multiple extern blocks may use the same qualifier only when they refer to the same Go import path and same alias. Duplicate function names under that qualifier are rejected.
7. **Namespace collision policy.** Extern qualifiers are checked against imported module namespace roots, direct imports, implicit prelude/core direct bindings, and top-level declarations in the same module. Local value bindings inside a body may shadow a qualifier in expression position.
8. **Direct calls only.** `qualifier.Function(args...)` is legal; `qualifier` as a value and `qualifier.Function` as a value are rejected with extern-specific diagnostics.
9. **V1 type mapping is deliberately narrow.** Extern parameters are limited to `Str`, `Bool`, `Float`, and `Int`. Non-`Unit` returns are limited to the same set. `Unit` is return-only in v1. `Int` means the current backend representation, Go `int64`. Go `int` adapters require later Go-type metadata.
10. **Trusted Go return handling.** If the Marmoset return type is `Unit`, the generated wrapper emits the Go call as a statement and returns `struct{}{}`, ignoring any Go returns. If the Marmoset return type is non-`Unit`, the Go call must produce one assignable Go value.
11. **Generalized call artifact now.** This milestone replaces the method-only resolution map with a `Resolution_artifacts.call_resolution` family and adds an extern-qualified-call variant. The emitter must not rediscover extern calls from syntax.
12. **Structured imports.** The emitter stops relying on substring scans such as `fmt.` and `strconv.` for main imports. Builtin and extern import needs are accumulated as structured import specs and rendered deterministically.
13. **Project-wide extern signature coherence.** The same Go import path plus function name must have exactly one Marmoset extern signature across the reachable project. If two modules declare conflicting signatures, compilation fails before codegen.
14. **Semantic adapters are explicit.** Go idioms such as `(T, error)`, `(T, bool)`, and nil returns must not silently become `Result` or `Option`. Wrapper authors choose the Marmoset API shape explicitly.
15. **No raw Go escape hatches.** Pointers, `any`, and channels are not first-class Marmoset concepts. Future support must wrap them in Marmoset-owned abstractions instead of exposing Go's raw model.

## Syntax

### Wrapper Module

```marmoset
# std/str.mr

export upcase, include?

extern "strings" = {
  fn ToUpper(s: Str) -> Str
  fn Contains(s: Str, substr: Str) -> Bool
}

fn upcase(s: Str) -> Str = {
  strings.ToUpper(s)
}

fn include?(s: Str, substr: Str) -> Bool = {
  strings.Contains(s, substr)
}
```

### Aliased Go Package

```marmoset
extern "path/filepath" as fp = {
  fn Base(path: Str) -> Str
}

fn basename(path: Str) -> Str = {
  fp.Base(path)
}
```

### Effectful Extern

```marmoset
extern "os" = {
  fn Getenv(key: Str) => Str
}

fn home() => Str = {
  os.Getenv("HOME")
}
```

The effect annotation is trusted. A wrapper author must mark any externally observable behavior as `=>`.

## V1 Type Mapping

| Marmoset | Go emitted in wrapper | Notes |
|---|---|---|
| `Str` | `string` | Direct |
| `Bool` | `bool` | Direct |
| `Float` | `float64` | Direct |
| `Int` | `int64` | Direct current backend representation |
| `Unit` | `struct{}` | Return-only in v1; wrapper returns `struct{}{}` |

Unsupported in v1:

- `Unit` parameters, `List[T]`, `Map[K, V]`, records, named wrappers, named sums, unions, intersections, `Dyn[...]`, function values, open rows, `Option`, and `Result`
- Go `int`/`uint` width adapters
- Go `error`, `any`, channels, pointers, methods, interfaces, and multiple returns as values

Transparent aliases to allowed primitive types are normalized after import/type rewriting. Transparent aliases to unsupported representations are rejected with the same unsupported-extern-type diagnostic.

The full stdlib can still start with scalar wrappers such as `std/str` and later expand the mapping when Go-side type metadata exists.

## Future Mapping Direction

This plan intentionally separates "not in v1" from "never". The long-term rule is that Marmoset should expose Marmoset semantics, not Go implementation details.

### Collections

Slices and maps can be added later when recursive FFI type mapping exists.

Required decisions before enabling them:

- `[]T` maps only when `T` maps recursively.
- `map[K]V` maps only when `K` and `V` map recursively and `K` is valid for Go map keys.
- Go slices/maps are mutable and can be nil. Marmoset collection APIs should not expose aliasing mutation by accident.
- If a Go function returns a slice/map and Marmoset exposes it as `List`/`Map`, the wrapper policy must say whether to copy, borrow read-only, or keep an opaque handle.
- Nil collection results must be explicit: either the wrapper maps nil to empty, or it exposes `Option[List[T]]` / `Option[Map[K, V]]`. There is no global nil-to-empty rule in this plan.

### Structs And Records

Simple Go structs may map to Marmoset records later only when field names and field types are fully known in the extern declaration.

Rules to decide later:

- exported Go fields only, unless generated Go wrapper code deliberately accesses unexported fields inside the same package, which normal external packages cannot do
- field order and missing-field diagnostics
- copy semantics for struct values
- no implicit mapping for embedded fields or methods in the first struct phase

### Pointers And Handles

Marmoset should not expose raw pointers.

Future pointer-facing APIs should use one of these explicit shapes:

- `Option[T]` for nullable pointer-to-value results when the wrapper chooses value-copy semantics
- an opaque nominal handle type for resources such as files, buffers, database handles, HTTP clients, or OS handles
- an effectful stdlib module that owns lifecycle operations such as open/close/read/write

Raw `*T`, pointer arithmetic, address-taking, and pointer identity are out of scope for Marmoset source.

### `any` And Interfaces

Go `any` / `interface{}` should not map to a universal Marmoset `Any`.

Future wrappers should choose one explicit representation:

- decode into a concrete Marmoset type
- decode into a closed sum such as a future JSON value type
- expose a trait object only when the Go value has a declared object-safe behavior contract
- keep it behind an opaque handle

This avoids making every use of `any` a dynamic type-system hole.

### Errors, Nil, And Presence Returns

Go error and presence idioms need explicit mapping because their meanings vary by API.

- `(T, error)` may become `Result[T, Str]`, `Result[T, E]`, panic-on-error, or an effectful wrapper-specific diagnostic policy.
- `(T, bool)` may mean optional lookup, successful parse, changed state, accepted write, or some other predicate.
- nil may mean absent, empty, default, invalid, closed, or error-dependent.

Therefore future FFI syntax may add explicit adapter declarations, but this milestone does not add silent global conversions. In v1, wrapper modules expose `Result` and `Option` by writing ordinary Marmoset APIs around supported scalar externs, or by waiting for an explicit adapter phase.

### Concurrency

Go channels and goroutines should not appear raw in Marmoset signatures.

Future concurrency should come through Marmoset-owned abstractions such as `Task`, `Stream`, `Mailbox`, or another stdlib design. Go channel interop can live behind those modules later, but `chan T` is not part of the user-facing FFI type mapping.

## Implementation Phases

### Phase F0: Syntax, Surface AST, Lowering

**Goal:** Parse extern blocks and preserve them through surface and lowered ASTs. No typechecking or codegen yet.

**Changes:**

- `lib/frontend/syntax/token.ml`: add `Extern` keyword.
- `lib/frontend/syntax/surface_ast.ml`: add `SExternBlock` plus positioned extern package/function/parameter nodes.
- `lib/frontend/syntax/ast.ml`: add canonical `ExternBlock`.
- `lib/frontend/syntax/lower.ml`: lower surface extern declarations into `AST.ExternBlock`.
- `lib/frontend/syntax/parser.ml`: add a dedicated extern parser, not `parse_function_parameters`, because extern params must be named and typed.
- Exhaustive AST consumers that walk top-level statements, including derive expansion, synthetic-id/codegen collection, and debug/printer helpers, must deliberately preserve, ignore, or reject `ExternBlock` during this syntax-only phase.

Preferred canonical shape:

```ocaml
type extern_block_def = {
  extern_go_path : string;
  extern_alias : string option;
  extern_qualifier : string;
  extern_fns : extern_fn_sig list;
}

and extern_fn_sig = {
  extern_fn_name : string;
  extern_fn_params : extern_param list;
  extern_fn_return_type : type_expr;
  extern_fn_effectful : bool;
  extern_fn_pos : int;
  extern_fn_end_pos : int;
}

and extern_param = {
  extern_param_name : string;
  extern_param_type : type_expr;
}
```

**Parser rules:**

- `extern` is top-level only and uses the normal declaration separator before its body: `extern "path" [as alias] = { ... }`.
- It counts as a body declaration for header ordering: `export` and `import` must appear before it.
- A default qualifier derived from the import path must be a plain identifier; otherwise parsing/lowering reports that an explicit `as alias` is required.
- Function signatures inside the block require:
  - no body
  - no generics in v1
  - every parameter named and annotated
  - explicit `->` or `=>`
  - explicit return type
- Alias and Go function names must be valid plain identifiers without `?` or `!` suffixes in v1.

**Tests:**

- Lex `extern`.
- Parse basic block, aliased block, multiple functions, and multiple blocks for the same package.
- Reject missing param annotation, missing arrow, missing return type, function body, generic extern function, and positional type-only params.
- Reject invalid derived qualifiers unless `as alias` is present, and reject aliases or function names with `?`/`!` suffixes.
- Lowering test proves path, alias, qualifier, spans, param types, return type, and effect flag survive.
- Discovery test proves `extern "fmt" = { ... }` does not try to load `fmt.mr`.

**Gate:** `make unit` green.

### Phase F1: Extern Metadata, Scope, and Call Artifacts

**Goal:** Validate extern declarations, make direct extern calls type-check, and record structured call metadata for the emitter.

**Execution slices:**

1. **F1a: Mechanical call-resolution migration only.**
   - Move the existing `Infer.method_resolution` variants into `Resolution_artifacts.call_resolution`.
   - Keep behavior unchanged.
   - Update `Checker.typecheck_result`, `Compiler.project_resolution_artifacts`, LSP helpers, and emitter parameters to use the new type.
   - Focused verification: existing unit tests in `lib/frontend/typecheck`, `lib/frontend`, `lib/backend/go`, and `tools/lsp/lib`.
2. **F1b: Extern declaration registry.**
   - Add registry storage and snapshots.
   - Register `ExternBlock` declarations.
   - Validate duplicates, qualifier/path/function consistency, unsupported types, and transparent alias normalization.
   - Do not type-check extern calls yet.
3. **F1c: Extern call typechecking.**
   - Add the extern namespace branch to dotted-call inference.
   - Enforce direct-call-only behavior.
   - Record `ExternQualifiedCall` plus effectfulness during inference.
4. **F1d: Module/import integration.**
   - Rewrite extern signature types in `import_resolver`.
   - Validate qualifier collisions against imports, implicit bindings, and declarations.
   - Merge extern artifacts through the compiler project.

Do not combine F1a with extern semantics. The artifact migration is risky enough to deserve its own green checkpoint.

**New or changed modules:**

- `lib/frontend/typecheck/resolution_artifacts.ml`
  - Introduce `call_resolution` and move existing method variants out of `Infer.method_resolution`.
  - Add `ExternQualifiedCall of extern_call_key`.
- `lib/frontend/typecheck/extern_registry.ml`
  - Store extern metadata inside `Infer.inference_state`, or otherwise clear/snapshot it from the same Checker/Infer prepare path used by standalone `Checker.check_program`; do not rely only on `Compiler.reset_module_state`.
  - Validate duplicate qualifier/path/function rules.
  - Validate project-wide signature coherence for the same Go path plus function.
  - Validate allowed v1 types.
  - Snapshot declarations and used calls for `Checker.typecheck_result`.
- `lib/frontend/typecheck/checker.ml`
  - Replace `call_resolution_map : (int, Infer.method_resolution) Hashtbl.t` with `(int, Resolution_artifacts.call_resolution) Hashtbl.t`.
  - Add explicit `extern_declarations : extern_func StringMap.t` and `extern_calls : (int, extern_call) Hashtbl.t` snapshots.
- `lib/frontend/compiler.ml`
  - Clear extern state in `reset_module_state`.
  - Merge extern declarations/calls into `project_resolution_artifacts`.
  - Pass extern artifacts to codegen.
- `lib/frontend/import_resolver.ml`
  - Rewrite type annotations inside `ExternBlock` using the same `rewrite_type_expr` path as other declarations.
  - Ignore extern declarations for module exports/declarations.
  - Check extern qualifier collisions against resolved imports and current module declarations.
- `lib/frontend/typecheck/infer.ml`
  - During top-level registration, register extern blocks before checking bodies.
  - Extend the dotted receiver classifier with an extern namespace case after value/local shadowing and before falling through to real field/method access.
  - Type-check direct `qualifier.Function(args...)` calls against the extern signature.
  - Record `ExternQualifiedCall` for the call expression id.
  - Make effectfulness available during inference: either `record_call_resolution` carries it immediately, or extern calls also record into the existing effectful-call store until the full migration is complete.
  - Reject `qualifier` and `qualifier.Function` value use in v1.

Required artifact payloads:

```ocaml
type extern_func = {
  extern_key : string;              (* stable key: go_path + go_func_name *)
  declaring_module : string;
  go_path : string;
  source_qualifier : string;        (* qualifier accepted in Marmoset source *)
  go_import_alias : string;         (* deterministic generated Go alias for this path *)
  go_func_name : string;
  param_names : string list;
  param_types : Types.mono_type list;
  return_type : Types.mono_type;
  is_effectful : bool;
  source_span : Diagnostics.Diagnostic.span;
}

type extern_call = {
  call_func_key : string;
  call_arg_types : Types.mono_type list;
  call_return_type : Types.mono_type;
  call_effectful : bool;
}
```

The generated Go import alias is canonical per import path, for example `mext_strings` or `mext_path_filepath`. It is independent of the Marmoset source qualifier so identical extern functions declared through different source aliases can still deduplicate safely.

**Scope invariants:**

- Extern metadata is module-local during typechecking.
- Importing a wrapper module never imports its Go package qualifier.
- Wrapper functions are ordinary Marmoset declarations and are the only exported API.
- Local value bindings shadow extern qualifiers only inside their lexical value scope.

**Effect invariants:**

- An extern signature with `=>` has an effectful function type.
- Pure functions cannot call effectful externs.
- A pure extern may be called from pure or effectful wrappers.
- The call artifact carries effectfulness so emitter and later LSP/features do not infer it from syntax.

**Tests:**

- Unit: extern package/function metadata registers and snapshots per module.
- Unit: duplicate qualifier/path/function diagnostics.
- Unit: conflicting project-wide signatures for the same Go path/function are rejected.
- Unit: unsupported v1 types are rejected in params and returns.
- Unit: `Unit` parameters are rejected, while `Unit` return is accepted.
- Unit: transparent aliases to primitives normalize, but aliases to unsupported types are rejected.
- Unit: `strings.ToUpper("x")` has type `Str`; `strings.Contains("abc", "b")` has type `Bool`.
- Unit: wrong arity and wrong argument type diagnostics.
- Unit: unknown extern function diagnostic.
- Unit: `let f = strings.ToUpper` and `strings` as a value are rejected.
- Unit: pure wrapper calling effectful extern is rejected.
- Unit: effectful-call validation sees extern effectfulness during inference, not only after Checker snapshots.
- Compiler/import tests:
  - extern qualifier does not create a module dependency
  - extern names are not exportable
  - wrapper module can call its extern, importing module cannot
  - extern alias collides with module alias/direct import/top-level declaration with clear diagnostics
  - extern alias collides with implicit core names such as `Option` or `Result`
  - type annotations in extern signatures see prelude and direct imports

**Gate:** `make unit` green.

### Phase F2: Go Import and Wrapper Emission

**Goal:** Compile extern calls to Go through generated wrappers and structured imports.

**Execution slices:**

1. **F2a: Structured import builder.**
   - Replace the final import substring scan with an explicit import accumulator.
   - Preserve current builtin import behavior first.
   - Snapshot current generated Go for representative existing fixtures before adding externs.
2. **F2b: Extern type-to-Go lowering.**
   - Add `extern_type_to_go` for only the v1 scalar mapping.
   - Add unit tests for unsupported types at the codegen boundary even though typecheck should reject them earlier.
3. **F2c: Wrapper emission.**
   - Emit wrappers for used extern calls only.
   - Use stable wrapper names based on Go path plus function name after signature coherence validation.
   - Use deterministic generated Go import aliases in wrapper bodies.
4. **F2d: Extern call lowering.**
   - Lower `ExternQualifiedCall` artifacts to wrapper calls.
   - Add exact generated-Go snapshots for imports, aliases, wrappers, unused externs, and duplicate-basename packages.

**Emitter changes (`lib/backend/go/emitter.ml`):**

1. Replace substring-based import detection with structured import accumulation.
   - Builtin runtime needs still add `fmt`, `reflect`, or other builtin imports explicitly.
   - Extern calls add their Go package path and deterministic generated Go alias.
   - Imports are sorted deterministically by path and alias.
2. Add `extern_type_to_go` for the v1 mapping. Do not reuse broader `type_to_go` for unsupported types.
3. Emit one wrapper per used extern function identity:
   - identity is based on full Go import path plus Go function name after signature-coherence validation, not local qualifier
   - wrapper name uses existing Go-safe escaping helpers
   - unused extern declarations emit no wrapper and no import
4. Lower `ExternQualifiedCall` artifacts to wrapper calls.
5. For `Unit` return, emit the Go call as a statement and return `struct{}{}`:

```go
func extern__fmt__Println(s string) struct{} {
    mext_fmt.Println(s)
    return struct{}{}
}
```

6. For non-`Unit` return, emit `return pkg.Func(args...)` and rely on `go build` to validate the trusted contract.

**Wrapper naming:**

- Use the full Go import path in the generated wrapper name, for example `extern__path_filepath__Base`.
- The wrapper body calls the deterministic generated Go import alias for the path, not the Marmoset source qualifier.
- The source qualifier only controls which dotted name the Marmoset typechecker accepts.
- Two different import paths with the same basename must not collide.

**Generated output example:**

Source:

```marmoset
export upcase

extern "strings" = {
  fn ToUpper(s: Str) -> Str
}

fn upcase(s: Str) -> Str = {
  strings.ToUpper(s)
}
```

Output shape:

```go
package main

import mext_strings "strings"

func extern__strings__ToUpper(s string) string {
    return mext_strings.ToUpper(s)
}

func std__str__upcase_string(s string) string {
    return extern__strings__ToUpper(s)
}
```

The exact module wrapper function name follows the existing monomorphized module naming scheme.

**Tests:**

- Integration: `strings.ToUpper` wrapper compiles and runs.
- Integration: `strings.Contains` wrapper compiles and returns `Bool`.
- Integration: alias import `extern "path/filepath" as fp = { fn Base(path: Str) -> Str }`.
- Integration/snapshot: unused extern does not emit wrapper or import.
- Snapshot: two packages with the same basename do not collide in wrapper names.
- Snapshot: import block is sorted and aliases render correctly.
- Integration: generated Go compiles with `go build`.
- Negative integration: trusted signature mismatch surfaces as a `build-go-compile` diagnostic.

**Gate:** focused FFI integration plus `make unit` green.

### Phase F3: Multi-Module Wrapper Flow and Privacy

**Goal:** Prove wrapper modules work as normal public APIs and extern qualifiers stay private.

**Tests:**

- `lib/str.mr` declares `extern "strings" = { ... }` and exports `upcase`; `main.mr` imports `lib.str` and calls `str.upcase("hi")`.
- `main.mr` cannot call `strings.ToUpper("hi")` unless it declares its own extern block.
- Two wrapper modules can use the same Go package/function only when their extern signatures match exactly; imports and wrappers are deduplicated for matching declarations.
- Two wrapper modules can use different source aliases for the same package without wrapper-name or Go import alias collision.
- Module import alias and extern alias collisions produce origin-rich diagnostics.

**Gate:** `make unit && make integration ffi` green.

### Phase F4: Docs, Fixtures, and Hardening

**Goal:** Document the trusted FFI contract and lock the edge cases.

**Changes:**

- Add `docs/features/ffi.md`.
- Update `docs/INDEX.md` so the new feature doc is discoverable.
- Update `docs/ARCHITECTURE.md` around Go backend and stdlib/FFI boundaries.
- Update `docs/ROADMAP.md` after implementation lands.
- Add fixture group `test/fixtures/ffi/`.
- Add `test/integration/13_ffi.sh` and wire explicit selector support through `test/integration.sh`:
  - `FFI_SUITE="$INTEGRATION_DIR/13_ffi.sh"`
  - `ffi`, `13_ffi`, and `13_ffi.sh` resolve to `__FFI__`
  - `all` includes `__FFI__`
  - a `run_ffi` branch executes the suite
- Keep exact generated-Go assertions in the existing snapshot suite or call the snapshot helper from `13_ffi.sh`; do not leave wrapper/import assertions only as fixture stdout tests.

**Edge-case tests:**

- duplicate package alias
- duplicate function name
- extern qualifier vs module alias
- extern qualifier vs direct import
- extern qualifier vs top-level declaration
- local value shadowing of qualifier inside a function
- unsupported type diagnostics for `List`, `Map`, records, `Dyn`, `Option`, and `Result`
- `Unit` parameter rejected
- transparent alias to primitive accepted; transparent alias to unsupported type rejected
- unknown extern function
- extern function used as first-class value
- effectful extern inside pure wrapper

**Gate:** `make unit && make integration ffi && make integration snapshots` green. Full integration remains a final user-run gate unless explicitly requested.

## Commit Plan

1. Syntax data model: token, surface AST, canonical AST, lowering, pass-through statement walkers.
2. Syntax parser: extern parser, header ordering, invalid qualifier/name diagnostics, discovery non-dependency test.
3. Mechanical call-resolution migration: no extern semantics, existing tests green.
4. Extern registry: signature validation, v1 type policy, transparent alias policy, duplicate/coherence checks.
5. Extern call inference: direct call typechecking, effect tracking, first-class use rejection.
6. Module integration: import resolver type rewriting, qualifier collision diagnostics, compiler artifact merge.
7. Structured imports: replace substring scan while preserving existing generated Go behavior.
8. Wrapper codegen: extern wrappers, deterministic import aliases, extern call lowering, snapshots.
9. Multi-module privacy: wrapper module fixtures and cross-module rejection tests.
10. Docs and harness: `docs/features/ffi.md`, `docs/INDEX.md`, `docs/ARCHITECTURE.md`, roadmap, `13_ffi.sh` selector wiring.

Each commit should keep focused tests green before moving to the next slice.

## Execution Guardrails For Implementation

This plan should be implementable by a medium-reasoning model if it follows these guardrails:

1. Start each slice by adding one focused failing test for the behavior in that slice.
2. Do not edit emitter code during F0/F1 except for type signature fallout from the mechanical artifact migration.
3. Do not add stdlib wrapper modules until extern syntax, typecheck, and codegen are green on small fixtures.
4. Keep the first call-resolution commit behavior-preserving. If generated Go changes in that commit, stop and investigate.
5. Keep unsupported type diagnostics in typecheck, not codegen. Codegen checks are defensive only.
6. When a slice changes shared artifacts, update all snapshot/lookup consumers in the same commit: compiler, emitter, LSP definition/completion/signature-help helpers, and tests.
7. Prefer exact diagnostics over broad failure strings for negative fixtures.
8. Run focused tests after each slice:
   - syntax/lowering: `dune runtest lib/frontend/syntax`
   - registry/typecheck: `dune runtest lib/frontend/typecheck`
   - compiler/import integration: `dune runtest lib/frontend`
   - emitter: `dune runtest lib/backend/go`
   - LSP artifact fallout: `dune runtest tools/lsp/lib`
   - fixtures: `make integration ffi`
   - generated Go snapshots: `make integration snapshots`

## Risks

1. **Artifact migration touches shared codegen paths.** Keep the first commit mechanical: move existing method variants into `Resolution_artifacts.call_resolution` before adding extern semantics.
2. **Extern trusted signatures can lie.** Typecheck validates Marmoset-side use; Go build remains the authority for Go-side assignability.
3. **Import aliasing can collide subtly.** Store import path, effective qualifier, and alias separately, and key wrappers by full path plus function name.
4. **Type mapping creep.** Reject unsupported types loudly in F1 instead of generating Go that happens to compile for a narrow example.
5. **Module/private boundary leaks.** Keep extern declarations out of module signatures and exports; only wrapper functions enter the public surface.

## Critical Files

| File | Role |
|---|---|
| `lib/frontend/syntax/token.ml` | Add `Extern` keyword |
| `lib/frontend/syntax/surface_ast.ml` | Add positioned surface extern declarations |
| `lib/frontend/syntax/ast.ml` | Add canonical `ExternBlock` |
| `lib/frontend/syntax/parser.ml` | Parse extern block/signature syntax |
| `lib/frontend/syntax/lower.ml` | Lower surface externs |
| `lib/frontend/import_resolver.ml` | Rewrite extern signature types and validate qualifier collisions |
| `lib/frontend/typecheck/resolution_artifacts.ml` | Generalize call resolution and add extern call metadata |
| `lib/frontend/typecheck/extern_registry.ml` | New extern declaration/use registry |
| `lib/frontend/typecheck/infer.ml` | Register externs and type-check extern-qualified calls |
| `lib/frontend/typecheck/checker.ml` | Snapshot extern/call artifacts |
| `lib/frontend/compiler.ml` | Reset, merge, and pass extern project artifacts |
| `lib/backend/go/emitter.ml` | Structured imports, wrapper emission, extern call lowering |
| `test/fixtures/ffi/**` | End-to-end fixtures |
| `test/integration/13_ffi.sh` | Focused FFI integration suite |
| `docs/features/ffi.md` | User-facing feature documentation |

## Progress

- 2026-04-28 16:25 CEST: Codex-only plan review started after moving completed tooling plans from `todo` to `done`; Claude review intentionally skipped per request.
- 2026-04-28 16:31 CEST: Parallel Codex review agents reported stale assumptions around prelude status, surface/lower parsing, missing generalized call artifacts, module-local extern metadata, over-broad type mapping, import generation, and fixture numbering.
- 2026-04-28 16:40 CEST: Plan revised to match the current compiler pipeline, narrow v1 type mapping, require a real call-resolution artifact migration, specify module-local extern privacy, and add concrete tests/docs/commit boundaries.
- 2026-04-28 16:47 CEST: Second Codex-only review completed; plan patched to resolve `Unit` parameter policy, fixed-arity variadic/ignored-return wording, explicit extern artifact payloads, registry lifetime, canonical Go import aliases, default qualifier validation, transparent alias policy, and `13_ffi.sh` selector wiring.
- 2026-04-28 17:07 CEST: F0 syntax/lowering test-first slice started; scope is lexer keyword, surface/core AST extern nodes, parser/lowering preservation, and syntax-only pass-through updates.
- 2026-04-28 17:08 CEST: User syntax correction accepted; extern blocks now require `=` before `{` to match other top-level declaration forms.
- 2026-04-28 17:13 CEST: F0 syntax/lowering implementation green; `dune build @all`, `dune runtest lib/frontend/syntax`, and `make unit` pass.
- 2026-04-28 17:14 CEST: Commit `d545f18` created for F0 extern syntax, parser/lowering preservation, discovery non-dependency coverage, and syntax-only pass-through fallout.
- 2026-04-28 17:16 CEST: F1a mechanical call-resolution migration started; target is moving existing method-call variants into `Resolution_artifacts.call_resolution` with no extern semantics.
- 2026-04-28 17:22 CEST: F1a mechanical migration green; `dune runtest lib/frontend/typecheck`, `dune runtest lib/frontend`, `dune runtest lib/backend/go`, `dune runtest tools/lsp/lib`, and `dune build @all` pass.
- 2026-04-28 17:23 CEST: F1a shared call-resolution artifacts and mechanical consumer updates committed.
