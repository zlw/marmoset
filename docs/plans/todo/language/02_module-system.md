# Module System Plan

## Maintenance

- Last verified: 2026-04-03
- Implementation status: Planning (not started)
- Update trigger: Any module/import/export syntax or compilation model change

## Context

Marmoset currently supports single-file compilation only. The pipeline is: source.mr → Lexer → Parser → Typechecker → Go Emitter → `go build`. The current registries and lookup tables still reflect the pre-module single-file world (enum, trait, impl, inherent, and transparent-type metadata that still uses some alias-like internal names) and are global mutable Hashtbl values keyed by flat strings. Identifiers are `Identifier of string` with no qualified names, no symbol table, and no multi-file awareness. The Go backend emits a single `package main` with one `main.go`.

This plan adds a module system as the first of two milestones (modules, then FFI). Some decisions are flagged as FFI-relevant.

### Key Pre-Module Status
- P0-8 (Symbol/ID model): **NOT STARTED** — critical blocker. No symbol table, no qualified names, global registries need scoping.
- All other P0 blockers: Done or partially done.
- `module_path` field exists in `mono_state` but is locked to `"main"` with a guard.
- Function-model rework now defines the canonical qualifier classifier and wrapped artifact-key direction (`expr_key`, `callable_key`, `call_resolution`). Module work should extend that machinery, not replace it.

---

## Locked Decisions

1. **File = module** (implicit). No `module` keyword. `math.mr` defines module `math`. `collections/list.mr` defines module `collections.list`.
2. **`.` is the universal qualifier** — field access, callable-field/direct-interface surfaces, named-sum constructors, module access, and later extern-qualified access. Parser stays syntactic; one central checker classifier resolves semantics.
3. **`import` keyword** with `.`-separated paths. One import per line in v1. Aliasing with `as` supported.
4. **`export` list at top of file** for visibility. No per-declaration keywords. Everything not exported is private. Export statement is optional (no export = all private).
5. **No wildcard imports.** Every imported name must be listed explicitly.
6. **No multi-import `{...}` syntax in v1.** Sugar for later.
7. **Single Go package** for all modules. Flatten into one `package main`. Module-qualified mangling with `__` separator (`math__add_int64`).
8. **Circular dependencies disallowed.** Error at dependency graph construction time.
9. **Bail-on-first error** per module (consistent with diagnostics plan).
10. **Per-module compilation from day one.** Each module type-checked independently. Module signatures passed to downstream modules. No combined-AST shortcut — clean architecture for incremental compilation and LSP.
11. **No orphan rule in v1 modules.** A module may define `impl Trait[Type]` even if it owns neither the trait nor the type.
12. **Coherence is build-wide, not import-scoped.** All impls from all modules in the build participate in one global impl set. Imports do not select between competing impl worlds.
13. **No scoped impl imports / local impl worlds in v1.** Trait satisfaction and dispatch must not depend on which helper module happened to be imported.

### Trait Impl Coherence Model

This plan intentionally adopts **no orphan rule + global build-wide coherence**.

Rationale:
- Marmoset's transparent exact types are structural again, so nominal "type ownership" is not a reliable coherence boundary for exact structural receivers.
- A Rust-style orphan boundary would fit constructor-bearing nominal wrappers and sums better than transparent exact structural types, and would therefore be an awkward mismatch for the language's current data model.
- Import-scoped impl worlds would preserve flexibility, but they would make trait satisfaction, `Dyn[...]` packaging, and qualified dispatch import-sensitive in ways that are difficult to reason about and difficult to compile predictably.

Operational policy:
- Any module may declare `impl Trait[Type]`.
- All impls discovered anywhere in the build participate in one global coherence set.
- Imports/exports do not gate impl visibility and do not choose between competing impls.
- The same global impl set is used for:
  - trait constraint solving,
  - qualified trait calls,
  - `Dyn[...]` witness packaging,
  - derive validation and expansion,
  - emitter specialization/helper selection.

Conflict policy:
- Concrete impl identity is keyed by canonical `(trait, receiver-type)` pairs.
- Generic impl identity and overlap checks are also keyed by canonicalized receiver/type-head structure.
- Transparent exact types collapse to their canonical receiver type for impl identity and conflict checking; alias/module ownership is not a separate coherence slot.
- Constructor-bearing wrappers and sums keep their nominal identity.
- Shapes do not own impls and are never coherence owners.
- Duplicate or overlapping impls that can both satisfy the same concrete receiver are deterministic build errors with origin-rich diagnostics.

Implementation consequence:
- Per-module checking still applies to value/type/shape/trait visibility.
- Impl visibility is handled separately through a build-wide pre-scan and global registry injection, not through ordinary import/export visibility.

---

## Syntax Design

```
# math.mr

export add, pi, Point, Color, Points, HasXY, Drawable

fn add(x: Int, y: Int) -> Int = x + y
let pi = 3.14159
fn helper(x: Int) -> Int = x * 2    # private (not in export list)

type Point = { x: Int, y: Int }
type Color = { Red, Green, Blue }     # canonical sum surface; `enum` remains compatibility sugar
type Points = List[Point]
shape HasXY = { x: Int, y: Int }
trait Drawable[a] = { fn draw(self: a) -> Str }
```

```
# main.mr

import math                     # namespace: math.add(1, 2), math.Point, math.Color.Red
import math.add                 # direct: add(1, 2)
import math.Point               # direct: Point available as type name
import math.Point as Pt         # aliased: Pt instead of Point
import math.Points              # direct: Points available as transparent type name
import math.HasXY               # direct: HasXY available as shape name
import collections.list         # nested module: collections.list.map(...)
import collections.list as l    # aliased namespace: l.map(...)

math.add(1, 2)                  # qualified call
add(1, 2)                       # unqualified (if directly imported)
let p: Point = { x: 1, y: 2 }   # direct-imported transparent record type
let c = math.Color.Red          # qualified named-sum constructor
```

**Unified Qualified-Call Classifier:** `.` is used for field access, callable-field calls, named-sum constructors, module access, trait-qualified calls, exact-type qualified calls, and later extern package access. The checker uses one centralized classifier and one `call_resolution` artifact family. This is the canonical reference — other plans defer to this order.

Given `a.b` or `a.b(args)` where `a` is a bare identifier:

| Priority | Check | Result | Example |
|----------|-------|--------|---------|
| 1 | Is `a` a value binding in scope? | Infer type, then field access, callable-field call, or interface field projection | `record.field`, `runner.run(1)` |
| 2 | Is `a` a namespace binding? | Namespace-qualified access | `math.add(1, 2)`, future: `fmt.Println(s)` |
| 3 | Is `a` a named-sum type? | Sub-resolve (see below) | `Option.Some(42)`, `Result.map(r, f)` |
| 4 | Is `a` a trait name? | Trait-qualified call | `Show.show(x)` |
| 5 | Is `a` another named type? | Exact-type qualified call | `Point.distance(p)`, `UserId.show(id)` |
| 6 | None of the above | Error: unknown identifier | |

**Namespace bucket:** In this plan, namespace means imported module bindings. In the later FFI plan, extern qualifiers join this same bucket. They do not get a separate later precedence tier.

Shapes participate in import/export and type-position resolution, but they do not create callable/member namespaces for `a.b`. Named types may participate in `Type.member` lookup when they have constructors or explicit exact-type impl entries. That qualification is explicit namespace lookup, not value-dot fallback.

**Named-sum sub-resolution (priority 3):** When `a` is a named-sum type (declared canonically with `type` or via `enum` sugar), check `b`:
- Is `b` a variant of named sum `a`? → constructor (`Option.Some(42)`, `Option.None`)
- Is `b` an exact-type grouped function on type `a`? → exact-type qualified call (`Result.map(r, f)`)
- Neither → error: unknown variant or method

**Key rules:**
- Value bindings always win (priority 1) — a local variable shadows a module/type/trait name, and once `a` is a value there is no fallback UFCS search from `a.b(...)`
- Named-sum variants are checked before exact-type grouped functions — constructors take priority within the named-sum namespace
- This order is stable across all plans and must be implemented as one centralized classifier, not duplicated rewrite-time and typecheck-time resolution paths

**FFI-relevant:** The `import` keyword and `.`-separated paths establish the convention that FFI declarations will also follow.

---

## Implementation Phases

### Phase M0: Symbol ID Model Foundation
**Goal:** Establish minimal symbol infrastructure that modules will build on.

This is P0-8 Phase 1 from the existing problem doc. Not the full multi-pass architecture — just enough to have stable, unique symbol IDs.

**Changes:**
- Add `symbol` type to `infer.ml` (or new `symbol_table.ml`):
  ```ocaml
  type symbol_id = int
  type symbol_kind = TopLevelLet | LocalLet | Param | TypeSym | TransparentTypeSym | ShapeSym | TraitSym | ...
  type symbol = { id: symbol_id; name: string; kind: symbol_kind;
                  definition_pos: int; definition_end_pos: int; file_id: string option }
  ```
- Create global `symbol_table: (symbol_id, symbol) Hashtbl.t`
- During inference, register symbols when bindings are created
- Map `(expr.id, symbol_id)` for identifier resolution tracking

**Files:** `lib/frontend/typecheck/infer.ml` (or new `lib/frontend/typecheck/symbol_table.ml`)

**Tests:** Unit tests that symbols are created for let bindings, function params, named types, transparent `type` declarations, and shapes. Verify symbol IDs are unique within a file.

**Gate:** All existing unit + integration tests pass unchanged.

---

### Phase M1: Syntax — `import`, `export`, Qualified Access
**Goal:** Parser understands module syntax. Single-file programs still work unchanged.

**Token additions** (`lib/frontend/syntax/token.ml`):
- `Import` keyword
- `Export` keyword
- `As` keyword

**Lexer changes** (`lib/frontend/syntax/lexer.ml`):
- Add `import`, `export`, `as` to keyword lookup

**AST additions** (`lib/frontend/syntax/ast.ml`):
```ocaml
(* Export declaration — must be first statement if present *)
| ExportDecl of string list   (* export add, pi, Point *)

(* Import declaration *)
| ImportDecl of {
    import_module: string list;       (* ["math"] or ["collections"; "list"] *)
    import_name: string option;       (* Some "add" for import math.add, None for import math *)
    import_alias: string option;      (* Some "Pt" for import math.Point as Pt *)
  }
```

No new expression kinds needed — `.` already parses as `FieldAccess` and `MethodCall`. The checker classifier introduced by the function-model rework is the semantic source of truth for deciding whether `math.add(...)` is a namespace-qualified call vs another dot form.

**Parser changes** (`lib/frontend/syntax/parser.ml`):
- Parse `export name1, name2, ...` as `ExportDecl`
- Parse `import a.b.c` as `ImportDecl` (dot-separated path)
- Parse `import a.b.c as alias` with `As` keyword
- Validate: `export` must be first non-import statement (or first statement entirely)

**Tests:**
- Unit: lex `import`, `export`, `as`
- Unit: parse `export add, Point, Color`
- Unit: parse `import math`, `import math.add`, `import math.Point as Pt`
- Unit: parse `import collections.list`, `import collections.list as l`
- Integration: all existing single-file tests pass (no import/export → no behavior change)

**Gate:** `make unit && make integration` green.

---

### Phase M2: Module Discovery and Multi-File Parsing
**Goal:** Given an entry file, discover all dependencies, parse them, build a dependency graph.

**New files:**
- `lib/frontend/discovery.ml` — file resolution and recursive discovery
- `lib/frontend/module_context.ml` — `module_graph`, topological sort, cycle detection

**Types:**
```ocaml
type parsed_module = { module_id: string; file_path: string;
                       source: string; program: AST.program;
                       exports: string list; imports: import_info list }
type module_graph = { modules: (string, parsed_module) Hashtbl.t;
                      topo_order: string list; entry_module: string }
```

**File resolution rules:**
- `import math` → `./math.mr` relative to entry file directory
- `import collections.list` → `./collections/list.mr` relative to project root
- Project root = directory containing entry file

**Expression ID collision fix:** Parser's `next_id` becomes file-scoped with offset ranges. File 0: IDs 0–999,999. File 1: IDs 1,000,000–1,999,999. Passed via `~id_offset` parameter to parser. Wrapped artifact keys continue to include `file_id`, consistent with the function-model rework.

**CLI changes** (`bin/main.ml`): `build`/`run` commands still take single entry .mr file. Loader discovers imports transitively.

**Tests:**
- Unit: discovery finds transitive deps
- Unit: cycle detection produces clear error message with cycle path
- Unit: topological sort is correct
- Unit: expression IDs don't collide across files

**Gate:** `make unit && make integration` green.

---

### Phase M3: Per-Module Type Checking with Signatures
**Goal:** Type-check each module independently. Extract module signatures. Pass signatures to downstream modules.

**Architecture:** For each module in topological order:
1. Pre-scan the full build for trait/type/shape definitions and impl headers needed for global coherence
2. Build the module's initial `type_env` from builtins + imported module signatures, while separately injecting the build-wide impl/coherence registry state
3. Run `infer_program` on the module's AST alone
4. Extract a `module_signature` from the results (exported values, nominal named types, transparent `type` declarations, shapes, traits, impls)
5. Store the signature for use by downstream modules
6. Ordinary names remain explicitly imported/exported; impl coherence metadata follows the separate build-wide visibility rule above

**New type: Module Signature** (new file `lib/frontend/typecheck/module_sig.ml`):
```ocaml
type module_signature = {
  module_id : string;
  values : (string * poly_type) list;         (* exported let bindings *)
  types : (string * named_type_info) list;    (* exported nominal named types, including sums and wrappers *)
  transparent_types : (string * type_alias_info) list;  (* exported `type Name = TypeExpr` declarations *)
  shapes : (string * shape_def) list;         (* exported open structural shapes *)
  traits : (string * trait_def) list;         (* exported trait definitions *)
  impls : impl_entry list;                    (* all impls (always build-visible; not export-gated) *)
}
```

Note: trait `impl` blocks are always visible across modules and are not export-gated. This is not Rust's orphan-rule model. Marmoset allows foreign-trait-for-foreign-type impls, but requires one coherent build-wide impl set. Values, nominal named types, transparent `type` declarations, shapes, and traits still participate in explicit export.

**Key changes:**

1. **Refactor `infer_program` to accept external environment:**
   ```ocaml
   val infer_program :
     ?env:type_env ->              (* pre-populated with imported signatures *)
     ?external_types:named_type_info list ->
     ?external_transparent_types:type_alias_info list ->
     ?external_shapes:shape_def list ->
     ?external_traits:trait_def list ->
     ?external_impls:impl_entry list ->
     AST.program -> infer_result
   ```
   The existing `infer_program` clears all registries and starts fresh. The refactored version accepts pre-populated registry state from imported signatures plus the build-wide impl/coherence pre-scan. The clear calls move to a `reset_all` function called once per full build, then each module *adds* its definitions.

2. **Per-module registries:**
   - Each module's `infer_program` call populates registries with its own definitions (module-prefixed keys)
   - Before body checking any module, do a whole-build pre-scan for trait/type/shape definitions and impl headers needed for global coherence
   - Before calling `infer_program` for a module, inject:
     - imported module signature entries for ordinary visibility,
     - the build-wide impl registry and related coherence metadata for trait resolution
   - After `infer_program` completes, extract the module's signature from the registry state + type_env
   - Private names are NOT included in the signature → downstream modules can't see them
   - Impl visibility remains build-wide even when other declarations remain import/export-gated

3. **Import resolution + namespace environment** (new `lib/frontend/import_resolver.ml`):
   - For each module, resolve `import` declarations against upstream signatures
   - Verify imported names exist and are exported
   - Build the module's initial scope/environment:
     - `import math` → bind `"math"` as a namespace binding in scope
     - `import math.add` → bind `"add"` → `math__add` in type_env
     - `import math.Point as Pt` → bind `"Pt"` → `math__Point` in type_env
   - Expose namespace metadata to the central checker classifier so `math.add(...)` classifies as a namespace-qualified call/member access
   - Optional lowering/desugaring after classification may rewrite already-resolved direct imports to internal names
   - Do **not** use AST rewrite as the semantic source of truth for dotted qualification
   - Rewrite type-position names: directly imported `Point`, `Points`, `HasXY` → module-prefixed internal names

4. **Module-qualified registry keys:** Named-type, trait, impl, transparent-type, and shape registries use prefixed keys:
   - `"math__point"` instead of `"point"`
   - `"math__color"` instead of `"color"`
   - Builtins remain unqualified

5. **Module-qualified mangling in emitter** (`lib/backend/go/emitter.ml`):
   - Remove `module_path` guard (line 236-238)
   - `mangle_func_name` adds module prefix: `math__add_int64`
   - `__` as module separator (single `_` already used for type suffixes)

6. **Project-level orchestrator** (new `lib/frontend/compiler.ml` or in `checker.ml`):
   ```ocaml
   val compile_project : module_graph -> (compiled_project, diagnostic) result
   ```
   Iterates modules in topological order, checks each, accumulates signatures, feeds them to the emitter.

**Files modified:**
- New: `lib/frontend/typecheck/module_sig.ml` — module signature type + extraction
- New: `lib/frontend/import_resolver.ml` — resolve imports, build namespace env, optional post-classification lowering
- New: `lib/frontend/compiler.ml` — project-level orchestrator
- `lib/frontend/typecheck/resolution_artifacts.ml` — shared wrapped keys + `call_resolution` family reused from function-model rework
- `lib/frontend/typecheck/infer.ml` — accept external env/registries, don't auto-clear
- `lib/frontend/typecheck/enum_registry.ml` — evolve existing sum/type registry logic to per-module named-sum population + query
- `lib/frontend/typecheck/trait_registry.ml` — per-module population + query
- `lib/frontend/typecheck/annotation.ml` — prefixed named-type / transparent-type / shape lookup
- `lib/frontend/typecheck/checker.ml` — per-module check entry point
- `lib/backend/go/emitter.ml` — remove guard, update mangling, accept multiple checked modules
- `bin/main.ml` — wire up full multi-module pipeline

**Tests:**
- Two-module programs: module A exports function, module B calls it
- Cross-module named sum: define `type Color = { ... }` or `enum Color = { ... }` in A, construct/match in B
- Cross-module trait: define trait in A, impl in B (impls auto-visible)
- Cross-module transparent type: define `type Users = List[User]` in A, use in B
- Cross-module shape: define `shape HasXY = { x: Int, y: Int }` in A, use in B
- Export enforcement: error when importing a non-exported name
- Isolation: module B cannot access module A's private names (type checker doesn't see them)
- Direct import: `import math.add` then bare `add(1, 2)` works
- Aliased import: `import math.Point as Pt` then `Pt` works as type name
- Name collision: two modules with same-named private function → no collision (not in signatures)
- Full integration: multi-file .mr programs compile to Go, build, run with correct output

**Gate:** `make unit && make integration` green.

---

### Phase M3.5: Codegen for Multiple Modules
**Goal:** Emit Go code from multiple independently-checked modules.

**Strategy:** All checked modules still flatten into one Go `package main` (locked decision #7). The emitter receives all module signatures + ASTs + type_maps and produces one Go output.

**Key changes:**
- Emitter accepts a list of `checked_module` records instead of a single program
- Monomorphization runs whole-program: collect all instantiations across all modules, emit specialized functions once
- Non-main module top-level let bindings execute before main's code (emitted at top of `func main()` in dependency order, or as package-level `var` declarations)
- Module-prefixed function names in Go output: `math__add`, `math__identity_string`

**Files:** `lib/backend/go/emitter.ml`, `bin/main.ml`

**Gate:** `make unit && make integration` green.

---

### Phase M4: Polish and Hardening
**Goal:** Edge cases, error quality, documentation.

- Shadowing rules: local names shadow imported names, with optional warning
- Error messages: "module not found", "circular dependency", "not exported", "ambiguous name"
- Standard library module / prelude handling
- Multi-import sugar: `import math.{add, pi}` (v2 convenience)
- Re-export support (v2)
- Update docs: ARCHITECTURE.md, feature docs, ROADMAP.md
- Update test infrastructure: `expect_multi_module` helper in `common.sh`

---

## Go Output Example

**math.mr:**
```
export add, identity

fn add(x: Int, y: Int) -> Int = x + y
fn identity[a](x: a) -> a = x
```

**main.mr:**
```
import math

puts(math.add(1, 2))
puts(math.identity("hello"))
```

**Generated Go (single main.go):**
```go
package main

import "fmt"

func math__add(x int64, y int64) int64 { return (x + y) }
func math__identity_string(x string) string { return x }

func main() {
    fmt.Println(math__add(int64(1), int64(2)))
    fmt.Println(math__identity_string("hello"))
}
```

---

## Relationship to FFI (Future Milestone)

Decisions made here that affect FFI:
1. **`import` keyword** — FFI may use `import extern "go/fmt"` or similar
2. **`.`-separated paths** — FFI module paths use same convention
3. **`export` list** — FFI-exposed functions must be in export list
4. **Single Go package** — extern functions compile as Go function stubs in same package
5. **`__` mangling** — extern names get special mangling or `//go:linkname` directives

These are flagged but NOT designed here. FFI is a separate plan.

---

## Risks

1. **Expression ID collision across files** — mitigated by per-file ID offset ranges in Phase M2
2. **`infer_program` refactor scope** — the biggest risk. Currently monolithic with global mutable state. Refactoring to accept external env/registries touches the core of type checking. Mitigated by: keeping the internal inference logic unchanged, only changing how state is initialized and extracted.
3. **Named-type / constructor collision across modules** — mitigated by module-prefixed registry keys
4. **`.` disambiguation** — resolved by the unified classifier defined in the Syntax Design section. Value bindings win first, then namespace bindings, then named sums (variants before exact-type grouped functions), then traits, then other named types. Shapes do not create callable/member namespaces. Later extern qualifiers join the namespace bucket rather than introducing a second precedence rule.
5. **Monomorphization across modules** — emitter runs whole-program across all checked modules. `collect_insts_stmt` sees all call sites from all modules.
6. **Trait impl visibility** — impls must be visible across modules even without explicit import. With no orphan rule, the per-module approach must inject one build-wide impl/coherence set into every module check rather than treating impls as ordinary imported names.
7. **Open-world impl conflict risk** — because foreign impls are allowed, adding a dependency can introduce duplicate or overlapping impls elsewhere in the build. Mitigate with canonical impl keys, whole-build overlap checks, and origin-rich diagnostics. No import-scoped impl selection is allowed as an escape hatch.

---

## Verification

After each phase:
1. `make unit` — all inline tests pass
2. `make integration` — all integration suites pass
3. New module-specific tests added per phase (see test sections above)
4. Multi-module coherence tests cover:
   - foreign trait for foreign type is accepted,
   - duplicate/overlapping impls across different modules fail deterministically,
   - imports do not affect impl availability or impl selection.
5. Manual verification: write a 2-3 module program, compile, run, verify output

---

## Critical Files

| File | Role |
|------|------|
| `lib/frontend/syntax/ast.ml` | AST types: add ExportDecl, ImportDecl |
| `lib/frontend/syntax/token.ml` | Add Import, Export, As tokens |
| `lib/frontend/syntax/lexer.ml` | Lex new keywords |
| `lib/frontend/syntax/parser.ml` | Parse import/export/as syntax |
| `lib/frontend/typecheck/resolution_artifacts.ml` | Shared wrapped keys + `call_resolution` artifact family |
| `lib/frontend/typecheck/infer.ml` | Symbol table, registry clearing, prefixed env |
| `lib/frontend/typecheck/enum_registry.ml` | Evolve current sum/type registry logic to module-prefixed named-sum keys |
| `lib/frontend/typecheck/trait_registry.ml` | Module-prefixed keys |
| `lib/frontend/typecheck/annotation.ml` | Prefixed named-type / transparent-type / shape lookup |
| `lib/frontend/typecheck/checker.ml` | Per-module check entry point |
| `lib/backend/go/emitter.ml` | Remove guard, module-prefixed mangling, compile_modules |
| `bin/main.ml` | Wire up loader + multi-module pipeline |
| **New:** `lib/frontend/discovery.ml` | File resolution, dependency discovery |
| **New:** `lib/frontend/module_context.ml` | Module graph, topo sort, cycle detection |
| **New:** `lib/frontend/import_resolver.ml` | Resolve imports, build namespace env, optional post-classification lowering |
| **New:** `lib/frontend/typecheck/module_sig.ml` | Module signature type + extraction |
| **New:** `lib/frontend/compiler.ml` | Project-level orchestrator |
