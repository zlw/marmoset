# Module System Plan

## Maintenance

- Last verified: 2026-04-04
- Implementation status: Implemented (focused verification green)
- Update trigger: Any module/import/export syntax or compilation model change

## Context

Marmoset currently supports single-file compilation only. The pipeline is: source.mr → Lexer → Parser → Typechecker → Go Emitter → `go build`. The current registries and lookup tables still reflect the pre-module single-file world (enum, trait, impl, grouped type-extension, and transparent-type metadata that still uses some alias-like internal names) and are global mutable Hashtbl values keyed by flat strings. Identifiers are still `Identifier of string` at the AST surface, there is no namespace-aware module scope model yet, and there is no multi-file compilation orchestration. The Go backend emits a single `package main` with one `main.go`.

This plan adds a module system as the first of two milestones (modules, then FFI). Some decisions are flagged as FFI-relevant.

### Key Pre-Module Status
- Symbol IDs / identifier tracking: **PARTIALLY DONE** — basic symbol IDs exist already; the remaining blocker is namespace-aware scope/binding spaces rather than raw symbol allocation.
- All other P0 blockers: Done or partially done.
- `module_path` field exists in `mono_state` but is locked to `"main"` with a guard.
- Function-model rework now defines the canonical qualifier classifier direction and wrapped artifact-key direction (`expr_key`, `callable_key`, future-generalized `call_resolution`). Module work should extend that machinery, not replace it.

---

## Locked Decisions

1. **File = module** (implicit). No `module` keyword. `math.mr` defines module `math`. `collections/list.mr` defines module `collections.list`.
2. **`.` is the universal qualifier** — field access, callable-field/direct-interface surfaces, named-sum constructors, module access, and later extern-qualified access. Parser stays syntactic; one central checker classifier resolves semantics.
3. **`import` keyword** with `.`-separated paths. One import per line in v1. Aliasing with `as` supported. `import a.b.c` is parsed as a raw dotted path and resolved semantically later:
   - if `a.b.c` is a module path, it is a namespace import,
   - else if `a.b` is a module path and `c` is one of its exported members, it is a direct import,
   - else it is an error,
   - if both interpretations are valid, it is an ambiguity error.
4. **`export` list at top of file** for visibility. No per-declaration keywords. Everything not exported is private. Export statement is optional (no export = all private).
5. **No wildcard imports.** Every imported name must be listed explicitly.
6. **No multi-import `{...}` syntax in v1.** Sugar for later.
7. **Single Go package** for all modules. Flatten into one `package main`. Module-qualified mangling with `__` separator (`math__add_int64`).
8. **Circular dependencies disallowed.** Error at dependency graph construction time.
9. **Bail-on-first error** per module (consistent with diagnostics plan).
10. **Per-module compilation from day one.** Each module type-checked independently. Module signatures passed to downstream modules. No combined-AST shortcut — clean architecture for incremental compilation and LSP.
11. **No orphan rule in v1 modules.** A module may define `impl Trait[Type]` even if it owns neither the trait nor the type.
12. **Build membership = transitive import closure from the entry module.** Files on disk that are not reachable through imports are not part of the build and do not affect semantics.
13. **Trait impls are open-world, direct-import visible, and globally coherent.**
14. **Type `impl` blocks are open-world grouped extensions, direct-import visible, and locally ambiguous.**
15. **Neither kind of impl is transitively re-exported in v1.** If module `A` imports an impl module, that does not make those impls visible in module `B`.
16. **Top-level runtime initialization executes once in module dependency order.** Do not inherit Go package-var/init semantics as the language model.

### Binding Spaces

Modules introduce multiple binding spaces. The checker must not model everything as "value in `type_env`".

Each module scope conceptually contains separate maps for:
- values
- modules / namespaces
- named types (constructor-bearing and transparent)
- traits
- shapes

These may be threaded as one record with distinct fields rather than as separate unrelated parameters.

Visibility rules:
- `import math` introduces a module binding only.
- `import collections.list` introduces the leaf module binding `list`.
- `import math.add` introduces a value binding only.
- `import math.Point` introduces a named-type binding only.
- `import math.Show` introduces a trait binding only.
- `import math.HasXY` introduces a shape binding only.
- Local value bindings shadow imported module/type/trait/shape names in expression position.

### Trait Impl Coherence Model

This plan intentionally adopts **no orphan rule + global build-wide coherence with direct-import visibility**.

Rationale:
- Marmoset's transparent exact types are structural again, so nominal "type ownership" is not a reliable coherence boundary for exact structural receivers.
- A Rust-style orphan boundary would fit constructor-bearing nominal wrappers and sums better than transparent exact structural types, and would therefore be an awkward mismatch for the language's current data model.
- Full build-global visibility would keep resolution simple, but it would make imports poor documentation because behavior could appear in a file only because some other file imported an impl module elsewhere.

Operational policy:
- Any module may declare `impl Trait[Type]`.
- Build-wide overlap / duplicate checking is global across all reachable modules in the build.
- Trait impl usability is local to the current module: a file may use trait impls defined in:
  - the current module,
  - modules it directly imports.
- Trait impl visibility is not re-exported transitively through intermediate modules.
- The current module's visible trait impl set is used for:
  - concrete trait constraint solving,
  - qualified trait calls when a concrete witness is needed,
  - `Dyn[...]` witness packaging,
  - derive validation and expansion for imported traits/types,
  - emitter specialization/helper selection for code proven in that module.

Conflict policy:
- Concrete impl identity is keyed by canonical `(trait, receiver-type)` pairs.
- Generic impl identity and overlap checks are also keyed by canonicalized receiver/type-head structure.
- Transparent exact types collapse to their canonical receiver type for impl identity and conflict checking; alias/module ownership is not a separate coherence slot.
- Constructor-bearing wrappers and sums keep their nominal identity.
- Shapes do not own impls and are never coherence owners.
- Duplicate or overlapping impls that can both satisfy the same concrete receiver are deterministic build errors with origin-rich diagnostics.

Implementation consequence:
- Per-module checking still applies to value/type/shape/trait visibility.
- Trait-impl overlap checking is handled with a build-wide pre-scan and global registry.
- Trait-impl usability in a given module is still filtered by that module's directly imported impl modules.

### Type `impl` Extension Model

`impl Type = { ... }` is not treated like trait coherence evidence. In a structural/data-first language it behaves more like an import-scoped grouped extension surface.

Operational policy:
- Any module may declare `impl Type = { ... }`.
- A file may use grouped type-extension members defined in:
  - the current module,
  - modules it directly imports.
- Type `impl` visibility is not re-exported transitively.
- Type `impl` lookup does not participate in trait solving or `Dyn[...]`.

Conflict policy:
- Extension identity is keyed by canonical `(receiver-type, member-name)` pairs.
- Lookup for `Type.member(...)` collects candidates only from the current module and directly imported modules visible in the current file.
- `0` candidates → unknown member error.
- `1` candidate → success.
- `>1` candidates → local ambiguity error with origin-rich diagnostics.

Implementation consequence:
- Transparent exact types collapse to their canonical structural receiver type for grouped-extension lookup.
- Constructor-bearing wrappers and sums keep their nominal identity.
- Type-grouped extensions are intentionally more like explicit open classes / extension groups than owner-only inherent methods.

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
import collections.list         # nested module: list.map(...)
import collections.list as l    # aliased namespace: l.map(...)
import instances.show_point     # direct-imports trait/type impls from that module into this file only

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

### Import Resolution Rules

`import a.b.c` is intentionally resolved semantically, not syntactically:
- If `a/b/c.mr` exists and is a module, the import is a namespace import of module `a.b.c`.
- Else if `a/b.mr` exists and module `a.b` exports `c`, the import is a direct import of member `c` from module `a.b`.
- Else the import is an error.
- If both interpretations are valid, the import is an ambiguity error and the user must rename one side or use a less ambiguous path.

**FFI-relevant:** The `import` keyword and `.`-separated paths establish the convention that FFI declarations will also follow.

---

## Implementation Phases

### Phase M0: Namespace-Aware Scope And Resolution Foundation
**Goal:** Establish the binding/scope substrate that modules will build on.

**Changes:**
- Keep the existing symbol-ID machinery, but stop treating it as the missing blocker.
- Introduce a scope record for per-module checking, for example:
  ```ocaml
  type module_scope = {
    value_bindings : poly_type StringMap.t;
    module_bindings : namespace_info StringMap.t;
    type_bindings : named_type_ref StringMap.t;
    trait_bindings : trait_ref StringMap.t;
    shape_bindings : shape_ref StringMap.t;
  }
  ```
- Generalize checker/emitter-facing resolution artifacts from the current method-only side channel to a namespace-aware `call_resolution` family that can represent module-qualified access without replacing the API shape later.
- Thread scope lookup through the central dotted classifier instead of using value env membership as a proxy for all names.
- Define local shadowing and lookup rules explicitly for values/modules/types/traits/shapes.

**Files:** `lib/frontend/typecheck/infer.ml`, `lib/frontend/typecheck/checker.ml`,
`lib/frontend/typecheck/resolution_artifacts.ml` (and helper modules if split out)

**Tests:**
- Unit tests for separate binding spaces and shadowing
- Unit tests for dotted classification with values vs modules vs traits vs types
- Existing symbol-table tests still pass unchanged

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

(* Import declaration: raw path, resolved semantically later *)
| ImportDecl of {
    import_path: string list;         (* ["math"; "add"] or ["collections"; "list"] *)
    import_alias: string option;      (* Some "Pt" for import math.Point as Pt *)
  }
```

No new expression kinds needed — `.` already parses as `FieldAccess` and `MethodCall`. The checker classifier introduced by the function-model rework is the semantic source of truth for deciding whether `math.add(...)` is a namespace-qualified call vs another dot form.

**Parser changes** (`lib/frontend/syntax/parser.ml`):
- Parse `export name1, name2, ...` as `ExportDecl`
- Parse `import a.b.c` as raw-path `ImportDecl` (dot-separated path only)
- Parse `import a.b.c as alias` with `As` keyword
- Validate: `export` must be the first statement if present; imports follow it

No semantic distinction between module import and direct import is made in the parser. That is resolved later by the import resolver.

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
- Project root = directory containing entry file
- `import math` → `./math.mr` relative to project root
- `import collections.list` → `./collections/list.mr` relative to project root
- `import a.b.c` tries:
  1. module path `./a/b/c.mr`
  2. member import from module `./a/b.mr` with exported member `c`
  3. ambiguity error if both succeed

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
1. Pre-scan the full build for trait/type/shape definitions and impl headers needed for:
   - global trait coherence,
   - grouped-extension collision indexing,
   - module signature extraction
2. Resolve the current module's imports against upstream module signatures
3. Build the module's initial scope from:
   - builtins,
   - imported module signatures,
   - directly imported trait-impl modules visible in this file,
   - directly imported type-extension modules visible in this file,
   - build-wide trait-coherence metadata
4. Run derive expansion using that imported context
5. Run `infer_program` on the expanded module AST alone
6. Extract a `module_signature` from the results (exported values, nominal named types, transparent `type` declarations, shapes, traits, trait impls, type extensions)
7. Store the signature for use by downstream modules

**New type: Module Signature** (new file `lib/frontend/typecheck/module_sig.ml`):
```ocaml
type module_signature = {
  module_id : string;
  values : (string * poly_type) list;         (* exported let bindings *)
  types : (string * named_type_info) list;    (* exported nominal named types, including sums and wrappers *)
  transparent_types : (string * type_alias_info) list;  (* exported `type Name = TypeExpr` declarations *)
  shapes : (string * shape_def) list;         (* exported open structural shapes *)
  traits : (string * trait_def) list;         (* exported trait definitions *)
  trait_impls : trait_impl_entry list;        (* visible only to modules that directly import this module *)
  type_impls : type_impl_entry list;          (* visible only to modules that directly import this module *)
}
```

Note: values, nominal named types, transparent `type` declarations, shapes, and traits participate in explicit export. Trait impls and type impls are not ordinary exported names, but they also do not become globally visible merely because the module is in the build; they become usable only in modules that directly import the defining module.

**Key changes:**

1. **Refactor `infer_program` to accept external environment:**
   ```ocaml
   val infer_program :
     ?scope:module_scope ->        (* pre-populated with imported names in separate binding spaces *)
     ?external_types:named_type_info list ->
     ?external_transparent_types:type_alias_info list ->
     ?external_shapes:shape_def list ->
     ?external_traits:trait_def list ->
     ?visible_trait_impls:trait_impl_entry list ->
     ?visible_type_impls:type_impl_entry list ->
     ?global_trait_coherence:trait_coherence_state ->
     AST.program -> infer_result
   ```
   The existing `infer_program` clears all registries and starts fresh. The refactored version accepts pre-populated imported scope plus imported registry state and visible impl sets. The clear calls move to a `reset_all` function called once per full build, then each module *adds* its definitions.

2. **Per-module registries:**
   - Each module's `infer_program` call populates registries with its own definitions (module-prefixed keys)
   - Before body checking any module, do a whole-build pre-scan for trait/type/shape definitions and trait-impl headers needed for global coherence
   - Before calling `infer_program` for a module, inject:
     - imported module signature entries for ordinary visibility,
     - directly imported trait impls and type impls for this file,
     - the build-wide trait-coherence metadata needed for duplicate/overlap checks
   - Run derive expansion after imports have been resolved and imported traits/types are visible
   - After `infer_program` completes, extract the module's signature from the registry state + type_env
   - Private names are NOT included in the signature → downstream modules can't see them
   - Neither trait impls nor type impls are transitively re-exported

3. **Import resolution + namespace environment** (new `lib/frontend/import_resolver.ml`):
   - For each module, resolve `import` declarations against upstream signatures
   - Verify imported names exist and are exported where export visibility applies
   - Build the module's initial scope record:
     - `import math` → bind `"math"` as a namespace binding
     - `import collections.list` → bind `"list"` as a namespace binding
     - `import math.add` → bind `"add"` in the value space
     - `import math.Point as Pt` → bind `"Pt"` in the named-type space
   - Record directly imported modules whose trait impls and type impls are visible in this file
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
- Cross-module trait: define trait in A, impl in B, import impl module in C, and verify the impl is usable only in C
- Cross-module transparent type: define `type Users = List[User]` in A, use in B
- Cross-module shape: define `shape HasXY = { x: Int, y: Int }` in A, use in B
- Export enforcement: error when importing a non-exported name
- Isolation: module B cannot access module A's private names (type checker doesn't see them)
- Direct import: `import math.add` then bare `add(1, 2)` works
- Aliased import: `import math.Point as Pt` then `Pt` works as type name
- Import ambiguity: error when `import a.b.c` could mean either module `a.b.c` or member `c` from `a.b`
- Imported trait derive: derive an imported trait for an imported type in a third module
- Type extension visibility: `Type.member(...)` is usable only when the extension-defining module is directly imported
- Type extension ambiguity: two directly imported modules defining the same `Type.member` produce a local ambiguity error
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
- Runtime top-level code is emitted as explicit per-module init helpers and executes exactly once in module-topological dependency order
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
6. **Import ambiguity** — `import a.b.c` intentionally permits two semantic interpretations. Mitigate with a deterministic resolution order and a hard ambiguity error when both interpretations are valid.
7. **Trait impl visibility vs coherence** — trait impl usability is local, but overlap checking is global. Mitigate by explicitly separating "visible trait impls for this file" from "global coherence state for the build".
8. **Open-world impl conflict risk** — because foreign trait impls are allowed, adding a reachable dependency can introduce duplicate or overlapping impls elsewhere in the build. Mitigate with canonical impl keys, whole-build overlap checks, and origin-rich diagnostics.
9. **Type-extension ambiguity** — open-world `impl Type = { ... }` can produce conflicting `Type.member` groups. Mitigate with direct-import-only visibility and local ambiguity errors instead of pretending there is one global winner.

---

## Verification

After each phase:
1. `make unit` — all inline tests pass
2. `make integration` — all integration suites pass
3. New module-specific tests added per phase (see test sections above)
4. Multi-module coherence tests cover:
   - foreign trait for foreign type is accepted,
   - duplicate/overlapping impls across different modules fail deterministically,
   - trait impl imports affect local availability but not global duplicate detection,
   - type-extension imports affect local availability and can trigger local ambiguity.
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

---

## PROGRESS

- 2026-04-03: Started implementation.
- 2026-04-03: Completed Phase M1 parser/header surface and Phase M2 discovery/graph groundwork.
- 2026-04-03: Completed Phase M3 per-module orchestration. Added `lib/frontend/import_resolver.ml`, `lib/frontend/typecheck/module_sig.ml`, and `lib/frontend/compiler.ml` to resolve imports semantically, rewrite module-visible names to stable internal names, extract typed module signatures, seed direct-import visibility, and type-check reachable modules in topological order.
- 2026-04-03: Completed Phase M3.5 multi-module codegen wiring. The CLI now builds/runs/checks from an entry file through discovery, keeps the locked single-file fast path for programs without module headers, emits one flattened Go `package main`, and preserves direct-import-only impl visibility plus non-transitive export visibility.
- 2026-04-03: Completed Phase M4 hardening for the current scope. Added module fixture corpora under `test/fixtures/modules/`, `test/fixtures/modules_edge/`, `test/fixtures/modules_codegen/`, and `test/fixtures/modules_codegen_edge/`, normalized them to the locked "entry directory = project root" rule, taught the integration harness to treat `modules*` groups as entrypoint suites (`main*.mr` only), and added exact generated-Go snapshots for module namespace/direct import mangling, trait/inherent qualification, and Go-keyword collision cases.
- 2026-04-03: Imported shape bindings now resolve correctly in type position through the registered shape registry, which was required for direct shape imports and shape aliases to type-check across module boundaries.
- 2026-04-03: Kept the locked-plan semantics even where the old single-file pipeline pushed toward shortcuts. In particular:
  - namespace-vs-direct import ambiguity is resolved semantically and rejected when both interpretations exist,
  - direct-imported impl visibility is local to the importing module,
  - impl visibility is not re-exported transitively,
  - local value bindings still shadow imported module/type/trait/shape names in expression position.
- 2026-04-03: Focused verification completed:
  - `dune runtest --root /Users/zlw/src/marmoset/marmoset lib/frontend --force`
  - `dune runtest --root /Users/zlw/src/marmoset/marmoset tools/lsp/lib --force`
  - `./test/integration.sh modules modules_edge modules_codegen modules_codegen_edge snapshots`
- Caveats and findings:
  - `infer_program` now exposes `~prepare_state` for the module compiler, but shared-environment runs still rely on the caller keeping any env-borne constrained generic obligations intact; the compiler path avoids that pitfall by using fresh per-module inference state and explicitly seeded registries/env.
  - Module fixtures need support modules colocated under the entry fixture's directory tree because discovery intentionally treats the entry file's directory as the project root.
  - The new emitted-Go snapshots are especially useful for modules because runtime output alone does not catch name-mangling or qualification regressions.
- 2026-04-04 02:32 CEST: Started follow-up refactor to finish the plan's "clean architecture for incremental compilation and LSP" boundary. Current slices:
  - extract a compiler-owned entry analysis API that returns parsed-graph plus typed-project artifacts without making LSP re-parse/re-check,
  - migrate `tools/lsp/lib/doc_state.ml` to consume compiler analysis instead of mixing a standalone checker path with a diagnostics-only module path,
  - keep current LSP surface behavior stable where module environments still use internalized compiler names, while exposing the richer compiler result for future navigation/hover/completion work.
- 2026-04-04 02:36 CEST: Compiler slice green. `lib/frontend/compiler.ml` now owns entry analysis for both standalone files and module projects, returns active-file surface AST plus typed artifacts, snapshots per-module user generic names, and keeps `check_entry*` as wrappers over the new boundary. Focused verification:
  - `dune runtest --root /Users/zlw/src/marmoset/marmoset lib/frontend --force`
- 2026-04-04 02:37 CEST: LSP adapter slice green. `tools/lsp/lib/doc_state.ml` now consumes the compiler-owned analysis for both standalone and module files and stores the raw compiler result for future navigation/hover/completion work. Current policy stays conservative for module files: diagnostics and surface AST come from compiler analysis, but `type_map`/`environment` exposure remains off until the LSP side translates compiler-internal names for editor-facing features. Focused verification:
  - `dune runtest --root /Users/zlw/src/marmoset/marmoset tools/lsp/lib --force`
- 2026-04-04 02:38 CEST: Combined focused verification green after both slices together:
  - `dune runtest --root /Users/zlw/src/marmoset/marmoset lib/frontend --force`
  - `dune runtest --root /Users/zlw/src/marmoset/marmoset tools/lsp/lib --force`
