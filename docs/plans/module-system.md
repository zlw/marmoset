# Module System Plan

## Maintenance

- Last verified: 2026-03-07
- Implementation status: Planning (not started)
- Update trigger: Any module/import/export syntax or compilation model change

## Context

Marmoset currently supports single-file compilation only. The pipeline is: source.mr → Lexer → Parser → Typechecker → Go Emitter → `go build`. All registries (enum, trait, impl, inherent, type alias) are global mutable Hashtbl values keyed by flat strings. Identifiers are `Identifier of string` with no qualified names, no symbol table, and no multi-file awareness. The Go backend emits a single `package main` with one `main.go`.

This plan adds a module system as the first of two milestones (modules, then FFI). Some decisions are flagged as FFI-relevant.

### Key Pre-Module Status
- P0-8 (Symbol/ID model): **NOT STARTED** — critical blocker. No symbol table, no qualified names, global registries need scoping.
- All other P0 blockers: Done or partially done.
- `module_path` field exists in `mono_state` but is locked to `"main"` with a guard.
- Function-model rework now defines the canonical qualifier classifier and wrapped artifact-key direction (`expr_key`, `callable_key`, `call_resolution`). Module work should extend that machinery, not replace it.

---

## Locked Decisions

1. **File = module** (implicit). No `module` keyword. `math.mr` defines module `math`. `collections/list.mr` defines module `collections.list`.
2. **`.` is the universal qualifier** — field access, method calls, enum constructors, module access, and later extern-qualified access. Parser stays syntactic; one central checker classifier resolves semantics.
3. **`import` keyword** with `.`-separated paths. One import per line in v1. Aliasing with `as` supported.
4. **`export` list at top of file** for visibility. No per-declaration keywords. Everything not exported is private. Export statement is optional (no export = all private).
5. **No wildcard imports.** Every imported name must be listed explicitly.
6. **No multi-import `{...}` syntax in v1.** Sugar for later.
7. **Single Go package** for all modules. Flatten into one `package main`. Module-qualified mangling with `__` separator (`math__add_int64`).
8. **Circular dependencies disallowed.** Error at dependency graph construction time.
9. **Bail-on-first error** per module (consistent with diagnostics plan).
10. **Per-module compilation from day one.** Each module type-checked independently. Module signatures passed to downstream modules. No combined-AST shortcut — clean architecture for incremental compilation and LSP.

---

## Syntax Design

```
# math.mr

export add, pi, point, color, drawable

fn add(x: Int, y: Int) -> Int = x + y
let pi = 3.14159
fn helper(x: Int) -> Int = x * 2    # private (not in export list)

type Point = { x: Int, y: Int }
enum Color = { Red, Green, Blue }
trait Drawable[a] = { fn draw(x: a) -> Str }
```

```
# main.mr

import math                     # namespace: math.add(1, 2), math.Point, math.Color.Red
import math.add                 # direct: add(1, 2)
import math.Point               # direct: Point available as type name
import math.Point as Pt         # aliased: Pt instead of Point
import collections.list         # nested module: collections.list.map(...)
import collections.list as l    # aliased namespace: l.map(...)

math.add(1, 2)                  # qualified call
add(1, 2)                       # unqualified (if directly imported)
let p: Point = { x: 1, y: 2 }   # direct-imported type
let c = math.Color.Red          # qualified enum constructor
```

**Unified Qualified-Call Classifier:** `.` is used for field access, method calls, enum constructors, module access, trait-qualified calls, inherent-qualified calls, and later extern package access. The checker uses one centralized classifier and one `call_resolution` artifact family. This is the canonical reference — other plans defer to this order.

Given `a.b` or `a.b(args)` where `a` is a bare identifier:

| Priority | Check | Result | Example |
|----------|-------|--------|---------|
| 1 | Is `a` a value binding in scope? | Infer type, then field access or method call | `x.show()`, `record.field` |
| 2 | Is `a` a namespace binding? | Namespace-qualified access | `math.add(1, 2)`, future: `fmt.Println(s)` |
| 3 | Is `a` an enum name? | Sub-resolve (see below) | `Option.Some(42)`, `Result.map(r, f)` |
| 4 | Is `a` a trait name? | Trait-qualified call | `Show.show(x)` |
| 5 | Is `a` a type alias name? | Inherent-qualified call | `Point.distance(p)` |
| 6 | None of the above | Error: unknown identifier | |

**Namespace bucket:** In this plan, namespace means imported module bindings. In the later FFI plan, extern qualifiers join this same bucket. They do not get a separate later precedence tier.

**Enum sub-resolution (priority 3):** When `a` is an enum name, check `b`:
- Is `b` a variant of enum `a`? → enum constructor (`Option.Some(42)`, `Option.None`)
- Is `b` an inherent method on type `a`? → inherent-qualified call (`Result.map(r, f)`)
- Neither → error: unknown variant or method

**Key rules:**
- Value bindings always win (priority 1) — a local variable shadows a module/enum/trait name
- Enum variants are checked before inherent methods — constructors take priority within the enum namespace
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
  type symbol_kind = TopLevelLet | LocalLet | Param | TypeAliasSym | TraitSym | EnumSym | ...
  type symbol = { id: symbol_id; name: string; kind: symbol_kind;
                  definition_pos: int; definition_end_pos: int; file_id: string option }
  ```
- Create global `symbol_table: (symbol_id, symbol) Hashtbl.t`
- During inference, register symbols when bindings are created
- Map `(expr.id, symbol_id)` for identifier resolution tracking

**Files:** `lib/frontend/typecheck/infer.ml` (or new `lib/frontend/typecheck/symbol_table.ml`)

**Tests:** Unit tests that symbols are created for let bindings, function params, type aliases. Verify symbol IDs are unique within a file.

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
| ExportDecl of string list   (* export add, pi, point *)

(* Import declaration *)
| ImportDecl of {
    import_module: string list;       (* ["math"] or ["collections"; "list"] *)
    import_name: string option;       (* Some "add" for import math.add, None for import math *)
    import_alias: string option;      (* Some "pt" for import math.point as pt *)
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
- Unit: parse `export add, point, color`
- Unit: parse `import math`, `import math.add`, `import math.point as pt`
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
1. Build the module's initial `type_env` from builtins + imported module signatures
2. Run `infer_program` on the module's AST alone
3. Extract a `module_signature` from the results (exported types, values, enums, traits, impls)
4. Store the signature for use by downstream modules
5. The type checker ONLY sees what's been explicitly made available — correct isolation by construction

**New type: Module Signature** (new file `lib/frontend/typecheck/module_sig.ml`):
```ocaml
type module_signature = {
  module_id : string;
  values : (string * poly_type) list;         (* exported let bindings *)
  type_aliases : (string * type_alias_info) list;  (* exported type aliases *)
  enums : (string * enum_def) list;           (* exported enum definitions *)
  traits : (string * trait_def) list;         (* exported trait definitions *)
  impls : impl_entry list;                    (* all impls (always visible) *)
}
```

Note: trait `impl` blocks are always visible across modules (like Rust's coherence — if you import a type or trait, you get its impls). Only values/types/enums/traits need explicit export.

**Key changes:**

1. **Refactor `infer_program` to accept external environment:**
   ```ocaml
   val infer_program :
     ?env:type_env ->              (* pre-populated with imported signatures *)
     ?external_enums:enum_def list ->
     ?external_traits:trait_def list ->
     ?external_impls:impl_entry list ->
     AST.program -> infer_result
   ```
   The existing `infer_program` clears all registries and starts fresh. The refactored version accepts pre-populated registry state from upstream modules. The clear calls (infer.ml:3146-3148) move to a `reset_all` function called once per full build, then each module *adds* its definitions.

2. **Per-module registries:**
   - Each module's `infer_program` call populates registries with its own definitions (module-prefixed keys)
   - Before calling `infer_program` for a module, inject imported module's registry entries
   - After `infer_program` completes, extract the module's signature from the registry state + type_env
   - Private names are NOT included in the signature → downstream modules can't see them

3. **Import resolution + namespace environment** (new `lib/frontend/import_resolver.ml`):
   - For each module, resolve `import` declarations against upstream signatures
   - Verify imported names exist and are exported
   - Build the module's initial scope/environment:
     - `import math` → bind `"math"` as a namespace binding in scope
     - `import math.add` → bind `"add"` → `math__add` in type_env
     - `import math.point as pt` → bind `"pt"` → `math__point` in type_env
   - Expose namespace metadata to the central checker classifier so `math.add(...)` classifies as a namespace-qualified call/member access
   - Optional lowering/desugaring after classification may rewrite already-resolved direct imports to internal names
   - Do **not** use AST rewrite as the semantic source of truth for dotted qualification
   - Rewrite types: bare `point` (if directly imported) → `"math__point"` in type position

4. **Module-qualified registry keys:** Enum, trait, impl, type alias registries use prefixed keys:
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
- `lib/frontend/typecheck/enum_registry.ml` — per-module population + query
- `lib/frontend/typecheck/trait_registry.ml` — per-module population + query
- `lib/frontend/typecheck/annotation.ml` — prefixed type alias lookup
- `lib/frontend/typecheck/checker.ml` — per-module check entry point
- `lib/backend/go/emitter.ml` — remove guard, update mangling, accept multiple checked modules
- `bin/main.ml` — wire up full multi-module pipeline

**Tests:**
- Two-module programs: module A exports function, module B calls it
- Cross-module enum: define enum in A, construct/match in B
- Cross-module trait: define trait in A, impl in B (impls auto-visible)
- Cross-module type alias: define type in A, use in B
- Export enforcement: error when importing a non-exported name
- Isolation: module B cannot access module A's private names (type checker doesn't see them)
- Direct import: `import math.add` then bare `add(1, 2)` works
- Aliased import: `import math.point as pt` then `pt` works as type name
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
3. **Enum/type name collision across modules** — mitigated by module-prefixed registry keys
4. **`.` disambiguation** — resolved by the unified classifier defined in the Syntax Design section. Value bindings win first, then namespace bindings, then enums (variants before inherent methods), then traits, then type aliases. Later extern qualifiers join the namespace bucket rather than introducing a second precedence rule.
5. **Monomorphization across modules** — emitter runs whole-program across all checked modules. `collect_insts_stmt` sees all call sites from all modules.
6. **Trait impl visibility** — impls must be visible across modules even without explicit import (coherence). The per-module approach must auto-inject upstream impls into each module's registry.

---

## Verification

After each phase:
1. `make unit` — all inline tests pass
2. `make integration` — all integration suites pass
3. New module-specific tests added per phase (see test sections above)
4. Manual verification: write a 2-3 module program, compile, run, verify output

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
| `lib/frontend/typecheck/enum_registry.ml` | Module-prefixed keys |
| `lib/frontend/typecheck/trait_registry.ml` | Module-prefixed keys |
| `lib/frontend/typecheck/annotation.ml` | Prefixed type alias lookup |
| `lib/frontend/typecheck/checker.ml` | Per-module check entry point |
| `lib/backend/go/emitter.ml` | Remove guard, module-prefixed mangling, compile_modules |
| `bin/main.ml` | Wire up loader + multi-module pipeline |
| **New:** `lib/frontend/discovery.ml` | File resolution, dependency discovery |
| **New:** `lib/frontend/module_context.ml` | Module graph, topo sort, cycle detection |
| **New:** `lib/frontend/import_resolver.ml` | Resolve imports, build namespace env, optional post-classification lowering |
| **New:** `lib/frontend/typecheck/module_sig.ml` | Module signature type + extraction |
| **New:** `lib/frontend/compiler.ml` | Project-level orchestrator |
