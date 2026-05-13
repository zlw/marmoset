# Production Error Model Plan

## Maintenance

- Last verified: 2026-05-13
- Implementation status: Planning
- Update trigger: Any change to named sum types, `Result`, error-returning stdlib APIs, shim ABI enum conversion, source spans, `try`/early-return syntax, editor grammars, or diagnostics
- Prerequisites:
  - `docs/plans/done/language/06_module-system.md`
  - `docs/plans/done/language/07_prelude.md`
  - `docs/plans/done/language/09_shim-first-go-interop.md`
  - `docs/plans/todo/language/05_stdlib.md`

## Context

The shim-first stdlib work exposed a real production error-model gap. Today Marmoset uses ordinary named sums such as:

```marmoset
type DecodeError = { InvalidUtf8 }

type Error = {
  NotFound,
  PermissionDenied,
  InvalidData(Str),
}
```

That is fine for toy examples, but it fails several production needs:

1. Canonical human messages are not defined at the error type. Call sites can drift into `"Organization not found"` versus `"Organization could not be found"`.
2. Wrapping lower-level errors is manual and lossy. `std.file` currently maps `DecodeError.InvalidUtf8` into `Error.InvalidData("invalid utf-8")`, losing the typed cause.
3. Deep app layers would need to hand-write repetitive wrapper enums and nested matches.
4. Go shims return domain enum values, but the boundary has no standard way to attach Marmoset-level origin frames.
5. `Result.or` exists as a generic error-mapping helper, but the intended operation is really `map_error`; propagation sugar does not exist.

Marmoset should make production-grade error handling a language and stdlib convention, not an application-level reinvention. The target is "best of Zig plus best of Go":

- typed error values remain part of `Result[T, E]` signatures;
- errors are ordinary values that can be matched by variant;
- every error has a canonical message and context;
- wrappers preserve typed original errors instead of erasing them into strings;
- propagation is explicit and typechecked;
- diagnostics can show a useful Marmoset-level cause chain.

## Goals

1. Make `Error`/`*Error` naming a compiler-enforced semantic convention.
2. Require every error variant to declare a canonical human-readable message at the definition site.
3. Attach hidden `std.error.Context` metadata to every constructed error value without making callers pass it manually.
4. Preserve typed causes by keeping lower-level errors as normal variant payloads.
5. Provide `std.error` helpers for `message`, `context`, frame inspection, and formatting.
6. Add `Result.map_error` as the explicit non-lossy error-mapping helper.
7. Add checked propagation sugar for `Result` values, including wrapping into an outer error variant.
8. Teach shim ABI adapters to attach Marmoset-level origin context when Go shims return error values.
9. Migrate `std.bytes`, `std.io`, `std.file`, and `std.dir` onto the new error model.
10. Keep LSP, tree-sitter, Zed, VS Code, Neovim, and JetBrains grammar support in lockstep.

## Non-Goals

1. Do not add exceptions.
2. Do not replace `Result[T, E]`.
3. Do not add a fully general effect system.
4. Do not require literal types before this plan can land.
5. Do not expose Go `error`, stack traces, pointers, or source locations as public Marmoset error payloads.
6. Do not force every application to use one global `AppError`; the model must support subdomain-local error files and layered wrappers.
7. Do not make dynamic `error.Any` the primary recovery API in the first implementation. Typed matching remains primary.

## Current State

- Sum types are represented in the surface AST as `Surface.STNamedSum` and lowered to `AST.EnumDef`.
- `Enum_registry.variant_def` tracks only `name` and `fields`.
- `Types.TEnum` has no metadata for "this enum is an error".
- `Type_registry` owns named products, wrappers, and extern types; error sums currently live only in `Enum_registry`.
- `std/result.mr` exports `Result` with `value_or`, `map`, `or`, `bind`, `success?`, and `failure?`.
- `std/file.mr` imports `std.bytes.DecodeError`, but `decode_error` converts it into `Error.InvalidData("invalid utf-8")`.
- Go shim API packages emit enum interfaces and per-variant structs such as `fileapi.ErrorNotFound{}` with no context field.
- Shim boundary conversion in `lib/backend/go/emitter.ml` maps Go API owner enums to internal Marmoset enum values in `emit_boundary_to_value`.
- The compiler already carries source spans (`pos`, `end_pos`, `file_id`) through parser, surface AST, core AST, and diagnostics.
- Editors already support canonical `type Foo = { Variant(...) }` and wrapper syntax, but they do not know about per-variant message annotations.

## Design Decisions

### Naming Is Semantic

An error type is a named sum whose unqualified source name is exactly `Error` or ends with `Error`.

Valid:

```marmoset
type Error = {
  Failed = "Operation failed",
}

type FileError = {
  NotFound = "File not found",
}
```

Invalid:

```marmoset
type Problem = {
  NotFound = "File not found",
}

type RenderError = { x: Int }
extern type Error
type Error = Error(Str)
```

Rules:

1. `Error` and `*Error` names are reserved for error sums and aliases to error types.
2. A non-sum named product, wrapper, or extern type must not be named `Error` or `*Error`.
3. A type alias whose name is `Error` or `*Error` must resolve to an error type.
4. A type alias to an error type must itself be named `Error` or `*Error`.
5. The rule applies after module resolution using the user-facing declaration name, not only the internal `std__file__Error` form.
6. Legacy `enum FooError = { ... }` follows the same rules while the legacy `enum` syntax still exists.

This deliberately makes naming part of the language. It supports the Marmoset goal of encoding large-codebase conventions directly into the compiler.

### Canonical Messages

Every variant of an error type must declare a static string literal message:

```marmoset
type Error = {
  NotFound(path.Path) = "File not found",
  PermissionDenied(path.Path) = "You do not have permission to access this file",
  InvalidData(bytes.DecodeError) = "File contains invalid data",
}
```

Rules:

1. The message is declaration metadata, not a normal caller-supplied constructor argument.
2. The message must be a string literal in the first implementation. Interpolation and computed messages are out of scope.
3. `error.message(err)` always returns `Str`, never `Option[Str]`.
4. Payloads carry structured data for matching, logging, and tests. The message is stable human-facing copy.
5. Future formatting helpers may combine message plus selected payload details, but the canonical message itself stays centralized.

### Hidden Context

Every error value carries `std.error.Context` as hidden metadata.

Source:

```marmoset
type Error = {
  InvalidData(bytes.DecodeError) = "File contains invalid data",
}
```

Conceptual internal shape:

```marmoset
Error.InvalidData(bytes.DecodeError, error.Context)
```

User-visible constructor and pattern arity stays based on declared payloads:

```marmoset
let err = Error.InvalidData(DecodeError.InvalidUtf8)

match err {
  case Error.InvalidData(DecodeError.InvalidUtf8): "bad utf-8"
}
```

Context is accessed through `std.error`, not by matching an extra hidden field:

```marmoset
error.message(err)
error.frames(err)
```

Initial stdlib shape:

```marmoset
# std/error.mr

export Context, Frame, Error, message, context, frames, format

type Frame = Frame(Str, Str, Str, Int, Int)
# module, function, file, line, column

type Context = Context(Str, List[Frame])
# canonical message plus Marmoset-level frames

trait Error[e] = {
  fn message(err: e) -> Str
  fn context(err: e) -> Context
  fn frames(err: e) -> List[Frame]
}
```

`Context` and `Frame` are nominal stdlib values with helper accessors. They are not structural records because the representation may grow later.

### Typed Causes

Wrapping is represented by ordinary declared payloads. If a variant payload is an error type, it is a typed cause:

```marmoset
type ConfigError = {
  File(file.Error) = "Could not load config file",
  Decode(bytes.DecodeError) = "Config file is not valid text",
  InvalidSyntax(parser.Error) = "Config file has invalid syntax",
}
```

Typed recovery stays precise:

```marmoset
match err {
  case ConfigError.File(file.Error.NotFound): "missing config"
  case ConfigError.Decode(bytes.DecodeError.InvalidUtf8): "bad encoding"
}
```

Formatting can recurse through known error payloads:

```text
Could not load config file
  at app.config.load app/config.mr:12:14
caused by: File contains invalid data
  at std.file.read std/file.mr:54:3
caused by: Invalid UTF-8
  at std.bytes.to_str std/bytes.mr:9:3
```

The first implementation may format only statically known concrete error payloads. Generic error wrappers such as `UseError[e]` can still keep the payload; recursive generic formatting can wait until `error.Error[e]` constraints on type parameters are available.

### Construction Sites And Frames

Constructing an error variant creates context automatically:

```marmoset
Error.NotFound(path)
```

The compiler inserts:

- canonical message from the variant declaration;
- a Marmoset source frame for the construction site;
- module id;
- enclosing function name when available, otherwise `<top-level>`;
- file path or module file id;
- line and column computed from the parser source map.

For errors returned by Go shims, the first Marmoset frame should be the extern function boundary or public wrapper that introduced the error. The implementation must choose one consistent initial rule:

1. Shim adapter frame: the `extern` function declaration, e.g. `std.file.read_path`.
2. Wrapper frame: the public Marmoset function that called the shim, e.g. `std.file.read`.

This plan chooses **shim adapter frame first**, because the generated adapter is the precise boundary where foreign data becomes a Marmoset error. Public wrappers and `try wrap` can add additional frames later.

### `Result.map_error`

`std.result` gains the clearer helper:

```marmoset
fn map_error[b](self: Result[a, e], f: (e) -> b) -> Result[a, b]
```

`Result.or` remains as a compatibility alias during migration, but new stdlib code should use `map_error`.

### Propagation Sugar

Add a `try` expression for `Result` propagation.

Same error type:

```marmoset
fn read_config(path: path.Path) -> Result[Str, file.Error] = {
  let text = try file.read[Str](path)
  Result.Success(text)
}
```

Lowering:

```marmoset
match file.read[Str](path) {
  case Result.Success(value): value
  case Result.Failure(err): return Result.Failure(error.add_frame(err))
}
```

Wrapping into an outer error:

```marmoset
type ConfigError = {
  File(file.Error) = "Could not read config file",
  Parse(parser.Error) = "Could not parse config file",
}

fn load(path: path.Path) -> Result[Config, ConfigError] = {
  let text = try file.read[Str](path) wrap ConfigError.File
  try parse(text) wrap ConfigError.Parse
}
```

Checks:

1. The tried expression must have type `Result[a, InnerError]`.
2. The enclosing function must return `Result[b, OuterError]`.
3. Without `wrap`, `InnerError` must be the same error type as `OuterError`.
4. With `wrap OuterError.Variant`, the variant must belong to `OuterError` and accept `InnerError` as one of its declared payloads.
5. The constructed outer error gets its own canonical message and frame while preserving the original error payload.

The exact parser form is:

```marmoset
try <expr>
try <expr> wrap <ErrorType>.<Variant>
```

`try` is valid only inside function bodies in the first implementation. It does not make a pure function effectful; it is typed control flow over `Result`.

## Implementation Phases

### Phase E0: Metadata And Parser Support

Goal: Parse and preserve error variant messages without changing runtime behavior yet.

Files:

- `lib/frontend/syntax/token.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/lower.ml`
- `lib/frontend/syntax/ast.ml`
- `tools/tree-sitter-marmoset/grammar.js`
- `tools/tree-sitter-marmoset/queries/highlights.scm`
- `tools/vscode-marmoset/syntaxes/marmoset.tmLanguage.json`
- `tools/scripts/sync_textmate_grammars.sh`
- Zed, Neovim, and JetBrains editor grammar fixtures/CI as applicable

Changes:

1. Extend `surface_variant_def` and `AST.variant_def` with `variant_message : string option`.
2. Parse optional `= "..."` after each sum variant payload.
3. Keep non-error sum variants message-less and unchanged.
4. Add parser diagnostics for malformed message syntax:
   - `type Error = { NotFound = 1 }`
   - `type Error = { NotFound = "a" + "b" }`
   - `type Error = { NotFound("x") }` when a string is parsed as a type rather than a message.
5. Update surface/core AST show helpers and lowering tests.
6. Update editor grammars so error messages highlight as strings inside type bodies.

Tests:

- Parser unit tests for:
  - `type Error = { NotFound = "File not found" }`
  - `type Error = { InvalidData(bytes.DecodeError) = "File contains invalid data" }`
  - ordinary `type Option[a] = { Some(a), None }` unchanged
  - malformed message diagnostics
- Tree-sitter corpus for message variants.
- VS Code/TextMate grammar check.

Gate:

```bash
dune runtest lib/frontend/syntax
test/ci/tree-sitter.sh
test/ci/editor-vscode.sh
test/ci/editor-nvim.sh
test/ci/editor-jetbrains.sh
cargo fmt --check && cargo test --locked --manifest-path tools/zed-marmoset/Cargo.toml
git diff --check
```

### Phase E1: Error-Type Classification And Diagnostics

Goal: Make `Error`/`*Error` names compiler-enforced error types.

Files:

- `lib/frontend/typecheck/enum_registry.ml`
- `lib/frontend/typecheck/type_registry.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/import_resolver.ml`
- `lib/frontend/compiler.ml`
- `tools/lsp/lib/*.ml` where declarations and diagnostics inspect type definitions

Changes:

1. Add error metadata to enum registry:

   ```ocaml
   type variant_def = {
     name : string;
     fields : mono_type list;
     message : string option;
   }

   type enum_kind =
     | OrdinaryEnum
     | ErrorEnum

   type enum_def = {
     name : string;
     source_name : string option;
     type_params : string list;
     variants : variant_def list;
     kind : enum_kind;
   }
   ```

2. Add helper predicates:

   ```ocaml
   val is_error_type_name : string -> bool
   val is_error_enum : string -> bool
   val require_error_name_for_error_alias : ...
   ```

3. Classify canonical named sums whose source declaration name is `Error` or ends with `Error` as `ErrorEnum`.
4. Reject `Error`/`*Error` products, wrappers, and extern types.
5. Reject error aliases under non-error names and non-error aliases under error names.
6. Require every variant of an error enum to have a message.
7. Reject messages on non-error enums unless this is intentionally allowed for future display metadata. The first implementation should reject them to keep semantics crisp.
8. Preserve module-qualified names and imports: `std.file.Error` remains a valid error type because the unqualified source name is `Error`.

Diagnostics:

```text
error-name-reserved: Type 'RenderError' ends with Error, so it must be an error sum.
error-message-required: Error variant 'FileError.NotFound' must declare a canonical message.
error-alias-name: Type 'Problem' aliases error type 'PgError', but error aliases must be named Error or *Error.
error-message-not-allowed: Non-error variant 'Option.None' cannot declare an error message.
```

Tests:

- Inline typechecker tests for all diagnostics.
- Module/import tests:
  - `std.file.Error` imported as `Error` remains an error type.
  - aliasing an imported error to `Problem` fails.
  - aliasing an imported error to `FileError` succeeds.
- Existing ordinary enum fixtures remain green.

Gate:

```bash
dune runtest lib/frontend/syntax lib/frontend/typecheck
make integration enums modules
git diff --check
```

### Phase E2: `std.error` And Hidden Context Runtime

Goal: Give every error value context and canonical message at runtime.

Files:

- `std/error.mr` (new)
- `std/result.mr`
- `std/prelude.mr` if `std.error` must be core-loaded
- `lib/frontend/compiler.ml`
- `lib/backend/go/emitter.ml`
- `runtime/go/marmoset/**`
- `test/integration/*` or new focused error suite

Changes:

1. Add `std.error` with nominal `Frame`, nominal `Context`, and trait `Error[e]`.
2. Add generated/synthetic trait satisfaction for `error.Error[e]` when `e` is an error enum.
3. Add `Result.map_error` to `std.result.mr`; keep `or` as compatibility alias.
4. Extend Go enum layout for error enums with a hidden context slot.
5. Make source constructors for error variants inject context automatically:
   - canonical message;
   - one frame for construction site;
   - no caller-visible context argument.
6. Make pattern matching ignore the hidden context for user-written constructor patterns.
7. Generate `error.message`, `error.context`, and `error.frames` support for error enum values.
8. Ensure derived `Show` or interpolation on error values uses the canonical message or a stable debug format. Pick one and document it before landing.

Tests:

- Constructor arity:

  ```marmoset
  type Error = { NotFound(path.Path) = "File not found" }
  let err = Error.NotFound(path.Path("missing"))
  ```

  must compile without a context argument.

- Pattern arity:

  ```marmoset
  match err { case Error.NotFound(_): "ok" }
  ```

  must compile without matching context.

- Message:

  ```marmoset
  error.message(Error.NotFound(path.Path("x"))) == "File not found"
  ```

- Frames contain at least one Marmoset frame with the declaring file/function.
- `Result.map_error` preserves success and maps failure.

Gate:

```bash
dune runtest lib/frontend lib/backend/go
make integration prelude stdlib-shims
git diff --check
```

### Phase E3: Shim Boundary Context

Goal: Make Go shim failures enter Marmoset with canonical messages and origin frames.

Files:

- `lib/frontend/typecheck/shim_boundary.ml`
- `lib/backend/go/emitter.ml`
- `runtime/go/shims/std/bytes/bytes.go`
- `runtime/go/shims/std/file/file.go`
- `runtime/go/shims/std/dir/dir.go`
- `runtime/go/shims/std/io/**`
- generated API snapshot fixtures
- `test/integration/13_ffi.sh`
- `test/integration/14_stdlib_shims.sh`

Changes:

1. Keep Go shim API packages ergonomic: shim authors still return `fileapi.ErrorNotFound{}` or equivalent without manually constructing `error.Context`.
2. In generated boundary conversion from Go API enum to Marmoset enum, detect `ErrorEnum` and inject context.
3. Use the extern function declaration as the initial frame:

   ```text
   std.file.read_path at std/file.mr:<extern fn span>
   ```

4. Preserve normal declared payloads returned by Go shims.
5. Reject unsupported error enum boundary shapes with clear diagnostics instead of silently stripping context.
6. Update FFI docs to explain that shims return typed domain errors while Marmoset adapters attach context.

Tests:

- A Go shim returning `DecodeError.InvalidUtf8` exposes `error.message(err) == "Invalid UTF-8"`.
- A Go shim returning `file.Error.NotFound` exposes `"File not found"` and a `std.file.read_path` frame.
- `std.file.read[Str]` on invalid UTF-8 preserves the typed `bytes.DecodeError.InvalidUtf8` cause after the stdlib migration phase.
- Generated shim API packages still compile with Go 1.18.

Gate:

```bash
dune runtest lib/backend/go
make integration ffi stdlib-shims
git diff --check
```

### Phase E4: Propagation Sugar

Goal: Add explicit, typed `try` propagation and `try ... wrap ...`.

Files:

- `lib/frontend/syntax/token.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/lower.ml`
- `lib/frontend/syntax/ast.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/backend/go/emitter.ml`
- LSP semantic tokens, hover, signature help, and selection ranges if they inspect expression forms
- editor grammars

Changes:

1. Add `try` and `wrap` tokens.
2. Add AST representation for:

   ```ocaml
   Try of {
     tried : expression;
     wrap : (string * string) option; (* ErrorType, Variant *)
   }
   ```

3. Typecheck `try` only inside a function body whose return type is `Result[_, OuterError]`.
4. Infer the success value type from `Result[a, InnerError]`.
5. Without `wrap`, require `InnerError` and `OuterError` to be the same error type.
6. With `wrap`, validate that the target variant belongs to `OuterError` and can accept `InnerError` as a declared payload.
7. Emit early return code that preserves or wraps the error and adds a frame.
8. Reject:
   - `try` outside a function body;
   - `try` in a function that does not return `Result`;
   - `try` on non-`Result` values;
   - wrapping into a non-error type;
   - wrapping with a variant that does not accept the inner error.

Tests:

- Same-error propagation.
- Wrapped propagation into an outer error.
- Nested wrapping across file -> config -> controller examples.
- Error diagnostics for every rejection above.
- Generated Go code contains early return paths and does not evaluate subsequent statements on failure.

Gate:

```bash
dune runtest lib/frontend/syntax lib/frontend/typecheck lib/backend/go
make integration result codegen_mono stdlib-shims
test/ci/tree-sitter.sh
test/ci/editor-vscode.sh
test/ci/editor-nvim.sh
test/ci/editor-jetbrains.sh
cargo fmt --check && cargo test --locked --manifest-path tools/zed-marmoset/Cargo.toml
git diff --check
```

### Phase E5: Stdlib Migration

Goal: Move current stdlib errors to the production model and remove lossy conversions.

Files:

- `std/bytes.mr`
- `std/io.mr`
- `std/io/err.mr`
- `std/file.mr`
- `std/dir.mr`
- `std/result.mr`
- `docs/features/ffi.md`
- `docs/plans/todo/language/05_stdlib.md`
- `runtime/go/shims/std/**`
- `test/integration/14_stdlib_shims.sh`

Target examples:

```marmoset
type DecodeError = {
  InvalidUtf8 = "Invalid UTF-8",
}

type Error = {
  NotFound(path.Path) = "File not found",
  PermissionDenied(path.Path) = "You do not have permission to access this file",
  AlreadyExists(path.Path) = "File already exists",
  IsDirectory(path.Path) = "Expected a file but found a directory",
  NotDirectory(path.Path) = "Expected a directory but found a file",
  InvalidPath(Str) = "Path is invalid",
  InvalidData(bytes.DecodeError) = "File contains invalid data",
  AlreadyClosed = "File is already closed",
  Other(Str) = "File operation failed",
}
```

Rules:

1. `std.file.Error.InvalidData` must carry `bytes.DecodeError`, not `Str`.
2. Filesystem shims should include path payloads where useful and cheap, but should not expose Go errors directly.
3. `std.dir` should use operation-specific errors if the stdlib plan has already split them, and each must have messages.
4. Generic wrapper errors such as `UseError[e]` must declare messages and keep payloads typed.
5. New stdlib code should use `Result.map_error`, `try`, or `try ... wrap ...` instead of nested matches where possible.
6. Existing public APIs may keep module-local `Error` names; `Error` itself is a valid error type name.

Tests:

- `std.bytes.DecodeError` message and formatting.
- `std.file.read[Str]` invalid UTF-8 matches:

  ```marmoset
  case Error.InvalidData(DecodeError.InvalidUtf8): ...
  ```

- `file.open` wrapper failures preserve callback error and close error payloads.
- `dir.ls`, `dir.mkdir`, `dir.mkdir_tree`, `dir.rmdir`, `dir.rmdir_tree`, `dir.exists?`, and `dir.pwd` return errors with messages.
- All existing stdlib-shim success paths remain green.

Gate:

```bash
MARMOSET_ROOT=$PWD dune exec -- bin/main.exe check std/file.mr
MARMOSET_ROOT=$PWD dune exec -- bin/main.exe check std/dir.mr
make integration stdlib-shims
make integration ffi
dune runtest lib/frontend lib/backend/go tools/lsp/lib
git diff --check
```

### Phase E6: Documentation, Editors, And Bugbash

Goal: Make the new model understandable and harden syntax/tooling.

Files:

- `docs/features/errors.md` (new)
- `docs/features/type-annotations-and-aliases.md`
- `docs/features/ffi.md`
- `docs/plans/todo/language/05_stdlib.md`
- editor grammar docs/fixtures
- LSP tests

Changes:

1. Document:
   - `Error`/`*Error` semantic naming;
   - canonical messages;
   - hidden context;
   - typed causes;
   - `error.message`, `error.context`, `error.frames`, `error.format`;
   - `Result.map_error`;
   - `try` and `try ... wrap ...`;
   - shim boundary context.
2. Update examples in stdlib and FFI docs away from bare no-message errors.
3. Add LSP hover/definition behavior for error variants and messages.
4. Run three bugbash rounds:
   - Round 1: parser/typechecker diagnostics and alias edge cases.
   - Round 2: codegen, shim boundary, and generic `Result`/error-wrapper cases.
   - Round 3: editor and LSP syntax coverage.

Gate:

```bash
dune runtest lib/frontend lib/backend/go tools/lsp/lib
make integration ffi stdlib-shims
test/ci/tree-sitter.sh
test/ci/editor-vscode.sh
test/ci/editor-nvim.sh
test/ci/editor-jetbrains.sh
cargo fmt --check && cargo test --locked --manifest-path tools/zed-marmoset/Cargo.toml
git diff --check
```

## Testing Strategy

Add focused tests first for every phase.

Unit-level coverage:

- parser accepts/rejects variant message syntax;
- lower preserves messages and source spans;
- enum registry classifies error enums;
- alias diagnostics enforce naming;
- typechecker enforces message presence;
- typechecker rejects invalid `try`;
- codegen injects hidden context and preserves visible constructor arity.

Integration coverage:

- `test/integration/14_stdlib_shims.sh` for stdlib/shim errors;
- `test/integration/13_ffi.sh` for project-local shim enum errors;
- focused codegen fixture for `try ... wrap ...`;
- module fixture for subdomain-style `errors.mr` imported by multiple modules;
- editor CI for the new syntax.

Representative fixture:

```marmoset
import std.error
import std.file
import std.path
import std.bytes.DecodeError

type ConfigError = {
  File(file.Error) = "Could not read config file",
}

fn load(path: path.Path) -> Result[Str, ConfigError] = {
  try file.read[Str](path) wrap ConfigError.File
}

match load(path.Path("bad.txt")) {
  case Result.Success(_): puts("ok")
  case Result.Failure(ConfigError.File(file.Error.InvalidData(DecodeError.InvalidUtf8))):
    puts("invalid utf-8")
  case Result.Failure(err):
    puts(error.message(err))
}
```

## Commit Plan

1. Parse and highlight error variant messages.
2. Add error enum classification and naming/message diagnostics.
3. Add `std.error`, `Result.map_error`, and hidden context runtime support.
4. Attach context at shim boundaries.
5. Add `try` and `try ... wrap ...`.
6. Migrate stdlib errors and remove lossy decode-error mapping.
7. Update docs, editor coverage, and bugbash fixes.

Each commit must include focused tests and update this plan's `## Progress` section before committing.

## Risks

1. **Hidden fields can leak through old enum code paths.** Constructor arity, pattern arity, `Show`, shim conversion, and enum layout must all use declared payload arity for source semantics and physical arity for codegen.
2. **Generic error wrappers are subtle.** `UseError[e]` can preserve payloads before recursive formatting for generic `e` is perfect. Do not block the core model on fully generic dynamic cause formatting.
3. **Naming-as-semantics is sharp.** Diagnostics must be excellent because renames across the `Error` suffix boundary are semantic changes.
4. **Source frames need stable file identity.** Use existing parser positions and compiler source maps; avoid ad hoc path reconstruction in codegen.
5. **Shim adapters may double-context errors.** Boundary conversion must detect whether an error value already has context when callbacks return error values through a shim.
6. **`try` can complicate expression lowering.** Start with function bodies and explicit `Result` return types if inference becomes ambiguous.
7. **Docs and editors can lag.** Treat editor CI as part of the plan, not a cleanup afterthought.

## Open Questions

1. Should `error.format` recurse into all statically known error payloads or only the first one?
2. Should `try` without `wrap` add a propagation frame, or should frames be added only when constructing/wrapping?
3. Should messages on non-error enums remain rejected forever, or become generic display metadata later?
4. Should `Show` for errors return only the canonical message or a debug representation with variant and payloads?
5. Should Go shims get generated constructor helper functions for error variants once context exists, or should struct literals remain the public Go shim API?

The implementation can start without resolving all five. Questions 1, 2, and 4 must be locked before Phase E2 lands; question 5 must be locked before Phase E3 lands.

## Progress

- 2026-05-13 17:26 CEST: Drafted the production error model plan after the stdlib file/dir work exposed lossy decode-error wrapping and missing canonical domain messages. The draft chooses `Error`/`*Error` naming as semantic syntax, required per-variant messages, hidden context, typed payload causes, `Result.map_error`, `try ... wrap ...`, shim-boundary context injection, and a stdlib migration path.
- 2026-05-13 17:48 CEST: `$feature-implement` started. Reconstructed the implementation into seven green slices matching the commit plan: E0 parser/AST/editor metadata, E1 error enum classification and diagnostics, E2 `std.error`/hidden context/`Result.map_error`, E3 shim-boundary context, E4 `try`/`wrap` propagation, E5 stdlib migration, and E6 docs/editor bugbash. Starting with E0 tests before parser and AST changes.
- 2026-05-13 17:51 CEST: E0 RED observed with `dune runtest lib/frontend/syntax`: new parser tests fail to compile because `Surface.surface_variant_def` has no `sv_message` field yet.
- 2026-05-13 17:55 CEST: E0 compiler parser/AST work is green under `dune runtest lib/frontend/syntax`: variant messages now parse into surface/core AST metadata, lower into `AST.variant_def`, and malformed message syntax reports `parse-invalid-variant-message`.
- 2026-05-13 17:58 CEST: E0 editor RED observed: `npm test -- --include 'Message-bearing error sum type'` in `tools/tree-sitter-marmoset` reports error nodes around `= "..."`, and `npm run check-grammar` in `tools/vscode-marmoset` reports missing enum-variant string-message highlighting.
- 2026-05-13 18:02 CEST: E0 focused editor checks are green: the message-bearing tree-sitter corpus passes, TextMate enum variants include canonical message string highlighting, and the JetBrains TextMate grammar copy is in sync.
- 2026-05-13 18:08 CEST: E0 gate is green: `dune runtest lib/frontend/syntax lib/frontend/typecheck lib/backend/go`, `test/ci/tree-sitter.sh`, `test/ci/editor-vscode.sh`, `test/ci/editor-nvim.sh`, `test/ci/editor-jetbrains.sh`, `cargo fmt --check --manifest-path tools/zed-marmoset/Cargo.toml`, `cargo test --locked --manifest-path tools/zed-marmoset/Cargo.toml`, and `git diff --check` all passed.
- 2026-05-13 18:10 CEST: E0 commit created for parser/AST/editor support for optional variant messages.
- 2026-05-13 18:18 CEST: E1 RED observed with `dune runtest lib/frontend/typecheck`: new classification tests fail to compile because `Enum_registry.enum_def` has no `kind` metadata yet.
- 2026-05-13 18:20 CEST: E1 gate is green: `dune runtest lib/frontend`, `dune runtest lib/frontend/syntax lib/frontend/typecheck`, `make integration enums modules`, `dune runtest lib/backend/go tools/lsp/lib`, and `git diff --check` all passed after adding error enum classification, alias/name diagnostics, imported error alias coverage, and stdlib/fixture messages.
- 2026-05-13 18:21 CEST: E1 commit created for error enum classification, reserved-name diagnostics, error alias validation, and migrated stdlib/fixture canonical messages.
- 2026-05-13 18:22 CEST: E2 started. Adding RED coverage for `Result.map_error`, `std.error.message`/`frames`, source constructor and pattern arity with hidden context, and generated Go hidden context layout.
- 2026-05-13 18:23 CEST: E2 RED observed: `dune runtest lib/backend/go` fails the hidden context codegen assertion, and `make integration prelude` fails because `Result.map_error` and `std.error` do not exist yet.
- 2026-05-13 18:25 CEST: E2 gate is green: `dune runtest lib/backend/go`, `dune runtest lib/frontend/typecheck`, `make integration prelude`, `dune runtest lib/frontend lib/backend/go`, `make integration prelude stdlib-shims`, and `git diff --check` all passed after adding `std.error`, `Result.map_error`, hidden Go error context, source constructor context injection, and synthetic `std.error.Error` impl emission.
- 2026-05-13 18:26 CEST: E2 commit created for `std.error`, hidden error context runtime/codegen, synthetic error trait helpers, and `Result.map_error`.
