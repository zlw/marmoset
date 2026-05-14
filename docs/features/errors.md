# Errors

## Maintenance

- Last verified: 2026-05-14
- Implementation status: Canonical (actively maintained)
- Update trigger: Any change to `Error`/`*Error` enum classification, canonical messages, `std.error`, `Result`, `try`, shim boundary conversion, or Go error codegen

## Scope

This doc covers production error values:

- semantic `Error`/`*Error` enum names,
- canonical variant messages,
- typed causes,
- hidden source context,
- `std.error` helpers,
- `Result.wrap`,
- `try`, `try ... wrap ...`, and `try ... or ...`,
- must-use `Result` checking,
- shim boundary context.

## Error Enums

Enums named exactly `Error` or ending in `Error` are error enums.
Every variant in an error enum must declare a canonical message string.

```marmoset
type DecodeError = {
  InvalidUtf8 = "Invalid UTF-8",
}

type FileError = {
  NotFound = "File not found",
  InvalidData(DecodeError) = "File contains invalid data",
  Other(Str) = "File operation failed",
}
```

The name is semantic. Import aliases for error enums must also be error names, so `import std.file.Error as FileError` is valid and a non-error alias is rejected.

Messages are attached to variants, not individual values. Payloads carry typed detail or causes.

## Typed Causes

Error payloads should preserve lower-level errors directly instead of stringifying them.

```marmoset
import std.bytes.DecodeError

type ConfigError = {
  File(std.file.Error) = "Could not read config file",
  Decode(DecodeError) = "Config file is not valid text",
}
```

Pattern matching can inspect the typed cause:

```marmoset
case ConfigError.File(std.file.Error.InvalidData(DecodeError.InvalidUtf8)):
  "bad encoding"
```

## Source Context

Error enums carry hidden context fields in generated code. Source code still constructs and matches only the declared payloads.

```marmoset
let err = DecodeError.InvalidUtf8
```

Use `std.error` for the public view:

```marmoset
import std.error

puts(error.message(err))
let ctx = error.context(err)
let frames = error.frames(err)
```

`error.message` returns the canonical variant message. `error.context` and `error.frames` expose the collected source frames.
`error.format` is reserved for a future recursive formatter; it is not part of the current `std.error` API.

## Result Helpers

`Result` values are must-use. A `Result` cannot be silently discarded as a bare expression statement, as a block statement whose value is ignored, or through `let _ = ...`.

Invalid:

```marmoset
std.io.print("Hello")
let _ = std.io.print("Hello")
```

Valid uses include returning the `Result`, binding it to a named variable, matching it, propagating it with `try`, recovering with `try ... or ...`, or composing it through `Result.bind`, `Result.map`, `Result.wrap`, or `Result.value_or`.

`Result.wrap` wraps failures and preserves successes:

```marmoset
Result.wrap(std.file.read[Str](path), (err: std.file.Error) -> ConfigError.File(err))
```

Use `Result.wrap` for explicit pipeline-style error wrapping, and `try ... wrap ...` for early-return propagation.

## Try Propagation

`try` unwraps a successful `Result` and returns early on failure from a function that itself returns `Result`.

```marmoset
fn read_text(path: std.path.Path) -> Result[Str, std.file.Error] = {
  let text = try std.file.read[Str](path)
  Result.Success(text)
}
```

Inside a function, the enclosing function must have a known `Result[_, ErrorType]` return type. At top level, `try` is allowed for scripting-style programs: a `Result.Failure` is printed to stderr and exits with status `1`. The tried expression must have type `Result[a, InnerError]`, and `InnerError` must be an `Error` or `*Error` enum.

Use `try ... wrap ...` when the enclosing error type differs:

```marmoset
type ConfigError = {
  File(std.file.Error) = "Could not read config file",
}

fn load(path: std.path.Path) -> Result[Str, ConfigError] = {
  let text = try std.file.read[Str](path) wrap ConfigError.File
  Result.Success(text)
}
```

Inside a function, the wrap variant must belong to the enclosing error enum and accept the inner error type. At top level, `try ... wrap ...` prints the adapted outer error and exits with status `1` on failure.

`try` also works with `Option` inside functions that return `Option`:

```marmoset
fn display_name(name: Option[Str]) -> Option[Str] = {
  let value = try name
  Option.Some(value)
}
```

At top level, `try Option.None` exits with status `1` and no error message.

Use `try ... or ...` to recover to a fallback value instead of propagating. It works for both `Result` failures and `Option.None`, and it is an ordinary expression, so it can be used at top level:

```marmoset
let remote_port = try read_port() or 8080
```

## Shims

Generated shim adapters attach a source frame when a Go shim returns an error enum. Shim code still returns typed API values such as `fileapi.ErrorNotFound{}`; the adapter owns conversion into Marmoset error context.

Shim signatures can use error enums, `Result`, typed causes, `Bytes`, handles, lists, options, wrappers, and supported callbacks. Unsupported boundary types are rejected during typechecking.

## Related Docs

- `docs/features/enums.md`
- `docs/features/ffi.md`
- `docs/features/pattern-matching.md`
- `docs/features/type-annotations-and-aliases.md`

## Implementation Touchpoints

- Error enum classification: `lib/frontend/typecheck/enum_registry.ml`
- Variant message parsing: `lib/frontend/syntax/parser.ml`
- Error trait synthesis: `lib/frontend/typecheck/trait_registry.ml`
- Typechecking and `try`: `lib/frontend/typecheck/infer.ml`
- Shim boundary conversion: `lib/frontend/typecheck/shim_boundary.ml`
- Go error layout/context/codegen: `lib/backend/go/emitter.ml`
