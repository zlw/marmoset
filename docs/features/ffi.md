# FFI

## Maintenance

- Last verified: 2026-04-28
- Implementation status: Canonical v1
- Update trigger: Any change to extern syntax, supported FFI types, typechecking, module visibility, or Go codegen

Marmoset can call trusted Go functions through `extern` declarations. An extern declaration describes a Go package path and a set of functions that Marmoset code may call directly in the declaring module.

## Syntax

```marmoset
extern "strings" = {
  fn ToUpper(s: Str) -> Str
  fn Contains(s: Str, substr: Str) -> Bool
}
```

Use `as` when the package path is not a valid Marmoset qualifier or when a different source qualifier is clearer:

```marmoset
extern "path/filepath" as fp = {
  fn Base(path: Str) -> Str
}
```

Pure extern functions use `->`. Effectful extern functions use `=>`:

```marmoset
extern "fmt" = {
  fn Println(s: Str) => Unit
}
```

## Supported Types

FFI v1 supports only scalar boundary types:

| Marmoset | Go |
|---|---|
| `Int` | `int64` |
| `Float` | `float64` |
| `Bool` | `bool` |
| `Str` | `string` |
| `Unit` return | `struct{}` |

`Unit` parameters are rejected. Lists, maps, records, enums, unions, `Dyn`, `Option`, `Result`, functions, and generic extern declarations are not supported at the FFI boundary in v1.

## Module Visibility

Extern declarations are module-local. Importing a module that declares an extern does not import its Go package qualifier.

The intended pattern is a small wrapper module:

```marmoset
export upcase

extern "strings" = {
  fn ToUpper(s: Str) -> Str
}

fn upcase(s: Str) -> Str = strings.ToUpper(s)
```

Other modules import and call `upcase` like any ordinary Marmoset API. They cannot call `strings.ToUpper` unless they declare their own extern block.

## Trust Boundary

Extern signatures are trusted Marmoset declarations. The typechecker verifies Marmoset-side calls against the declared signature, and the Go compiler remains the final authority that the declaration matches the real Go function.

Semantic adapters are explicit. Go patterns such as `(T, error)`, `(T, bool)`, nil returns, or panics are not automatically converted to Marmoset `Result`, `Option`, or effects. Wrap those semantics in ordinary Marmoset or Go code and expose the intended shape deliberately.

## Codegen

The Go backend emits wrappers only for extern functions that are actually called. Imports use deterministic generated aliases such as `mext_strings` or `mext_path_filepath`, and wrapper names are based on the full Go import path plus function name so packages with the same basename do not collide.
