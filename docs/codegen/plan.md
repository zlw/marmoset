# Go Codegen Implementation Plan

## Phase 1: Minimal Working Codegen

Goal: Compile simple expressions to working Go code.

### Type Mappings

| Marmoset | Go |
|----------|-----|
| Int | int64 |
| Float | float64 |
| Bool | bool |
| String | string |
| [T] | []T |
| {K: V} | map[K]V |
| (A, B) -> C | func(A, B) C |
| Null | (special) |

### Phase 1a: Literals and Operators

```
1 + 2           →  (1 + 2)
3.14 * 2.0      →  (3.14 * 2.0)
"hello"         →  "hello"
true            →  true
1 < 2           →  (1 < 2)
!true           →  !true
-5              →  -5
```

Generated Go:
```go
package main

func main() {
    _ = (1 + 2)
}
```

### Phase 1b: Let Bindings

```
let x = 5;
let y = x + 1;
y
```

Generated Go:
```go
package main

func main() {
    x := int64(5)
    y := (x + 1)
    _ = y
}
```

### Phase 1c: If Expressions

```
if (x > 0) { x } else { -x }
```

Generated Go (using inline func for expression):
```go
func() int64 {
    if x > 0 {
        return x
    } else {
        return -x
    }
}()
```

Or simpler with statements:
```go
var result int64
if x > 0 {
    result = x
} else {
    result = -x
}
```

### Phase 1d: Functions

```
let add = fn(a, b) { a + b };
add(1, 2)
```

Generated Go:
```go
package main

func main() {
    add := func(a int64, b int64) int64 {
        return (a + b)
    }
    _ = add(1, 2)
}
```

For top-level functions:
```go
func add(a int64, b int64) int64 {
    return (a + b)
}

func main() {
    _ = add(1, 2)
}
```

## Phase 2: Collections

### Arrays

```
let arr = [1, 2, 3];
arr[0]
```

Generated Go:
```go
arr := []int64{1, 2, 3}
_ = arr[0]
```

### Hashes

```
let h = {"a": 1, "b": 2};
h["a"]
```

Generated Go:
```go
h := map[string]int64{"a": 1, "b": 2}
_ = h["a"]
```

## Phase 3: Builtins

Create a runtime package with builtin implementations:

```go
// runtime.go
package main

import "fmt"

func mr_puts(v interface{}) interface{} {
    fmt.Println(v)
    return nil
}

func mr_len_string(s string) int64 {
    return int64(len(s))
}

func mr_len_array[T any](arr []T) int64 {
    return int64(len(arr))
}

// etc.
```

## Implementation Strategy

### Approach: Infer-as-we-go

For simplicity, we won't create a separate typed AST. Instead:

1. Type-check the whole program first (to catch errors)
2. During codegen, re-infer types for subexpressions as needed
3. Use the type to determine Go representation

```ocaml
(* Simplified codegen *)
let rec emit_expr env expr =
  let typ = infer_type env expr in
  match expr.expr with
  | Integer i -> Printf.sprintf "%Ld" i
  | Float f -> Printf.sprintf "%f" f
  | Infix (l, "+", r) ->
      Printf.sprintf "(%s + %s)" (emit_expr env l) (emit_expr env r)
  | ...
```

### Output Structure

For a simple program:
```
/tmp/marmoset-XXXXX/
├── go.mod          # module marmoset_out
├── main.go         # generated code
└── runtime.go      # builtin functions (copied from lib)
```

### Entry Point

```go
// main.go
package main

// ... generated code ...

func main() {
    // ... program entry ...
}
```

## File Structure

```
lib/backend/go/
├── emitter.ml      # Main codegen entry point
├── emit_expr.ml    # Expression emission (maybe inline)
├── emit_stmt.ml    # Statement emission (maybe inline)
├── emit_type.ml    # Type to Go type string (maybe inline)
└── runtime.go      # Copied to output dir
```

For Phase 1, keep it simple - everything in `emitter.ml`.

## Testing Strategy

1. Unit tests: emit specific expressions, check output string
2. Integration tests: full program → Go → compile → run → check output

```ocaml
let%test "emit integer" =
  emit_program "42" = "package main\n\nfunc main() {\n\t_ = 42\n}\n"

let%test "emit addition" =
  emit_program "1 + 2" = "package main\n\nfunc main() {\n\t_ = (1 + 2)\n}\n"
```

## Next Steps

1. [ ] Implement `emit_type` - convert Marmoset type to Go type string
2. [ ] Implement `emit_expr` - emit expressions
3. [ ] Implement `emit_stmt` - emit statements  
4. [ ] Implement `emit_program` - wrap in package/main
5. [ ] Add runtime.go with puts
6. [ ] Add CLI command `marmoset build`
7. [ ] Test with simple programs
