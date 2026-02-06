# Plan: Multi-Field Enum Variants with Optimal Layout

**Status:** PLANNED  
**Created:** Feb 4, 2026  
**Phase:** 4.4

## Goal

Fix the current enum implementation to properly support:
1. Multi-field variants (currently only first field is stored)
2. Heterogeneous variant types (currently falls back to interface{})

Using the Tag + multi-field struct approach with optimal memory layout.

## Design Decision: Tag + Union of Fields

We keep the current Tag-based approach but extend it to handle all fields:

```go
type result_int_string struct {
    Tag   int8
    Data0 int64   // Used by ok(int)
    Data1 string  // Used by error(string)
}
```

**Why not interfaces?**
- Interface type switches are slower than int comparison
- Interface values often escape to heap
- More complex codegen

**Tradeoff accepted:** Struct has space for all variant fields (memory overhead), but:
- Int comparison is very fast
- No heap allocations
- Predictable layout
- Go compiler can optimize field access

## Memory Layout Optimization

Order fields by alignment size (largest first) to minimize padding:

```go
// Bad (with padding):
type bad struct {
    Tag   int8    // 1 byte + 7 padding
    Data0 bool    // 1 byte + 7 padding  
    Data1 int64   // 8 bytes
}  // Total: 24 bytes

// Good (optimized):
type good struct {
    Data1 int64   // 8 bytes
    Data0 bool    // 1 byte
    Tag   int8    // 1 byte + 6 padding
}  // Total: 16 bytes
```

**Algorithm:**
1. Collect all fields needed across all variants
2. Sort by type size: int64/float64/string/pointer (8) > int32/float32 (4) > int16 (2) > int8/bool (1)
3. Tag (int8) goes last

## Current vs Target

**Current (broken for multi-field):**
```go
type http_response struct {
    Tag  int8
    Data int64  // Only stores first field!
}

func http_response_ok(v0 int64, v1 string) http_response {
    return http_response{Tag: 0, Data: v0}  // v1 is LOST
}
```

**Target:**
```go
type http_response struct {
    Data0 int64   // ok.code, error.code
    Data1 string  // ok.msg, error.msg
    Tag   int8
}

func http_response_ok(v0 int64, v1 string) http_response {
    return http_response{Tag: 0, Data0: v0, Data1: v1}
}

func http_response_error(v0 int64, v1 string) http_response {
    return http_response{Tag: 1, Data0: v0, Data1: v1}
}
```

**Pattern matching:**
```go
switch __scrutinee.Tag {
case 0: // ok
    code := __scrutinee.Data0
    msg := __scrutinee.Data1
    return code
case 1: // error
    code := __scrutinee.Data0
    msg := __scrutinee.Data1
    return code
}
```

## Heterogeneous Types

For `enum result[a,b] { ok(a) error(b) }` instantiated as `result[int, string]`:

```go
type result_int_string struct {
    Data0 int64   // Used by ok (field 0 = int)
    Data1 string  // Used by error (field 0 = string, but different position!)
    Tag   int8
}
```

Wait - this is tricky. The `ok` variant's field 0 is `int`, but the `error` variant's field 0 is `string`. They're at the same *position* in the variant but have different *types*.

**Solution: Map (variant, position) -> struct field**

We need to track which struct field each variant's field maps to:

```
ok(int)      -> ok.field0 uses Data0 (int64)
error(string) -> error.field0 uses Data1 (string)
```

Pattern matching for `ok`:
```go
case 0: // ok
    v := __scrutinee.Data0  // int64
```

Pattern matching for `error`:
```go
case 1: // error
    e := __scrutinee.Data1  // string
```

**Algorithm:**
1. Collect all (variant_name, field_index, field_type) tuples
2. Group by field_type
3. Assign each unique type a DataN field
4. Build mapping: (variant, position) -> DataN

## Implementation Tasks

### Task 1: Analyze enum fields and build layout

```ocaml
type field_mapping = {
  data_field_name: string;      (* e.g., "Data0" *)
  data_field_type: Types.mono_type;
  go_type: string;              (* e.g., "int64" *)
}

type variant_field_map = (int * field_mapping) list  (* position -> mapping *)

type enum_layout = {
  fields: field_mapping list;                 (* All DataN fields in order *)
  variant_maps: (string * variant_field_map) list;  (* variant_name -> mappings *)
}

let analyze_enum_layout (enum_def: enum_def) (type_args: mono_type list) : enum_layout
```

### Task 2: Update `emit_enum_type`

**2.1 Generate struct with optimal field order**
```ocaml
(* Sort fields by size, Tag last *)
let sorted_fields = sort_by_size layout.fields in
let fields_str = List.map (fun f -> 
    Printf.sprintf "\t%s %s" f.data_field_name f.go_type
  ) sorted_fields 
  |> String.concat "\n" 
in
let struct_def = Printf.sprintf "type %s struct {\n%s\n\tTag int8\n}\n" 
    go_type_name fields_str
```

**2.2 Generate constructors using mapping**
```ocaml
(* For each variant, generate constructor that sets correct fields *)
let field_inits = List.mapi (fun i mapping ->
    Printf.sprintf "%s: v%d" mapping.data_field_name i
  ) variant_field_map
  |> String.concat ", "
in
Printf.sprintf "func %s(%s) %s {\n\treturn %s{Tag: %d, %s}\n}\n"
    constructor_name params go_type_name go_type_name tag_value field_inits
```

### Task 3: Update `emit_pattern_bindings`

Support multiple field patterns:
```ocaml
match field_patterns with
| [] -> []
| patterns ->
    List.mapi (fun i pat ->
      match pat.AST.pat with
      | AST.PVariable var_name ->
          let mapping = List.assoc i variant_field_map in
          (var_name, mapping.data_field_name, mapping.data_field_type)
      | AST.PWildcard -> None
      | _ -> failwith "Complex patterns not supported"
    ) patterns
    |> List.filter_map Fun.id
```

### Task 4: Update `emit_match_arm_enum`

Generate bindings using mapped field names:
```ocaml
let binding_strs = List.map (fun (var_name, data_field, _typ) ->
    Printf.sprintf "\t\t%s := __scrutinee.%s" var_name data_field
  ) bindings
in
```

### Task 5: Remove old code

- Remove `concrete_only` flag (no longer needed - always concrete)
- Remove single-Data-field logic
- Remove interface{} fallback for heterogeneous types

## Test Cases

```bash
# Multi-field variant extraction
test_case "Multi-field variant extraction" \
    'enum http_response { ok(int, string) error(int, string) }
     let x = http_response.ok(200, "OK")
     match x {
       http_response.ok(code, msg): code
       http_response.error(code, msg): code
     }' \
    "true"

# Heterogeneous single-field
test_case "Heterogeneous result type" \
    'enum result[a, b] { ok(a) error(b) }
     let x: result[int, string] = result.ok(42)
     match x {
       result.ok(v): v + 1
       result.error(e): 0
     }' \
    "true"

# Three fields
test_case "Three-field variant" \
    'enum triple { t(int, string, bool) }
     let x = triple.t(1, "two", true)
     match x {
       triple.t(a, b, c): a
     }' \
    "true"

# Mixed field counts
test_case "Mixed field count variants" \
    'enum mixed { none zero(int) pair(int, int) triple(int, int, int) }
     let x = mixed.pair(1, 2)
     match x {
       mixed.none: 0
       mixed.zero(a): a
       mixed.pair(a, b): a + b
       mixed.triple(a, b, c): a + b + c
     }' \
    "true"

# Heterogeneous multi-field
test_case "Heterogeneous multi-field" \
    'enum either[a, b] { left(a, int) right(b, string) }
     let x: either[bool, float] = either.left(true, 42)
     match x {
       either.left(flag, n): n
       either.right(val, s): 0
     }' \
    "true"
```

## Files to Modify

| File | Changes |
|------|---------|
| `lib/backend/go/emitter.ml` | Core changes to enum generation and pattern matching |
| `test/test_typecheck_and_codegen.sh` | Add new test cases |

## Success Criteria

1. All 43 existing tests pass
2. New multi-field tests pass
3. New heterogeneous type tests pass
4. Generated Go code compiles and runs correctly
5. No `interface{}` in generated code
6. No runtime type assertions
7. Optimal struct field ordering (largest fields first)
