# 🎯 **COMPREHENSIVE IMPLEMENTATION PLAN: Rust-Style Enum Monomorphization**

## **Executive Summary**

**Goal:** Transform Marmoset's enum compilation from TypeScript-style (interface{}-based) to Rust-style (concrete type monomorphization) for **zero-cost abstractions** and **2-5x performance improvement**.

**Current State:**
- ✅ Enum definitions, pattern matching, codegen all working (40/40 tests passing)
- ❌ Enum type annotations **BLOCKED** at `annotation.ml:41`
- ❌ Always generates `Data interface{}` (slow, unsafe)
- ❌ Cannot write `fn(x: option[int])` - must rely on inference

**Target State:**
- ✅ Support `fn(x: option[int]) -> int` with full type annotations
- ✅ Generate concrete types: `type option_int64 struct { Tag int8; Data int64 }`
- ✅ Zero runtime overhead (no type assertions)
- ✅ Backwards-compatible: HM inference still works for scripts
- ✅ Incremental migration path

**Performance Target:** 2-5x speedup on enum-heavy code, zero heap allocations for small enums.

---

## **Phase 1: Enable Enum Type Annotations** (TDD: Write Tests First)

### **Step 1.1: Write Failing Tests for Enum Annotations**

**File:** `lib/frontend/typecheck/annotation.ml` (add at end of file, after line 100)

**What to test:**
1. Parse `option[int]` as type annotation
2. Parse `result[string, int]` (multi-param)
3. Parse nested `option[list[int]]`
4. Error on unknown enum `foo[int]`
5. Error on wrong arity `option[int, string]` (expects 1 param)

**Expected behavior:** Tests FAIL initially (blocked at line 41)

```ocaml
(* Add after line 100 *)

let%test "enum annotation option[int]" =
  let te = Syntax.Ast.AST.TApp ("option", [Syntax.Ast.AST.TCon "int"]) in
  let result = type_expr_to_mono_type te in
  result = Types.TEnum ("option", [Types.TInt])

let%test "enum annotation result[string, int]" =
  let te = Syntax.Ast.AST.TApp ("result", 
    [Syntax.Ast.AST.TCon "string"; Syntax.Ast.AST.TCon "int"]) in
  let result = type_expr_to_mono_type te in
  result = Types.TEnum ("result", [Types.TString; Types.TInt])

let%test "enum annotation nested option[list[int]]" =
  let te = Syntax.Ast.AST.TApp ("option", 
    [Syntax.Ast.AST.TApp ("list", [Syntax.Ast.AST.TCon "int"])]) in
  let result = type_expr_to_mono_type te in
  result = Types.TEnum ("option", [Types.TArray Types.TInt])
```

**Status after this step:** Tests compile but FAIL (as expected).

---

### **Step 1.2: Implement Enum Type Annotation Support**

**File:** `lib/frontend/typecheck/annotation.ml` (modify lines 28-42)

**Before (line 41):**
```ocaml
| "option" | "result" | "set" ->
    failwith ("Type constructor not yet fully supported: " ^ con_name ^ " (Phase 3)")
| _ -> failwith ("Unknown type constructor: " ^ con_name))
```

**After:**
```ocaml
| _ -> (
    (* Check if this is a registered enum type *)
    match Typecheck.Enum_registry.lookup con_name with
    | Some enum_def ->
        (* Verify arity matches *)
        let expected_arity = List.length enum_def.type_params in
        let actual_arity = List.length arg_types in
        if expected_arity <> actual_arity then
          failwith (Printf.sprintf 
            "Enum %s expects %d type argument(s), got %d"
            con_name expected_arity actual_arity)
        else
          Types.TEnum (con_name, arg_types)
    | None ->
        failwith ("Unknown type constructor: " ^ con_name)))
```

**What this does:**
1. Looks up enum in the registry (enums are registered during parsing)
2. Validates arity: `option` expects 1 param, `result` expects 2
3. Returns `TEnum("option", [TInt])` instead of failing

**Status after this step:** Inline tests at bottom of `annotation.ml` should PASS ✅

---

### **Step 1.3: Integration Test - Function Parameters with Enum Types**

**File:** `test/test_typecheck_and_codegen.sh` (add after existing enum tests)

**Add these test cases:**
```bash
echo ""
echo "-- ENUM TYPE ANNOTATIONS (Phase 4.3) --"

test_case "Function parameter with enum type option[int]" \
    'enum option[a] { some(a) none }
     let unwrap = fn(x: option[int]) -> int {
       match x {
         option.some(v): v
         option.none: 0
       }
     }
     unwrap(option.some(42))' \
    "true"

test_case "Function returning enum type" \
    'enum option[a] { some(a) none }
     let wrap = fn(x: int) -> option[int] {
       option.some(x)
     }
     let y = wrap(100)
     match y {
       option.some(v): v
       option.none: 0
     }' \
    "true"

test_case "Generic function with enum constraint" \
    'enum option[a] { some(a) none }
     let map_opt = fn[a, b](opt: option[a], f: fn(a) -> b) -> option[b] {
       match opt {
         option.some(v): option.some(f(v))
         option.none: option.none
       }
     }
     let x = option.some(5)
     let y = map_opt(x, fn(n) { n * 2 })
     match y {
       option.some(v): v
       option.none: 0
     }' \
    "true"
```

**Expected after Step 1.2:** These tests should PASS ✅ (TypeScript-style codegen, but annotations work)

---

## **Phase 2: Implement Rust-Style Codegen** (TDD: Make Tests Pass with Concrete Types)

### **Step 2.1: Add Concrete-Only Mode to mono_state**

**File:** `lib/backend/go/emitter.ml` (modify lines 111-119)

**Before:**
```ocaml
type mono_state = {
  mutable func_defs : func_def list;
  mutable instantiations : InstSet.t;
  mutable enum_insts : EnumInstSet.t;
  mutable name_counter : int;
}

let create_mono_state () =
  { func_defs = []; instantiations = InstSet.empty; enum_insts = EnumInstSet.empty; name_counter = 0 }
```

**After:**
```ocaml
type mono_state = {
  mutable func_defs : func_def list;
  mutable instantiations : InstSet.t;
  mutable enum_insts : EnumInstSet.t;
  mutable name_counter : int;
  concrete_only : bool; (* NEW: Rust-style if true, TypeScript-style if false *)
}

let create_mono_state ?(concrete_only=true) () =
  { func_defs = []; instantiations = InstSet.empty; enum_insts = EnumInstSet.empty; 
    name_counter = 0; concrete_only }
```

**What this does:** Adds a flag to control codegen mode. Default is `true` (Rust-style).

---

### **Step 2.2: Modify type_to_go to Error on TVar in Rust-Style Mode**

**File:** `lib/backend/go/emitter.ml` (modify line 54)

**Before:**
```ocaml
| Types.TVar _name -> "interface{}" (* Unresolved type variable - use interface{} *)
```

**After:**
```ocaml
| Types.TVar name -> 
    (* In Rust-style mode, unresolved type variables are an error *)
    if !current_mono_state.concrete_only then
      failwith (Printf.sprintf 
        "Cannot generate code for unresolved type variable '%s'. \
         Add type annotation to resolve this type."
        name)
    else
      "interface{}" (* TypeScript-style fallback *)
```

**Problem:** We need access to `mono_state` inside `type_to_go`. 

**Solution:** Pass `mono_state` as parameter:

**Refactor signature:**
```ocaml
(* Before *)
let rec type_to_go (t : Types.mono_type) : string = ...

(* After *)
let rec type_to_go (state : mono_state) (t : Types.mono_type) : string = ...
```

**Then update all call sites** (grep shows ~30 occurrences). Use search/replace:
- Find: `type_to_go (`
- Replace: `type_to_go state (`

---

### **Step 2.3: Modify emit_enum_type for Concrete Data Field**

**File:** `lib/backend/go/emitter.ml` (modify lines 1086-1138)

**Key change at line 1094:**

**Before:**
```ocaml
let struct_def = Printf.sprintf "type %s struct {\n\tTag int8\n\tData interface{}\n}\n\n" go_type_name in
```

**After:**
```ocaml
(* Determine Data field type based on variants *)
let data_field_type =
  if state.concrete_only then
    (* Rust-style: Use concrete union type of all variant fields *)
    let field_types = 
      List.concat_map (fun (v : Typecheck.Enum_registry.variant_def) -> 
        let subst = List.combine enum_def.type_params type_args in
        List.map (Types.apply_substitution subst) v.fields
      ) enum_def.variants
    in
    match field_types with
    | [] -> "struct{}" (* No fields - nullary enum *)
    | [single] -> type_to_go state single (* Single field type *)
    | multiple -> 
        (* Multiple different types - use interface{} *)
        (* TODO Phase 4.4: Generate tagged union *)
        "interface{}"
  else
    "interface{}" (* TypeScript-style *)
in

let struct_def = Printf.sprintf "type %s struct {\n\tTag int8\n\tData %s\n}\n\n" 
  go_type_name data_field_type in
```

**What this does:**
- In Rust-style mode, analyzes all variant fields
- If all variants have same type (e.g., `option[int]` has only `int` field), uses that type
- Otherwise falls back to `interface{}` temporarily (will improve in Phase 4.4)

---

### **Step 2.4: Update Constructor Functions for Concrete Types**

**File:** `lib/backend/go/emitter.ml` (modify lines 1107-1134)

**Key changes at line 1122:**

**Before:**
```ocaml
let params =
  List.mapi (fun i t -> Printf.sprintf "v%d %s" i (type_to_go t)) field_types
  |> String.concat ", "
in
```

**After:**
```ocaml
let params =
  List.mapi (fun i t -> Printf.sprintf "v%d %s" i (type_to_go state t)) field_types
  |> String.concat ", "
in
```

**No semantic change,** just passing `state` parameter.

---

### **Step 2.5: Update Pattern Matching for Direct Data Access**

**File:** `lib/backend/go/emitter.ml` (modify lines 467-484)

**Before (line 467):**
```ocaml
and emit_match_arm_enum state env enum_name variants scrutinee_type arm =
  (* ... *)
  (* Extract field value with type assertion *)
  let field_val = Printf.sprintf "__scrutinee.Data.(%s)" (type_to_go field_type) in
```

**After:**
```ocaml
and emit_match_arm_enum state env enum_name variants scrutinee_type arm =
  (* ... *)
  (* Extract field value - direct access in Rust-style, type assertion in TS-style *)
  let field_val = 
    if state.mono.concrete_only then
      "__scrutinee.Data" (* Direct access, no cast needed *)
    else
      Printf.sprintf "__scrutinee.Data.(%s)" (type_to_go state field_type) (* TS-style *)
  in
```

**What this does:** Removes type assertion overhead in Rust-style mode.

---

### **Step 2.6: Modify track_enum_inst to Skip Type Variables in Rust Mode**

**File:** `lib/backend/go/emitter.ml` (modify lines 204-210)

**Before:**
```ocaml
let track_enum_inst state (t : Types.mono_type) =
  match t with
  | Types.TEnum (name, args) ->
      (* Track all enum instantiations, even with type variables *)
      state.enum_insts <- EnumInstSet.add (name, args) state.enum_insts
  | _ -> ()
```

**After:**
```ocaml
let track_enum_inst state (t : Types.mono_type) =
  match t with
  | Types.TEnum (name, args) ->
      (* In Rust-style mode, only track fully concrete instantiations *)
      if state.concrete_only && List.exists has_type_vars args then
        (* Skip - this enum has unresolved type variables *)
        (* The user needs to add type annotations *)
        ()
      else
        state.enum_insts <- EnumInstSet.add (name, args) state.enum_insts
  | _ -> ()
```

**What this does:** Prevents generating `option_any` with `interface{}` in Rust mode.

---

### **Step 2.7: Update emit_program_with_typed_env to Use Rust-Style by Default**

**File:** `lib/backend/go/emitter.ml` (modify line 1145)

**Before:**
```ocaml
let emit_program_with_typed_env (typed_env : Infer.type_env) (program : AST.program) : string =
  let mono_state = create_mono_state () in
```

**After:**
```ocaml
let emit_program_with_typed_env ?(concrete_only=true) (typed_env : Infer.type_env) (program : AST.program) : string =
  let mono_state = create_mono_state ~concrete_only () in
```

**What this does:** Makes Rust-style the default, but allows opt-in to TypeScript-style.

---

## **Phase 3: Improve Type Inference & Error Messages**

### **Step 3.1: Better Error Messages for Unresolved Enums**

**File:** `lib/frontend/typecheck/infer.ml` (add new error type at line 100)

**Add after line 100:**
```ocaml
| UnannotatedEnum of string * string (* enum_name, constructor_name *)
```

**Modify error formatting (find `error_to_string` function):**
```ocaml
| UnannotatedEnum (enum_name, constructor_name) ->
    Printf.sprintf 
      "Cannot infer type parameter for %s.%s. \
       Add a type annotation like: let x: %s[int] = %s.%s(...)"
      enum_name constructor_name enum_name enum_name constructor_name
```

---

### **Step 3.2: Detect Unannotated option.none and Suggest Fix**

**File:** `lib/frontend/typecheck/infer.ml` (modify enum constructor inference)

**Find the function that infers enum constructors** (search for `infer_enum_constructor` or `EnumConstructor`):

**Add check:**
```ocaml
(* After inferring type *)
let inferred_enum_type = Types.TEnum (enum_name, type_args) in

(* Check if type_args contain unresolved variables *)
if List.exists (fun t -> match t with TVar _ -> true | _ -> false) type_args then
  (* User needs annotation *)
  Error (UnannotatedEnum (enum_name, variant_name))
else
  Ok (subst, inferred_enum_type)
```

---

### **Step 3.3: Propagate Constraints from Function Signatures**

**File:** `lib/frontend/typecheck/infer.ml`

**Goal:** When calling `fn(x: option[int])`, ensure argument type is constrained.

**Find function call inference** (search for `infer_call` or `Call`):

**After unifying argument types:**
```ocaml
(* Unify each argument with parameter type *)
let arg_types, subst' = 
  List.fold_left2 (fun (acc_types, acc_subst) arg param_type ->
    let arg_subst, arg_type = infer_expression env arg in
    let combined_subst = compose_substitution acc_subst arg_subst in
    (* Unify argument type with parameter type *)
    match unify (apply_substitution combined_subst param_type) 
                 (apply_substitution combined_subst arg_type) with
    | Ok unify_subst -> 
        let final_subst = compose_substitution combined_subst unify_subst in
        (arg_type :: acc_types, final_subst)
    | Error e -> Error (UnificationError e)
  ) ([], subst) args param_types
in
```

**This ensures:** If function expects `option[int]`, passing `option.none` will unify with `option[int]`.

---

## **Phase 4: Testing & Validation**

### **Step 4.1: Update All Integration Tests**

**File:** `test/test_typecheck_and_codegen.sh`

**Add comprehensive test suite:**

```bash
echo ""
echo "-- RUST-STYLE ENUM CODEGEN (Phase 4.3) --"

test_case "Rust-style option[int] generates concrete type" \
    'enum option[a] { some(a) none }
     let x: option[int] = option.some(42)
     let y = match x {
       option.some(v): v + 10
       option.none: 0
     }
     y' \
    "true"

test_case "Unannotated option.none errors with helpful message" \
    'enum option[a] { some(a) none }
     let x = option.none
     x' \
    "false" \
    "Cannot infer type parameter"

test_case "Function parameter constrains enum type" \
    'enum option[a] { some(a) none }
     let f = fn(x: option[int]) -> int {
       match x {
         option.some(v): v
         option.none: 0
       }
     }
     f(option.none)' \
    "true"

test_case "Nested enum types option[list[int]]" \
    'enum option[a] { some(a) none }
     let f = fn(x: option[list[int]]) -> int {
       match x {
         option.some(lst): len(lst)
         option.none: 0
       }
     }
     f(option.some([1, 2, 3]))' \
    "true"

test_case "TypeScript-style fallback for polymorphic code" \
    'enum option[a] { some(a) none }
     let id = fn(x) { x }
     id(option.some(42))' \
    "true"
```

---

## **Implementation Timeline**

| Phase | Tasks | Time Estimate | Dependencies |
|-------|-------|---------------|--------------|
| **Phase 1** | Enable enum type annotations | 2-3 hours | None |
| Step 1.1 | Write failing tests | 30 min | |
| Step 1.2 | Implement annotation parsing | 1 hour | |
| Step 1.3 | Integration tests | 1 hour | |
| **Phase 2** | Rust-style codegen | 4-6 hours | Phase 1 complete |
| Step 2.1-2.2 | Add concrete_only mode | 1 hour | |
| Step 2.3-2.4 | Concrete Data field | 2 hours | |
| Step 2.5-2.7 | Pattern matching + tracking | 2 hours | |
| **Phase 3** | Better inference & errors | 2-3 hours | Phase 2 complete |
| Step 3.1-3.2 | Error messages | 1 hour | |
| Step 3.3 | Constraint propagation | 2 hours | |
| **Phase 4** | Testing & validation | 3-4 hours | Phase 3 complete |
| Step 4.1 | Integration tests | 1 hour | |
| Step 4.2 | Code quality verification | 1 hour | |
| Step 4.3 | Performance benchmarks | 1-2 hours | |

**Total Time:** 12-18 hours (~2-3 days of focused work)

---

## **Success Criteria**

✅ **All 40+ tests passing** (existing + new Phase 4.3 tests)  
✅ **Concrete types generated:** `Data int64` not `Data interface{}`  
✅ **No type assertions in pattern matching:** Direct `__scrutinee.Data` access  
✅ **Better error messages:** "Cannot infer type parameter for option.none"  
✅ **Performance:** 2x speedup on benchmark (measured with `benchmark_enums.sh`)  
✅ **Backwards compatible:** `--ts-style` flag still works  
✅ **Documentation complete:** CLAUDE.md, README.md updated  

---

**Last Updated:** Feb 4, 2026  
**Status:** Ready for execution
