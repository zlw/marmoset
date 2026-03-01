#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - ENUMS"
echo "-- ENUM CONSTRUCTOR TESTS (Phase 4.2) --"
test_case "Simple enum definition" \
    'enum direction { north south east west }' \
    "true"

expect_build "Duplicate enum definition is rejected in one program" "Duplicate enum definition: direction" << 'EOF'
enum direction { north south east west }
enum direction { up down }
puts(1)
EOF

test_case "Generic enum option[a] with some constructor" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     x' \
    "true"

test_case "Generic enum option[a] with none constructor" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     let y = match x {
       option.some(v): v
       option.none: 0
     }
     y' \
    "true"

test_case "Enum list[a] with recursive structure" \
    'enum list[a] { cons(a) nil }
     let x = list.cons(42)
     x' \
    "true"

test_case "Enum with multi-field variant" \
    'enum http_response { ok(int, string) error(int, string) }
     let x = http_response.ok(200, "OK")
     x' \
    "true"

echo ""
echo "-- MATCH EXPRESSION TESTS (Phase 4.2) --"
test_case "Match on simple enum variant" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     let y = match x {
       option.some(v): v
       option.none: 0
     }
     y' \
    "true"

test_case "Match with wildcard pattern" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     let y = match x {
       option.some(v): v
       _: 0
     }
     y' \
    "true"

test_case "Match with literal patterns" \
    'let x = 42
     let y = match x {
       0: "zero"
       1: "one"
       _: "other"
     }
     y' \
    "true"

test_case "Match with variable binding" \
    'let x = 42
     let y = match x {
       n: n + 1
     }
     y' \
    "true"

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

echo ""
echo "-- PHASE 4.4: MULTI-FIELD ENUM VARIANTS --"

test_case "Multi-field variant extraction" \
    'enum http_response { ok(int, string) error(int, string) }
     let x = http_response.ok(200, "OK")
     match x {
       http_response.ok(code, _): code
       http_response.error(code, _): code
     }' \
    "true"

test_case "Heterogeneous result type" \
    'enum result[a, b] { ok(a) error(b) }
     let x: result[int, string] = result.ok(42)
     match x {
       result.ok(v): v + 1
       result.error(_): 0
     }' \
    "true"

test_case "Three-field variant" \
    'enum triple { t(int, string, bool) }
     let x = triple.t(1, "two", true)
     match x {
       triple.t(a, _, _): a
     }' \
    "true"

test_case "Mixed field count variants" \
    'enum mixed { none zero(int) pair(int, int) triple(int, int, int) }
     let x = mixed.pair(1, 2)
     match x {
       mixed.none: 0
       mixed.zero(a): a
       mixed.pair(a, _): a
       mixed.triple(a, _, _): a
     }' \
    "true"


# -------------------------------------------------------------------
# Imported From 03_enums_edge_cases.sh
# -------------------------------------------------------------------

########################################
# SECTION A: MANY VARIANTS (10+)
########################################

expect_runtime_output "Enum with 10+ variants - middle variant" "5" << 'EOF'
enum planet { mercury venus earth mars jupiter saturn uranus neptune pluto ceres eris }
let classify = fn(p: planet) -> int {
  match p {
    planet.mercury: 1
    planet.venus: 2
    planet.earth: 3
    planet.mars: 4
    planet.jupiter: 5
    planet.saturn: 6
    planet.uranus: 7
    planet.neptune: 8
    planet.pluto: 9
    planet.ceres: 10
    planet.eris: 11
  }
}
puts(classify(planet.jupiter))
EOF

expect_runtime_output "Enum with 10+ variants - last variant matches" "11" << 'EOF'
enum planet { mercury venus earth mars jupiter saturn uranus neptune pluto ceres eris }
let classify = fn(p: planet) -> int {
  match p {
    planet.mercury: 1
    planet.venus: 2
    planet.earth: 3
    planet.mars: 4
    planet.jupiter: 5
    planet.saturn: 6
    planet.uranus: 7
    planet.neptune: 8
    planet.pluto: 9
    planet.ceres: 10
    planet.eris: 11
  }
}
puts(classify(planet.eris))
EOF

expect_runtime_output "Enum with 10+ variants - wildcard catches the rest" "0" << 'EOF'
enum planet { mercury venus earth mars jupiter saturn uranus neptune pluto ceres eris }
let is_earth = fn(p: planet) -> int {
  match p {
    planet.earth: 1
    _: 0
  }
}
puts(is_earth(planet.saturn))
EOF

########################################
# SECTION B: VARIANT NAME SHADOWING
########################################

expect_runtime_output "Enum variant name same as let binding does not shadow" "hello" << 'EOF'
let x = "hello"
enum myenum { x y z }
puts(x)
EOF

expect_runtime_output "Enum name coexists with other variables" "42" << 'EOF'
enum color { red green blue }
let color_val = 42
puts(color_val)
EOF

########################################
# SECTION C: NESTED ENUMS (enum containing enum as payload)
########################################

expect_runtime_output "Enum with another enum as payload - extract inner" "42" << 'EOF'
enum inner { a(int) b }
enum outer { wrap(inner) empty }
let x = outer.wrap(inner.a(42))
let result = match x {
  outer.wrap(i): match i {
    inner.a(n): n
    inner.b: 0
  }
  outer.empty: 0
}
puts(result)
EOF

expect_runtime_output "Nested enum - inner nullary case" "0" << 'EOF'
enum inner { a(int) b }
enum outer { wrap(inner) empty }
let x = outer.wrap(inner.b)
let result = match x {
  outer.wrap(i): match i {
    inner.a(n): n
    inner.b: 0
  }
  outer.empty: 0
}
puts(result)
EOF

expect_runtime_output "Nested enum - outer empty case" "-1" << 'EOF'
enum inner { a(int) b }
enum outer { wrap(inner) empty }
let x = outer.empty
let result = match x {
  outer.wrap(i): match i {
    inner.a(n): n
    inner.b: 0
  }
  outer.empty: 0 - 1
}
puts(result)
EOF

########################################
# SECTION D: GENERIC ENUM INSTANTIATED AT MULTIPLE TYPES
########################################

# Separate functions avoid the __scrutinee reuse bug.
expect_runtime_output "Generic enum at 4 types via separate functions" "4" << 'EOF'
enum box[a] { full(a) empty }
let count_int = fn(x: box[int]) -> int {
  match x { box.full(_): 1 box.empty: 0 }
}
let count_str = fn(x: box[string]) -> int {
  match x { box.full(_): 1 box.empty: 0 }
}
let count_bool = fn(x: box[bool]) -> int {
  match x { box.full(_): 1 box.empty: 0 }
}
let count_list = fn(x: box[list[int]]) -> int {
  match x { box.full(_): 1 box.empty: 0 }
}
let a = box.full(42)
let b = box.full("hello")
let c = box.full(true)
let d = box.full([1, 2, 3])
puts(count_int(a) + count_str(b) + count_bool(c) + count_list(d))
EOF

expect_runtime_output "Generic enum - extract int value" "42" << 'EOF'
enum box[a] { full(a) empty }
let unwrap_int = fn(x: box[int]) -> int {
  match x { box.full(v): v box.empty: 0 }
}
puts(unwrap_int(box.full(42)))
EOF

expect_runtime_output "Generic enum - extract string value" "hello" << 'EOF'
enum box[a] { full(a) empty }
let unwrap_str = fn(x: box[string]) -> string {
  match x { box.full(v): v box.empty: "" }
}
puts(unwrap_str(box.full("hello")))
EOF

########################################
# SECTION E: ENUM AS FUNCTION PARAMETER AND RETURN TYPE
########################################

expect_runtime_output "Enum as function parameter" "1" << 'EOF'
enum direction { north south east west }
let is_north = fn(d: direction) -> int {
  match d {
    direction.north: 1
    _: 0
  }
}
puts(is_north(direction.north))
EOF

expect_runtime_output "Enum as function return type" "south" << 'EOF'
enum direction { north south east west }
let opposite = fn(d: direction) -> direction {
  match d {
    direction.north: direction.south
    direction.south: direction.north
    direction.east: direction.west
    direction.west: direction.east
  }
}
puts(opposite(direction.north))
EOF

expect_runtime_output "Enum round-trip through wrap/unwrap" "42" << 'EOF'
enum option[a] { some(a) none }
let wrap = fn(x: int) -> option[int] { option.some(x) }
let unwrap = fn(x: option[int]) -> int {
  match x { option.some(v): v option.none: 0 }
}
puts(unwrap(wrap(42)))
EOF

expect_runtime_output "Higher-order function operating on enum" "99" << 'EOF'
enum option[a] { some(a) none }
let map_opt = fn(x: option[int], f: fn(int) -> int) -> option[int] {
  match x {
    option.some(v): option.some(f(v))
    option.none: option.none()
  }
}
let double = fn(n: int) -> int { n * 2 }
let result = map_opt(option.some(49), double)
let out = match result {
  option.some(v): v + 1
  option.none: 0
}
puts(out)
EOF

########################################
# SECTION F: ENUM IN RECORD FIELD
########################################

expect_runtime_output "Record with enum payload field - some" "42" << 'EOF'
enum option[a] { some(a) none }
let r = { value: option.some(42) }
let out = match r.value {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

# Regression: Nullary generic constructor in record field should infer from
# downstream match context and compile.
expect_runtime_output "Record with nullary generic enum field infers from match context" "0" << 'EOF'
enum option[a] { some(a) none }
let r = { value: option.none() }
let out = match r.value {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

expect_runtime_output "Record with simple enum field" "east" << 'EOF'
enum direction { north south east west }
let r = { name: "compass", dir: direction.east }
puts(r.dir)
EOF

########################################
# SECTION G: ENUM IN ARRAY/LIST
########################################

expect_runtime_output "List of simple enum values" "3" << 'EOF'
enum color { red green blue }
let colors = [color.red, color.green, color.blue]
puts(len(colors))
EOF

expect_runtime_output "List of generic enum values has correct length" "3" << 'EOF'
enum option[a] { some(a) none }
let opts = [option.some(1), option.some(2), option.none()]
puts(len(opts))
EOF

expect_runtime_output "Index into list of enums and match" "green" << 'EOF'
enum color { red green blue }
let colors = [color.red, color.green, color.blue]
let c = colors[1]
let name = match c {
  color.red: "red"
  color.green: "green"
  color.blue: "blue"
}
puts(name)
EOF

########################################
# SECTION H: MULTIPLE ENUM DEFINITIONS IN SAME PROGRAM
########################################
#
# Regression: multiple match expressions in the same scope should use distinct
# scrutinee temporaries and compile cleanly.
expect_runtime_output "Two enums matched in same function body" "1" << 'EOF'
enum color { red green blue }
enum shape { circle square triangle }
let describe = fn(c: color, s: shape) -> int {
  let cv = match c { color.red: 1 _: 0 }
  let sv = match s { shape.circle: 1 _: 0 }
  cv * sv
}
puts(describe(color.red, shape.circle))
EOF

expect_runtime_output "Three enums at top level" "6" << 'EOF'
enum ea { x y }
enum eb { m n }
enum ec { p q r }
let va = match ea.x { ea.x: 1 ea.y: 2 }
let vb = match eb.n { eb.m: 10 eb.n: 2 }
let vc = match ec.r { ec.p: 100 ec.q: 200 ec.r: 3 }
puts(va + vb + vc)
EOF

expect_runtime_output "Two matches on same enum type at top level" "3" << 'EOF'
enum color { red green blue }
let c = color.green
let a = match c { color.red: 1 color.green: 2 color.blue: 3 }
let b = match c { color.red: 10 color.green: 1 color.blue: 0 }
puts(a + b)
EOF

# However, separate function bodies each get their own scope, so this should work:
expect_runtime_output "Two enums matched in separate functions work" "3" << 'EOF'
enum fruit { apple banana cherry }
enum veggie { carrot broccoli spinach }
let count_fruit = fn(f: fruit) -> int {
  match f { fruit.cherry: 1 _: 0 }
}
let count_veggie = fn(v: veggie) -> int {
  match v { veggie.spinach: 2 _: 0 }
}
puts(count_fruit(fruit.cherry) + count_veggie(veggie.spinach))
EOF

########################################
# SECTION I: COMPLEX PAYLOAD EXPRESSIONS
########################################

expect_runtime_output "Enum variant with precomputed arithmetic payload" "15" << 'EOF'
enum option[a] { some(a) none }
let val = 3 * 5
let x = option.some(val)
let result = match x {
  option.some(v): v
  option.none: 0
}
puts(result)
EOF

# BUG PROBE: Infix expression inside constructor call may have precedence issues.
# option.some(3 * 5) could parse as (option.some(3)) * 5.
expect_runtime_output "Enum variant with inline arithmetic as payload" "15" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(3 * 5)
let result = match x {
  option.some(v): v
  option.none: 0
}
puts(result)
EOF

expect_runtime_output "Enum variant with function call result as payload" "10" << 'EOF'
enum option[a] { some(a) none }
let double = fn(n: int) -> int { n * 2 }
let x = option.some(double(5))
let result = match x {
  option.some(v): v
  option.none: 0
}
puts(result)
EOF

expect_runtime_output "Enum variant with if-expression as payload" "100" << 'EOF'
enum option[a] { some(a) none }
let flag = true
let x = option.some(if (flag) { 100 } else { 0 })
let result = match x {
  option.some(v): v
  option.none: 0
}
puts(result)
EOF

expect_runtime_output "Enum variant with match-expression as payload" "7" << 'EOF'
enum option[a] { some(a) none }
enum color { red green blue }
let c = color.green
let x = option.some(match c { color.red: 5 color.green: 7 color.blue: 9 })
let result = match x {
  option.some(v): v
  option.none: 0
}
puts(result)
EOF

########################################
# SECTION J: ENUM USED IN IF-EXPRESSION CONDITION
########################################

expect_runtime_output "Enum match used as if condition" "yes" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(42)
let is_some = match x {
  option.some(_): true
  option.none: false
}
let out = if (is_some) { "yes" } else { "no" }
puts(out)
EOF

expect_runtime_output "Enum match directly in if condition" "present" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(1)
let out = if (match x { option.some(_): true option.none: false }) {
  "present"
} else {
  "absent"
}
puts(out)
EOF

########################################
# SECTION K: EXHAUSTIVENESS CHECKING
########################################

expect_build "Non-exhaustive match on simple enum is rejected" "Non-exhaustive" << 'EOF'
enum direction { north south east west }
let d = direction.north
match d {
  direction.north: 1
  direction.south: 2
}
EOF

expect_build "Non-exhaustive match on generic enum is rejected" "Non-exhaustive" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(42)
match x {
  option.some(v): v
}
EOF

expect_runtime_output "Exhaustive match on all variants passes" "3" << 'EOF'
enum direction { north south east west }
let d = direction.east
let v = match d {
  direction.north: 1
  direction.south: 2
  direction.east: 3
  direction.west: 4
}
puts(v)
EOF

########################################
# SECTION L: WILDCARD AND VARIABLE PATTERNS
########################################

expect_runtime_output "Wildcard catches remaining variants" "0" << 'EOF'
enum direction { north south east west }
let d = direction.west
let v = match d {
  direction.north: 1
  _: 0
}
puts(v)
EOF

# Regression: Variable pattern in enum match can be unused in the arm body
# without causing Go "declared and not used" errors.
expect_runtime_output "Variable pattern in enum match allows unused binding" "42" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(5)
let v = match x {
  y: 42
}
puts(v)
EOF

# BUG PROBE: Wildcard before specific pattern. Go switch `default` is
# always evaluated last regardless of source position, so the specific
# case wins. This diverges from ML-style top-to-bottom match semantics
# where the first matching arm should win.
expect_runtime_output "Wildcard before specific pattern - Go default semantics" "1" << 'EOF'
enum direction { north south east west }
let d = direction.north
let v = match d {
  _: 0
  direction.north: 1
}
puts(v)
EOF

########################################
# SECTION M: MULTI-PATTERN ARMS (| separator)
########################################

# Multi-pattern match arms parse but codegen is not yet implemented.
expect_build "Multi-pattern arm hits codegen limitation" "Multiple patterns" << 'EOF'
enum color { red orange yellow green blue purple }
let c = color.orange
let temp = match c {
  color.red | color.orange | color.yellow: "warm"
  color.green | color.blue | color.purple: "cool"
}
puts(temp)
EOF

########################################
# SECTION N: MATCH ARM BODY CREATES NEW ENUM VALUE
########################################

# Regression: constructing an enum in one match and re-matching it should work.
expect_runtime_output "Match then re-match same enum" "99" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(99)
let y = match x {
  option.some(v): option.some(v)
  option.none: option.none()
}
let result = match y {
  option.some(v): v
  option.none: 0
}
puts(result)
EOF

expect_runtime_output "Match returns different enum type then re-matched" "7" << 'EOF'
enum color { red green blue }
enum size { small medium large }
let c = color.green
let s = match c {
  color.red: size.small
  color.green: size.medium
  color.blue: size.large
}
let out = match s {
  size.small: 3
  size.medium: 7
  size.large: 11
}
puts(out)
EOF

# Workaround: use a function to isolate each match in its own scope.
expect_runtime_output "Match-then-rematch works when isolated in functions" "99" << 'EOF'
enum option[a] { some(a) none }
let identity = fn(x: option[int]) -> option[int] {
  match x {
    option.some(v): option.some(v)
    option.none: option.none()
  }
}
let unwrap = fn(x: option[int]) -> int {
  match x { option.some(v): v option.none: 0 }
}
puts(unwrap(identity(option.some(99))))
EOF

########################################
# SECTION O: GENERIC ENUM WITH MULTIPLE TYPE PARAMS
########################################

expect_runtime_output "result[a, b] with two type params - success case" "42" << 'EOF'
enum result[a, b] { ok(a) err(b) }
let x: result[int, string] = result.ok(42)
let v = match x {
  result.ok(n): n
  result.err(_): 0
}
puts(v)
EOF

expect_runtime_output "result[a, b] with two type params - error case" "0" << 'EOF'
enum result[a, b] { ok(a) err(b) }
let x: result[int, string] = result.err("oops")
let v = match x {
  result.ok(n): n
  result.err(_): 0
}
puts(v)
EOF

expect_runtime_output "result[a, b] with complex type args" "hello" << 'EOF'
enum result[a, b] { ok(a) err(b) }
let x: result[string, int] = result.ok("hello")
let v = match x {
  result.ok(s): s
  result.err(_): "error"
}
puts(v)
EOF

########################################
# SECTION P: MULTI-FIELD ENUM VARIANTS (STRESS)
########################################

expect_runtime_output "Three-field variant extraction" "6" << 'EOF'
enum data { triple(int, int, int) empty }
let x = data.triple(1, 2, 3)
let sum = match x {
  data.triple(a, b, c): a + b + c
  data.empty: 0
}
puts(sum)
EOF

expect_runtime_output "Mixed-type multi-field variant" "hello" << 'EOF'
enum tagged { item(int, string, bool) nothing }
let x = tagged.item(42, "hello", true)
let out = match x {
  tagged.item(_, s, _): s
  tagged.nothing: "nothing"
}
puts(out)
EOF

expect_runtime_output "Four-field variant" "10" << 'EOF'
enum quad { q(int, int, int, int) z }
let x = quad.q(1, 2, 3, 4)
let sum = match x {
  quad.q(a, b, c, d): a + b + c + d
  quad.z: 0
}
puts(sum)
EOF

########################################
# SECTION Q: ENUM CONSTRUCTOR ARITY ERRORS
########################################

expect_build "Enum constructor with too many args is rejected" "expects" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(1, 2)
puts(x)
EOF

expect_build "Enum constructor with too few args is rejected" "expects" << 'EOF'
enum data { pair(int, int) }
let x = data.pair(1)
puts(x)
EOF

expect_build "Unknown variant is rejected" "Unknown" << 'EOF'
enum color { red green blue }
let x = color.yellow
puts(x)
EOF

# Accessing an undefined enum name gives "Unbound variable" because the
# identifier is resolved as a variable before enum lookup.
expect_build "Unknown enum name gives unbound variable" "Unbound" << 'EOF'
let x = nonexistent.foo
puts(x)
EOF

########################################
# SECTION R: ENUM WITH ONLY NULLARY VARIANTS
########################################

expect_runtime_output "All-nullary enum match" "2" << 'EOF'
enum traffic { red yellow green }
let t = traffic.yellow
let v = match t {
  traffic.red: 1
  traffic.yellow: 2
  traffic.green: 3
}
puts(v)
EOF

expect_runtime_output "All-nullary enum printed via puts uses String method" "red" << 'EOF'
enum traffic { red yellow green }
puts(traffic.red)
EOF

########################################
# SECTION S: ENUM WITH ONLY PAYLOAD VARIANTS
########################################

expect_runtime_output "All-payload variants - add branch" "30" << 'EOF'
enum expr { lit(int) add(int, int) mul(int, int) }
let e = expr.add(10, 20)
let v = match e {
  expr.lit(n): n
  expr.add(a, b): a + b
  expr.mul(a, b): a * b
}
puts(v)
EOF

expect_runtime_output "All-payload variants - mul branch" "200" << 'EOF'
enum expr { lit(int) add(int, int) mul(int, int) }
let e = expr.mul(10, 20)
let v = match e {
  expr.lit(n): n
  expr.add(a, b): a + b
  expr.mul(a, b): a * b
}
puts(v)
EOF

########################################
# SECTION T: DUPLICATE ENUM DEFINITION
########################################

expect_build "Duplicate enum definition in same program rejected" "Duplicate enum definition" << 'EOF'
enum color { red green blue }
enum color { cyan magenta yellow }
puts(1)
EOF

########################################
# SECTION U: ENUM PATTERN MATCH IN FUNCTION BODY
########################################

expect_runtime_output "Function with enum match in body" "matched" << 'EOF'
enum option[a] { some(a) none }
let describe = fn(x: option[int]) -> string {
  match x {
    option.some(_): "matched"
    option.none: "empty"
  }
}
puts(describe(option.some(42)))
EOF

expect_runtime_output "Recursive function with enum parameter" "120" << 'EOF'
enum option[a] { some(a) none }
let fact = fn(n: int) -> int {
  if (n <= 1) { 1 } else { n * fact(n - 1) }
}
let safe_fact = fn(x: option[int]) -> int {
  match x {
    option.some(n): fact(n)
    option.none: 0
  }
}
puts(safe_fact(option.some(5)))
EOF

########################################
# SECTION V: GENERIC ENUM WITH LIST PAYLOAD
########################################

expect_runtime_output "Generic enum wrapping a list" "3" << 'EOF'
enum option[a] { some(a) none }
let x = option.some([10, 20, 30])
let result = match x {
  option.some(lst): len(lst)
  option.none: 0
}
puts(result)
EOF

########################################
# SECTION W: ENUM MIXED WITH RECORDS
########################################

expect_runtime_output "Enum variant wrapping a record" "alice" << 'EOF'
enum option[a] { some(a) none }
let x = option.some({ name: "alice", age: 30 })
let result = match x {
  option.some(r): r.name
  option.none: "nobody"
}
puts(result)
EOF

expect_runtime_output "Record containing enum accessed through function" "5" << 'EOF'
enum option[a] { some(a) none }
let r = { value: option.some(5), label: "test" }
let extract = fn(opt: option[int]) -> int {
  match opt { option.some(v): v option.none: 0 }
}
puts(extract(r.value))
EOF

########################################
# SECTION X: BUILTIN ENUMS NOT AVAILABLE FROM USER CODE
########################################

# The enum registry has builtins (option, result, ordering) registered
# internally for type system use, but they are not in the name resolution
# scope for user code. Users must define their own enum declarations.
expect_build "Builtin option not available without explicit declaration" "Unbound" << 'EOF'
let x = option.some(42)
puts(x)
EOF

expect_build "Builtin result not available without explicit declaration" "Unbound" << 'EOF'
let x = result.success(100)
puts(x)
EOF

########################################
# SECTION Y: EDGE CASES IN PATTERN MATCHING
########################################

expect_runtime_output "All-wildcard field patterns in constructor" "matched" << 'EOF'
enum pair { p(int, int) }
let x = pair.p(1, 2)
let out = match x {
  pair.p(_, _): "matched"
}
puts(out)
EOF

expect_runtime_output "Match arm with effectful body (puts)" "hello" << 'EOF'
enum option[a] { some(a) none }
let x = option.some("hello")
match x {
  option.some(v): puts(v)
  option.none: puts("nothing")
}
EOF

expect_runtime_output "Match inside match arm body" "deep" << 'EOF'
enum color { red green blue }
enum size { small large }
let c = color.green
let s = size.small
let out = match c {
  color.red: "red"
  color.green: match s {
    size.small: "deep"
    size.large: "wide"
  }
  color.blue: "blue"
}
puts(out)
EOF

########################################
# SECTION Z: STRESS AND UNUSUAL COMBINATIONS
########################################

expect_runtime_output "Enum value passed through chain of functions" "42" << 'EOF'
enum option[a] { some(a) none }
let id = fn(x: option[int]) -> option[int] { x }
let wrap = fn(n: int) -> option[int] { option.some(n) }
let unwrap = fn(x: option[int]) -> int {
  match x { option.some(v): v option.none: 0 }
}
puts(unwrap(id(wrap(42))))
EOF

expect_runtime_output "Enum with bool payload" "true" << 'EOF'
enum wrapper { w(bool) }
let x = wrapper.w(true)
let v = match x {
  wrapper.w(b): b
}
puts(v)
EOF

expect_runtime_output "Enum with string payload" "hello world" << 'EOF'
enum option[a] { some(a) none }
let x = option.some("hello world")
let v = match x {
  option.some(s): s
  option.none: ""
}
puts(v)
EOF

# BUG: Different variants of same generic enum in separate let bindings
# where option.none() lacks type context causes unresolved type variable.
expect_build "Untyped none alongside some - unresolved TVar" "unresolved type variable" << 'EOF'
enum option[a] { some(a) none }
let a = option.some(42)
let b = option.none()
let va = match a { option.some(v): v option.none: 0 }
let vb = match b { option.some(v): v option.none: 0 }
puts(va + vb)
EOF

expect_runtime_output "First element of enum list matched" "10" << 'EOF'
enum option[a] { some(a) none }
let xs = [option.some(10), option.some(20), option.none()]
let first_val = match xs[0] {
  option.some(v): v
  option.none: 0
}
puts(first_val)
EOF

expect_runtime_output "Simplest possible enum match" "1" << 'EOF'
enum bit { zero one }
let b = bit.one
let v = match b {
  bit.zero: 0
  bit.one: 1
}
puts(v)
EOF

expect_runtime_output "Enum with single variant" "42" << 'EOF'
enum wrapper { wrap(int) }
let x = wrapper.wrap(42)
let v = match x {
  wrapper.wrap(n): n
}
puts(v)
EOF

expect_runtime_output "Enum match function called multiple times" "3" << 'EOF'
enum option[a] { some(a) none }
let unwrap = fn(x: option[int], fallback: int) -> int {
  match x {
    option.some(v): v
    option.none: fallback
  }
}
let a = unwrap(option.some(1), 0)
let b = unwrap(option.some(2), 0)
let c = unwrap(option.none(), 99)
puts(a + b + c - 99)
EOF

expect_runtime_output "All match arms return same constant type" "42" << 'EOF'
enum choice { left right up down }
let c = choice.up
let v = match c {
  choice.left: 10
  choice.right: 20
  choice.up: 42
  choice.down: 50
}
puts(v)
EOF

expect_runtime_output "Direct constructor in match scrutinee" "hello" << 'EOF'
enum option[a] { some(a) none }
let v = match option.some("hello") {
  option.some(s): s
  option.none: "empty"
}
puts(v)
EOF

# Regression: two top-level matches on the same generic enum should compile and run.
expect_runtime_output "Two top-level matches with same generic enum" "150" << 'EOF'
enum option[a] { some(a) none }
let base = option.some(100)
let bonus = option.some(50)
let b = match base { option.some(v): v option.none: 0 }
let n = match bonus { option.some(v): v option.none: 0 }
puts(b + n)
EOF

expect_runtime_output "Constructor pattern allows partially unused bindings" "1" << 'EOF'
enum pair { p(int, int) }
let x = pair.p(1, 2)
let out = match x {
  pair.p(a, b): a
}
puts(out)
EOF

suite_end
