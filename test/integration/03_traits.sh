#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - TRAITS"
echo "-- PHASE 4.3: TRAIT SYSTEM --"

test_case "Basic trait definition" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     42' \
    "true"

test_case "Trait with multiple methods" \
    'trait eq[a] {
       fn eq(x: a, y: a) -> bool
       fn ne(x: a, y: a) -> bool
     }
     42' \
    "true"

run_case_from_stdin "Method-only trait without type parameter" "42" << 'EOF'
trait ping {
  fn ping(x: int) -> int
}
impl ping for int {
  fn ping(x: int) -> int {
    x
  }
}
puts(42.ping())
EOF

run_case_from_stdin "Field-only trait constraint accepts matching record shape" "alice" << 'EOF'
trait named {
  name: string
}
let get_name = fn[t: named](x: t) -> string {
  x.name
};
let person = { name: "alice", age: 42 }
puts(get_name(person))
EOF

run_build_fail_contains_from_stdin "Field-only trait constraint rejects missing required field" "missing required field 'name'" << 'EOF'
trait named {
  name: string
}
let get_name = fn[t: named](x: t) -> string {
  "ok"
};
get_name({ age: 42 })
EOF

test_case "Trait with supertraits" \
    'trait eq[a] {
       fn eq(x: a, y: a) -> bool
     }
     trait ord[a]: eq {
       fn compare(x: a, y: a) -> int
     }
     42' \
    "true"

test_case "Basic impl for primitive" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     impl show for int {
       fn show(x: int) -> string {
         "int"
       }
     }
     42' \
    "true"

test_case "Impl with multiple methods" \
    'trait eq[a] {
       fn eq(x: a, y: a) -> bool
       fn ne(x: a, y: a) -> bool
     }
     impl eq for int {
       fn eq(x: int, y: int) -> bool {
         true
       }
       fn ne(x: int, y: int) -> bool {
         false
       }
     }
     42' \
    "true"

test_case "Multiple impls for different types" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     impl show for int {
       fn show(x: int) -> string {
         "int"
       }
     }
     impl show for bool {
       fn show(x: bool) -> string {
         "bool"
       }
     }
     42' \
    "true"

test_case "Duplicate impl for same trait and type fails" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     impl show for int {
       fn show(x: int) -> string {
         "first"
       }
     }
     impl show for int {
       fn show(x: int) -> string {
         "second"
       }
     }
     42' \
    "false" \
    "Duplicate impl for trait"

run_build_fail_contains_from_stdin "Ambiguous method dispatch fails during typecheck" "Ambiguous method 'render'" << 'EOF'
trait render_a[a] {
  fn render(x: a) -> string
}
trait render_b[a] {
  fn render(x: a) -> string
}
impl render_a for int {
  fn render(x: int) -> string { "a" }
}
impl render_b for int {
  fn render(x: int) -> string { "b" }
}
let x = 1
puts(x.render())
EOF

test_case "Derive single trait for primitive" \
    'trait eq[a] {
       fn eq(x: a, y: a) -> bool
     }
     derive eq for int;
     42' \
    "true"

test_case "Derive multiple traits" \
    'trait eq[a] {
       fn eq(x: a, y: a) -> bool
     }
     trait show[a] {
       fn show(x: a) -> string
     }
     derive eq, show for int;
     42' \
    "true"

test_case "Derive for multiple types" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     derive show for int;
     derive show for bool;
     derive show for string;
     42' \
    "true"

test_case "Mixed manual and derived impls" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     impl show for int {
       fn show(x: int) -> string {
         "manual"
       }
     }
     derive show for bool;
     42' \
    "true"

run_build_fail_contains_from_stdin "Generic impls fail with typed error (no compiler crash)" "Generic impls are not supported yet" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show[b: show] for list[b] {
  fn show(x: list[b]) -> string {
    "ok"
  }
}
EOF

echo "-- PHASE 4.3: TRAIT METHOD CALLS --"

run_case_from_stdin "Basic trait method call on int" "42" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x: int) -> string {
    "42"
  }
}
let x = 42
puts(x.show())
EOF

run_case_from_stdin "Method call with parameter" "true" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
impl eq for int {
  fn eq(x: int, y: int) -> bool {
    x == y
  }
}
let a = 42
let b = 42
puts(a.eq(b))
EOF

run_case_from_stdin "Method call on string" "hello" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for string {
  fn show(x: string) -> string {
    x
  }
}
let s = "hello"
puts(s.show())
EOF

run_case_from_stdin "Multiple method calls in sequence" "number
number" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x: int) -> string {
    "number"
  }
}
let x = 1
let y = 2
puts(x.show())
puts(y.show())
EOF

run_case_from_stdin "Method call result used in expression" "42" << 'EOF'
trait double[a] {
  fn double(x: a) -> int
}
impl double for int {
  fn double(x: int) -> int {
    x + x
  }
}
let n = 21
let result = n.double()
puts(result)
EOF

echo "-- BUILTIN TRAITS --"

run_build_ok_from_stdin "Builtin show trait exists" << 'EOF'
impl show for int {
  fn show(x: int) -> string {
    "test"
  }
}
42
EOF

run_build_ok_from_stdin "Builtin eq trait exists" << 'EOF'
impl eq for int {
  fn eq(x: int, y: int) -> bool {
    true
  }
}
42
EOF

run_build_ok_from_stdin "Builtin ord trait exists" << 'EOF'
enum ordering { less equal greater }
impl ord for int {
  fn compare(x: int, y: int) -> ordering {
    ordering.equal
  }
}
42
EOF

echo "-- BUILTIN TRAIT IMPLS FOR PRIMITIVES --"

run_case_from_stdin "int implements show (builtin)" "42" << 'EOF'
let x = 42
let s = x.show()
puts(s)
EOF

run_case_from_stdin "int implements eq (builtin)" "true" << 'EOF'
let a = 42
let b = 42
let result = a.eq(b)
puts(result)
EOF

run_case_from_stdin "string implements show (builtin)" "hello" << 'EOF'
let s = "hello"
puts(s.show())
EOF

run_case_from_stdin "bool implements show (builtin)" "true" << 'EOF'
let b = true
puts(b.show())
EOF

echo ""
echo "-- TRAIT SOLVER --"

run_case_from_stdin "Trait solver: int implements show" "42" << 'EOF'
let check = fn[a: show](x: a) -> string {
  x.show()
}
let result = check(42)
puts(result)
EOF

run_build_fail_contains_from_stdin "Trait solver: array lacks show (should fail typecheck)" "does not implement trait" << 'EOF'
let check = fn[a: show](x: a) -> string {
  x.show()
}
let arr = [1, 2, 3]
let result = check(arr)
puts(result)
EOF

run_case_from_stdin "Trait solver: multiple constraints work" "42" << 'EOF'
let show_if_equal = fn[a: show + eq](x: a, y: a) -> string {
  if (x.eq(y)) {
    x.show()
  } else {
    "different"
  }
}
let result = show_if_equal(42, 42)
puts(result)
EOF

run_case_from_stdin "Trait solver: multiple constraints with different values" "different" << 'EOF'
let show_if_equal = fn[a: show + eq](x: a, y: a) -> string {
  if (x.eq(y)) {
    x.show()
  } else {
    "different"
  }
}
let result = show_if_equal(42, 99)
puts(result)
EOF

run_build_fail_contains_from_stdin "Supertrait: impl ord requires eq for same type" "supertrait" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
trait ord[a]: eq {
  fn compare(x: a, y: a) -> int
}
type point = { x: int }
impl ord for point {
  fn compare(x: point, y: point) -> int {
    0
  }
}
let p: point = { x: 1 }
puts(p.compare(p))
EOF

run_case_from_stdin "Supertrait: methods from supertrait available through ord constraint" "true" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
trait ord[a]: eq {
  fn compare(x: a, y: a) -> int
}
type point = { x: int }
impl eq for point {
  fn eq(x: point, y: point) -> bool {
    x.x == y.x
  }
}
impl ord for point {
  fn compare(x: point, y: point) -> int {
    0
  }
}
let eq_via_ord = fn[a: ord](x: a, y: a) -> bool {
  x.eq(y)
}
let p1: point = { x: 1 }
let p2: point = { x: 1 }
puts(eq_via_ord(p1, p2))
EOF

run_build_fail_contains_from_stdin "Supertrait: ord constraint also requires eq transitively" "supertrait" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
trait ord[a]: eq {
  fn compare(x: a, y: a) -> int
}
type point = { x: int }
impl ord for point {
  fn compare(x: point, y: point) -> int {
    0
  }
}
let compare_self = fn[a: ord](x: a) -> int {
  x.compare(x)
}
let p: point = { x: 1 }
puts(compare_self(p))
EOF


suite_end
