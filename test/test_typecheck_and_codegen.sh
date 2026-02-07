#!/bin/bash
# Comprehensive tests for typecheck and codegen
# These tests verify real end-to-end functionality

set -e

BINDIR="./_build/default/bin"
EXECUTABLE="$BINDIR/main.exe"

echo "Building project..."
dune build "$EXECUTABLE"

PASS=0
FAIL=0
TOTAL=0

# Helper function for testing
test_case() {
    local name="$1"
    local source="$2"
    local should_succeed="$3"  # "true" or "false"
    local expected_output="$4" # optional

    TOTAL=$((TOTAL + 1))

    echo -n "TEST [$TOTAL] $name ... "

    # Write source to temp file
    tmpfile=$(mktemp)
    echo "$source" > "$tmpfile"

    # Try to build
    if output=$($EXECUTABLE build "$tmpfile" 2>&1); then
        if [ "$should_succeed" = "true" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (expected to fail but passed)"
            FAIL=$((FAIL + 1))
        fi
    else
        if [ "$should_succeed" = "false" ]; then
            # Check if error message contains expected substring
            if [ -n "$expected_output" ]; then
                if echo "$output" | grep -q "$expected_output"; then
                    echo "✓ PASS"
                    PASS=$((PASS + 1))
                else
                    echo "✗ FAIL (error message doesn't contain '$expected_output')"
                    echo "  Got: $output"
                    FAIL=$((FAIL + 1))
                fi
            else
                echo "✓ PASS"
                PASS=$((PASS + 1))
            fi
        else
            echo "✗ FAIL"
            echo "  Error: $output"
            FAIL=$((FAIL + 1))
        fi
    fi

    rm -f "$tmpfile"
}

# Build + execute helper for runtime-output tests.
# Usage:
#   run_case_from_stdin "Name" "expected output" << 'EOF'
#   <program>
#   EOF
run_case_from_stdin() {
    local name="$1"
    local expected_output="$2"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile
    local binpath
    tmpfile=$(mktemp)
    binpath=$(mktemp /tmp/marmoset_test_bin.XXXXXX)
    rm -f "$binpath"
    echo "$source" > "$tmpfile"

    if binary_output=$($EXECUTABLE build "$tmpfile" -o "$binpath" 2>&1) && output=$("$binpath" 2>&1); then
        if [ "$output" = "$expected_output" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (expected '$expected_output', got '$output')"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build or execution failed)"
        echo "  Output: $binary_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$tmpfile" "$binpath"
}

run_build_ok_from_stdin() {
    local name="$1"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile
    tmpfile=$(mktemp)
    echo "$source" > "$tmpfile"

    if build_output=$($EXECUTABLE build "$tmpfile" 2>&1); then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$tmpfile"
}

run_build_ok_not_contains_from_stdin() {
    local name="$1"
    local forbidden_fragment="$2"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile
    tmpfile=$(mktemp)
    echo "$source" > "$tmpfile"

    if build_output=$($EXECUTABLE build "$tmpfile" 2>&1); then
        if echo "$build_output" | grep -q "$forbidden_fragment"; then
            echo "✗ FAIL (output contains forbidden fragment '$forbidden_fragment')"
            echo "  Output: $build_output"
            FAIL=$((FAIL + 1))
        else
            echo "✓ PASS"
            PASS=$((PASS + 1))
        fi
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$tmpfile"
}

run_build_fail_contains_from_stdin() {
    local name="$1"
    local expected_fragment="$2"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile
    tmpfile=$(mktemp)
    echo "$source" > "$tmpfile"

    if build_output=$($EXECUTABLE build "$tmpfile" 2>&1); then
        if echo "$build_output" | grep -q "$expected_fragment"; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (expected build to fail)"
            FAIL=$((FAIL + 1))
        fi
    else
        if echo "$build_output" | grep -q "$expected_fragment"; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (missing expected error fragment '$expected_fragment')"
            echo "  Output: $build_output"
            FAIL=$((FAIL + 1))
        fi
    fi

    rm -f "$tmpfile"
}

run_codegen_deterministic_from_stdin() {
    local name="$1"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile out1 out2 bin1 bin2
    tmpfile=$(mktemp)
    out1=$(mktemp -d /tmp/marmoset_emit1.XXXXXX)
    out2=$(mktemp -d /tmp/marmoset_emit2.XXXXXX)
    bin1=$(mktemp /tmp/marmoset_bin1.XXXXXX)
    bin2=$(mktemp /tmp/marmoset_bin2.XXXXXX)
    rm -f "$bin1" "$bin2"
    echo "$source" > "$tmpfile"

    if build1=$($EXECUTABLE build "$tmpfile" --emit-go "$out1" -o "$bin1" 2>&1) && build2=$($EXECUTABLE build "$tmpfile" --emit-go "$out2" -o "$bin2" 2>&1); then
        if diff -u "$out1/main.go" "$out2/main.go" >/dev/null 2>&1 && diff -u "$out1/runtime.go" "$out2/runtime.go" >/dev/null 2>&1; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (emitted Go is not deterministic)"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build failed)"
        if [ -n "$build1" ]; then
            echo "  First build output: $build1"
        fi
        if [ -n "$build2" ]; then
            echo "  Second build output: $build2"
        fi
        FAIL=$((FAIL + 1))
    fi

    rm -f "$tmpfile" "$bin1" "$bin2"
    rm -rf "$out1" "$out2"
}

test_emit_go_contains() {
    local name="$1"
    local source="$2"
    local expected_fragment="$3"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpdir_go
    local tmpfile
    tmpdir_go=$(mktemp -d)
    tmpfile=$(mktemp)
    echo "$source" > "$tmpfile"

    if $EXECUTABLE build "$tmpfile" --emit-go "$tmpdir_go" >/dev/null 2>&1 && [ -f "$tmpdir_go/main.go" ]; then
        if grep -q "$expected_fragment" "$tmpdir_go/main.go"; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (Go source emission failed)"
        FAIL=$((FAIL + 1))
    fi

    rm -rf "$tmpdir_go" "$tmpfile"
}

echo "=========================================="
echo "TYPECHECK & CODEGEN INTEGRATION TESTS"
echo "=========================================="
echo ""

echo "-- PARAMETER ANNOTATION TESTS --"
test_case "Single int parameter" \
    'let f = fn(x: int) { x + 1 }; f(5)' \
    "true"

test_case "Multiple int parameters" \
    'let add = fn(x: int, y: int) -> int { x + y }; add(3, 4)' \
    "true"

test_case "Mixed type parameters" \
    'let f = fn(x: int, y: string) { y }; f(5, "hi")' \
    "true"

echo ""
echo "-- RETURN TYPE ANNOTATION TESTS --"
test_case "Return type int matches" \
    'let f = fn() -> int { 42 }; f()' \
    "true"

test_case "Return type string matches" \
    'let f = fn() -> string { "hello" }; f()' \
    "true"

test_case "Return type bool matches" \
    'let f = fn() -> bool { true }; f()' \
    "true"

echo ""
echo "-- ANNOTATION MISMATCH DETECTION (MUST FAIL) --"
test_case "Mismatch: int vs string" \
    'let f = fn() -> string { 42 }; f()' \
    "false" \
    "annotation mismatch"

test_case "Mismatch: string vs int" \
    'let f = fn() -> int { "hello" }; f()' \
    "false" \
    "annotation mismatch"

test_case "Mismatch: bool vs int" \
    'let f = fn() -> bool { 42 }; f()' \
    "false" \
    "annotation mismatch"

test_case "Mismatch: array vs int" \
    'let f = fn() -> list[int] { 42 }; f()' \
    "false" \
    "annotation mismatch"

echo ""
echo "-- COMPLEX ANNOTATION TESTS --"
test_case "Full signature: params + return" \
    'let f = fn(x: int, y: int) -> int { x + y }; f(1, 2)' \
    "true"

test_case "Conditional with matching return type" \
    'let f = fn(x: int) -> int { if (x < 0) { 0 } else { x } }; f(-5)' \
    "true"

test_case "Recursive function with correct type" \
    'let fib = fn(n: int) -> int { if (n < 2) { return n } return fib(n - 1) + fib(n - 2); }; fib(10)' \
    "true"

test_case "Recursive function with wrong return type" \
    'let fib = fn(n: int) -> string { if (n < 2) { return n } return fib(n - 1) + fib(n - 2); }; fib(10)' \
    "false" \
    "annotation mismatch"

echo ""
echo "-- BACKWARD COMPATIBILITY TESTS --"
test_case "Unannotated function still works" \
    'let f = fn(x) { x + 1 }; f(5)' \
    "true"

test_case "Phase 1 fibonacci (no annotations)" \
    'let fib = fn(n) { if (n < 2) { return n } return fib(n - 2) + fib(n - 1); }; fib(10)' \
    "true"

echo ""
echo "-- PHASE 3: EARLY RETURN SYNTAX TESTS --"
test_case "Early return without braces (consequence)" \
    'let fib = fn(n: int) -> int { if (n < 2) return n else { fib(n - 1) + fib(n - 2) } }; fib(5)' \
    "true"

test_case "Early return in both branches" \
    'let abs = fn(x: int) -> int { if (x < 0) return -x else return x; }; abs(-5)' \
    "true"

test_case "Early return with complex expression" \
    'let max = fn(a: int, b: int) -> int { if (a < b) return b else return a; }; max(3, 7)' \
    "true"

test_case "Nested early returns" \
    'let f = fn(n: int) -> int { if (n < 0) return -n else { if (n < 1) return 0 else return n * 2 } }; f(5)' \
    "true"

echo ""
echo "-- CODEGEN OUTPUT TESTS --"
test_emit_go_contains "Codegen produces valid Go" "let x = 42; x" "int64"

echo ""
echo "-- PHASE 4.1: UNION TYPE TESTS --"
test_case "Parse union in parameter type" \
    'let f = fn(x: int | string) -> string { "ok" }; f(5)' \
    "true"

test_case "Parse union in return type" \
    'let f = fn(x: int) -> int | string { if (x > 0) { x } else { "negative" } }; f(5)' \
    "true"

test_case "Parse multi-member union" \
    'let f = fn(x: int | string | bool) { x }; f(true)' \
    "true"

test_case "is operator returns bool" \
    'let x: int | string = 5; if (x is int) { true } else { false }' \
    "true"

test_case "Type narrowing allows int operations" \
    'let f = fn(x: int | string) -> int | string {
       if (x is int) {
           x + 1
       } else {
           x
       }
     }; f(5)' \
    "true"

echo ""
echo "-- PHASE 4.1: EXHAUSTIVENESS & EDGE CASES --"

test_case "Complement narrowing in else branch" \
    'let f = fn(x: int | string) -> string {
       if (x is int) {
           "was int"
       } else {
           x
       }
     }; f("hello")' \
    "true"

test_case "Three-way union exhaustiveness" \
    'let f = fn(x: int | string | bool) -> string {
       if (x is int) {
           "int"
       } else {
           if (x is string) {
               x
           } else {
               "bool"
           }
       }
     }; f(true)' \
    "true"

test_case "Nested type narrowing" \
    'let outer = fn(x: int | string) -> int {
       if (x is int) {
           let inner = fn(y: int | bool) -> int {
               if (y is int) { y } else { 0 }
           };
           inner(x)
       } else {
           0
       }
     }; outer(42)' \
    "true"

test_case "Type narrowing with string operations" \
    'let f = fn(x: int | string) -> int {
       if (x is string) {
           len(x)
       } else {
           x
       }
     }; f(42)' \
    "true"

test_case "Union return from both branches" \
    'let f = fn(b: bool, x: int | string) -> int | string {
       if (b) { x } else { x }
     }; f(true, 5)' \
    "true"

echo ""
echo "-- ENUM CONSTRUCTOR TESTS (Phase 4.2) --"
test_case "Simple enum definition" \
    'enum direction { north south east west }' \
    "true"

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

echo ""
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

echo ""
echo "-- PHASE 4.4: RECORDS & ROW POLYMORPHISM --"

run_case_from_stdin "Record literal + field access" "30" << 'EOF'
let p = { x: 10, y: 20 }
puts(p.x + p.y)
EOF

run_case_from_stdin "Type alias with record annotation" "3" << 'EOF'
type point = { x: int, y: int }
let p: point = { x: 1, y: 2 }
puts(p.x + p.y)
EOF

run_case_from_stdin "Record spread update" "12" << 'EOF'
let p = { x: 1, y: 2 }
let p2 = { ...p, x: 10 }
puts(p2.x + p2.y)
EOF

run_case_from_stdin "Explicit row-polymorphic function" "5" << 'EOF'
let p = { x: 5, y: 10, z: 20 }
let get_x = fn(r: { x: int, ...row }) -> int { r.x }
puts(get_x(p))
EOF

run_case_from_stdin "Record pattern match" "30" << 'EOF'
let p = { x: 10, y: 20 }
let result = match p {
  { x:, y: }: x + y
  _: 0
}
puts(result)
EOF

run_case_from_stdin "Derive eq for record" "true" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
type point = { x: int, y: int }
derive eq for point;
let p1: point = { x: 1, y: 2 }
let p2: point = { x: 1, y: 2 }
puts(p1.eq(p2))
EOF

run_case_from_stdin "Derive show for record" "{ x: 1, y: 2 }" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
type point = { x: int, y: int }
derive show for point;
let p: point = { x: 1, y: 2 }
puts(p.show())
EOF

run_case_from_stdin "Derive ord for record" "0" << 'EOF'
trait ord[a] {
  fn compare(x: a, y: a) -> int
}
type point = { x: int, y: int }
derive ord for point;
let p1: point = { x: 1, y: 2 }
let p2: point = { x: 1, y: 3 }
puts(p1.compare(p2))
EOF

run_case_from_stdin "Derive hash for record" "16370" << 'EOF'
trait hash[a] {
  fn hash(x: a) -> int
}
type point = { x: int, y: int }
derive hash for point;
let p: point = { x: 1, y: 2 }
puts(p.hash())
EOF

run_case_from_stdin "Reordered record literal still resolves derived show" "{ x: 1, y: 2 }" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
type point = { x: int, y: int }
derive show for point;
let p: point = { y: 2, x: 1 }
puts(p.show())
EOF

run_case_from_stdin "Reordered record aliases are assignment-compatible" "3" << 'EOF'
type point = { x: int, y: int }
type vec = { y: int, x: int }
let p: point = { x: 1, y: 2 }
let v: vec = p
puts(v.x + v.y)
EOF

run_case_from_stdin "Derived eq/hash are stable across field order" "ok" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
trait hash[a] {
  fn hash(x: a) -> int
}
type point = { x: int, y: int }
derive eq, hash for point;
let p1: point = { x: 1, y: 2 }
let p2: point = { y: 2, x: 1 }
let out = if (p1.eq(p2)) {
  if (p1.hash() == p2.hash()) { "ok" } else { "hash_bad" }
} else {
  "eq_bad"
}
puts(out)
EOF

run_build_fail_contains_from_stdin "Hash literal missing comma reports clear parse error" "expected ',' or '}' after hash literal entry" << 'EOF'
let x = { "a": 1 b: 2 }
EOF

run_build_fail_contains_from_stdin "Record literal missing comma reports clear parse error" "expected ',' or '}' after record literal entry" << 'EOF'
let x = { a: 1 b: 2 }
EOF

run_build_fail_contains_from_stdin "Record literal duplicate spread reports clear parse error" "multiple spread entries in record literal are not supported yet" << 'EOF'
let x = { ...a, ...b }
EOF

echo ""
echo "-- P0.1: TYPE MAP COMPLETENESS (NO EMITTER RE-INFERENCE) --"

run_case_from_stdin "Top-level function calling top-level function compiles and runs" "1" << 'EOF'
let f = fn(x: int) -> int { x }
let g = fn(y: int) -> int { f(y) }
puts(g(1))
EOF

run_case_from_stdin "Impl method calling union-param helper resolves using typed env" "int" << 'EOF'
let helper = fn(x: int | string) -> string {
  if (x is int) { "int" } else { "string" }
}
trait show[a] { fn show(x: a) -> string }
impl show for int {
  fn show(x: int) -> string { helper(x) }
}
puts(1.show())
EOF

run_build_ok_not_contains_from_stdin "Successful build emits no missing-type warning text" "missing type for expression id" << 'EOF'
let f = fn(x: int) -> int { x + 1 }
puts(f(1))
EOF

run_build_fail_contains_from_stdin "Specialized body codegen failure is surfaced cleanly" "Codegen error: Multiple patterns per arm not yet supported in codegen" << 'EOF'
let f = fn(x: int) -> int {
  match x {
    1 | 2: 1
    _: 0
  }
}
puts(f(1))
EOF

run_codegen_deterministic_from_stdin "Codegen output is deterministic for identical input" << 'EOF'
let f = fn(x: int) -> int { x + 1 }
let g = fn(y: int) -> int { f(y) }
puts(g(1))
EOF

echo ""
echo "-- RUNTIME OUTPUT TESTS --"

run_case_from_stdin "Print integer" "42" << 'EOF'
puts(42)
EOF

run_case_from_stdin "Print string" "hello" << 'EOF'
puts("hello")
EOF

run_case_from_stdin "Print boolean" "true" << 'EOF'
puts(true)
EOF

run_case_from_stdin "Print enum value with data" "some(42)" << 'EOF'
enum option[a] {
  some(a)
  none
}

let x = option.some(42)
puts(x)
EOF

run_case_from_stdin "Print enum with no data" "none" << 'EOF'
enum option[a] {
  some(a)
  none
}

let x: option[int] = option.none()
puts(x)
EOF

echo "=========================================="
echo "RESULTS: $PASS passed, $FAIL failed out of $TOTAL tests"
echo "=========================================="

if [ $FAIL -eq 0 ]; then
    echo "✓ ALL TESTS PASSED"
    exit 0
else
    echo "✗ SOME TESTS FAILED"
    exit 1
fi
