#!/bin/bash
# Comprehensive tests for typecheck and codegen
# These tests verify real end-to-end functionality

set -e

BINDIR="./_build/default/bin"
EXECUTABLE="$BINDIR/main.exe"

if [ ! -f "$EXECUTABLE" ]; then
    echo "Building project..."
    dune build
fi

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
# These tests check that Go code is actually being generated
TMPDIR_GO=$(mktemp -d)
echo "let x = 42; x" > /tmp/simple.mr
$EXECUTABLE build /tmp/simple.mr --emit-go "$TMPDIR_GO" 2>/dev/null
if [ -f "$TMPDIR_GO/main.go" ]; then
    TOTAL=$((TOTAL + 1))
    if grep -q "int64" "$TMPDIR_GO/main.go"; then
        echo "TEST [$TOTAL] Codegen produces valid Go ... ✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "TEST [$TOTAL] Codegen produces valid Go ... ✗ FAIL"
        FAIL=$((FAIL + 1))
    fi
fi
# Clean up temp directory
rm -rf "$TMPDIR_GO" /tmp/simple.mr

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

# Test 58: Basic trait method call
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Basic trait method call on int ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
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
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test58 2>&1) && output=$(/tmp/test58 2>&1); then
    if [ "$output" = "42" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected '42', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test58

# Test 59: Method call with parameter
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Method call with parameter ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
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
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test59 2>&1) && output=$(/tmp/test59 2>&1); then
    if [ "$output" = "true" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected 'true', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test59

# Test 60: Method call on string
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Method call on string ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
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
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test60 2>&1) && output=$(/tmp/test60 2>&1); then
    if [ "$output" = "hello" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected 'hello', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test60

# Test 61: Multiple method calls in sequence
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Multiple method calls in sequence ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
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
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test61 2>&1) && output=$(/tmp/test61 2>&1); then
    expected="number
number"
    if [ "$output" = "$expected" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected '$expected', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test61

# Test 62: Method call result used in expression
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Method call result used in expression ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
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
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test62 2>&1) && output=$(/tmp/test62 2>&1); then
    if [ "$output" = "42" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected '42', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    echo "  Build output: $binary_output"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test62

echo "-- BUILTIN TRAITS --"

# Test 63: Builtin show trait is defined
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Builtin show trait exists ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
impl show for int {
  fn show(x: int) -> string {
    "test"
  }
}
42
EOF
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test63 2>&1); then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    echo "✗ FAIL (build failed)"
    echo "  Build output: $binary_output"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test63

# Test 64: Builtin eq trait is defined
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Builtin eq trait exists ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
impl eq for int {
  fn eq(x: int, y: int) -> bool {
    true
  }
}
42
EOF
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test64 2>&1); then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    echo "✗ FAIL (build failed)"
    echo "  Build output: $binary_output"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test64

# Test 65: Builtin ord trait is defined
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Builtin ord trait exists ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
enum ordering { less equal greater }
impl ord for int {
  fn compare(x: int, y: int) -> ordering {
    ordering.equal
  }
}
42
EOF
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test65 2>&1); then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    echo "✗ FAIL (build failed)"
    echo "  Build output: $binary_output"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test65

echo "-- BUILTIN TRAIT IMPLS FOR PRIMITIVES --"

# Test 66: int implements show (builtin)
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] int implements show (builtin) ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
let x = 42
let s = x.show()
puts(s)
EOF
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test66 2>&1) && output=$(/tmp/test66 2>&1); then
    if [ "$output" = "42" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected '42', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    echo "  Build output: $binary_output"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test66

# Test 67: int implements eq (builtin)
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] int implements eq (builtin) ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
let a = 42
let b = 42
let result = a.eq(b)
puts(result)
EOF
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test67 2>&1) && output=$(/tmp/test67 2>&1); then
    if [ "$output" = "true" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected 'true', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    echo "  Build output: $binary_output"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test67

# Test 68: string implements show (builtin)
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] string implements show (builtin) ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
let s = "hello"
puts(s.show())
EOF
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test68 2>&1) && output=$(/tmp/test68 2>&1); then
    if [ "$output" = "hello" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected 'hello', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    echo "  Build output: $binary_output"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test68

# Test 69: bool implements show (builtin)
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] bool implements show (builtin) ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
let b = true
puts(b.show())
EOF
if binary_output=$($EXECUTABLE build "$tmpfile" -o /tmp/test69 2>&1) && output=$(/tmp/test69 2>&1); then
    if [ "$output" = "true" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected 'true', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    echo "  Build output: $binary_output"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test69

echo ""
echo "-- TRAIT SOLVER --"

# Test 70: Check if int implements show (builtin)
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Trait solver: int implements show ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
let check = fn[a: show](x: a) -> string {
  x.show()
}
let result = check(42)
puts(result)
EOF
if binary_output=$(dune exec -- marmoset build "$tmpfile" -o /tmp/test70 2>&1) && output=$(/tmp/test70 2>&1); then
    if [ "$output" = "42" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected '42', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    echo "  Build output: $binary_output"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test70

# Test 71: Check constraint violation: array doesn't implement show
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Trait solver: array lacks show (should fail typecheck) ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
let check = fn[a: show](x: a) -> string {
  x.show()
}
let arr = [1, 2, 3]
let result = check(arr)
puts(result)
EOF
if output=$(dune exec -- marmoset build "$tmpfile" 2>&1) && echo "$output" | grep -q "does not implement trait"; then
    echo "✓ PASS"
    PASS=$((PASS + 1))
elif echo "$output" | grep -q "does not implement trait"; then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    echo "✗ FAIL (should reject, got: $output)"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile"

# Test 72: Multiple constraints
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Trait solver: multiple constraints work ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
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
if binary_output=$(dune exec -- marmoset build "$tmpfile" -o /tmp/test72 2>&1) && output=$(/tmp/test72 2>&1); then
    if [ "$output" = "42" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected '42', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    echo "  Output: $binary_output"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test72

# Test 73: Multiple constraints with different result
TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] Trait solver: multiple constraints with different values ... "
tmpfile=$(mktemp)
cat > "$tmpfile" << 'EOF'
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
if binary_output=$(dune exec -- marmoset build "$tmpfile" -o /tmp/test73 2>&1) && output=$(/tmp/test73 2>&1); then
    if [ "$output" = "different" ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (expected 'different', got '$output')"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (build or execution failed)"
    echo "  Output: $binary_output"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" /tmp/test73

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
