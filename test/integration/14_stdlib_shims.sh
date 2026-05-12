#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "STDLIB SHIM TESTS"

run_source_expect_output() {
    local name="$1"
    local expected_output="$2"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local root binpath build_output output
    root=$(mktemp -d marmoset_stdlib_shims.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_stdlib_bin.XXXXXX")
    rm -f "$binpath"
    printf "%s\n" "$source" > "$root/main.mr"

    if build_output=$($EXECUTABLE build "$root/main.mr" -o "$binpath" 2>&1) && output=$("$binpath" 2>&1); then
        if [ "$output" = "$expected_output" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (expected '$expected_output', got '$output')"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build or execution failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
    rm -rf "$root"
}

run_source_with_stdin_expect_output() {
    local name="$1"
    local stdin="$2"
    local expected_output="$3"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local root binpath build_output output
    root=$(mktemp -d marmoset_stdlib_shims.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_stdlib_bin.XXXXXX")
    rm -f "$binpath"
    printf "%s\n" "$source" > "$root/main.mr"

    if build_output=$($EXECUTABLE build "$root/main.mr" -o "$binpath" 2>&1) && output=$(printf "%s" "$stdin" | "$binpath" 2>&1); then
        if [ "$output" = "$expected_output" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (expected '$expected_output', got '$output')"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build or execution failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
    rm -rf "$root"
}

run_source_expect_build_failure() {
    local name="$1"
    local expected_fragment="$2"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local root binpath build_output
    root=$(mktemp -d marmoset_stdlib_shims.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_stdlib_bin.XXXXXX")
    rm -f "$binpath"
    printf "%s\n" "$source" > "$root/main.mr"

    if build_output=$($EXECUTABLE build "$root/main.mr" -o "$binpath" 2>&1); then
        echo "✗ FAIL (expected build failure but build succeeded)"
        FAIL=$((FAIL + 1))
    elif echo "$build_output" | grep -qF "$expected_fragment"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (missing expected diagnostic '$expected_fragment')"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
    rm -rf "$root"
}

run_source_emit_go_expect_protocol_shape() {
    local name="$1"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local root outdir binpath build_output failures
    root=$(mktemp -d marmoset_stdlib_shims.XXXXXX)
    outdir=$(mktemp -d marmoset_stdlib_go.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_stdlib_bin.XXXXXX")
    rm -f "$binpath"
    printf "%s\n" "$source" > "$root/main.mr"

    failures=0
    if build_output=$($EXECUTABLE build "$root/main.mr" --emit-go "$outdir" -o "$binpath" 2>&1); then
        grep -qF 'func Read(reader ioapi.Stdin)' "$outdir/shims/std/io/io.go" || failures=$((failures + 1))
        grep -qF 'func Write(writer ioapi.Stdout, value string)' "$outdir/shims/std/io/io.go" || failures=$((failures + 1))
        grep -qF 'func Flush(writer ioapi.Stdout)' "$outdir/shims/std/io/io.go" || failures=$((failures + 1))
        grep -qF 'func Write(writer errapi.Stderr, value string)' "$outdir/shims/std/io/err/err.go" || failures=$((failures + 1))
        grep -qF 'func Flush(writer errapi.Stderr)' "$outdir/shims/std/io/err/err.go" || failures=$((failures + 1))
        if rg -q 'std__file__Path|flush_path|FlushPath' "$outdir"; then
            failures=$((failures + 1))
        fi

        if [ "$failures" -eq 0 ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (emitted Go still has phantom protocol plumbing)"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
    rm -rf "$root" "$outdir"
}

DATA_DIR=$(mktemp -d "$REPO_ROOT/.marmoset/build/stdlib_shims_data.XXXXXX")
ROUNDTRIP_PATH="$DATA_DIR/roundtrip.txt"
MISSING_PATH="$DATA_DIR/missing.txt"
EXISTING_PATH="$DATA_DIR/existing.txt"
printf "opened" > "$EXISTING_PATH"

run_source_expect_output \
    "std.file maps text, bytes and filesystem errors" \
    $'roundtrip\nstr:hello\nread:not-found\nread:is-directory\nwrite:is-directory' <<EOF
import std.bytes
import std.file
import std.file.read
import std.file.write
import std.file.read_bytes
import std.file.write_bytes
import std.file.FileReadError
import std.file.FileWriteError

fn read_error_label(err: FileReadError) -> Str = match err {
  case FileReadError.NotFound: "read:not-found"
  case FileReadError.PermissionDenied: "read:permission-denied"
  case FileReadError.IsDirectory: "read:is-directory"
  case FileReadError.AlreadyClosed: "read:already-closed"
  case FileReadError.Other(message): "read:other:" + message
}

fn write_error_label(err: FileWriteError) -> Str = match err {
  case FileWriteError.NotFound: "write:not-found"
  case FileWriteError.PermissionDenied: "write:permission-denied"
  case FileWriteError.IsDirectory: "write:is-directory"
  case FileWriteError.Other(message): "write:other:" + message
}

match write_bytes("$ROUNDTRIP_PATH", bytes.from_str("roundtrip")) {
  case Result.Success(_): match read_bytes("$ROUNDTRIP_PATH") {
    case Result.Success(payload): puts(bytes.to_str_lossy(payload))
    case Result.Failure(err): puts(read_error_label(err))
  }
  case Result.Failure(err): puts(write_error_label(err))
}

match write("$ROUNDTRIP_PATH", "hello") {
  case Result.Success(_): match read("$ROUNDTRIP_PATH") {
    case Result.Success(payload): puts("str:" + payload)
    case Result.Failure(err): puts(read_error_label(err))
  }
  case Result.Failure(err): puts(write_error_label(err))
}

match read("$MISSING_PATH") {
  case Result.Success(_): puts("read:unexpected-success")
  case Result.Failure(err): puts(read_error_label(err))
}

match read("$DATA_DIR") {
  case Result.Success(_): puts("read-dir:unexpected-success")
  case Result.Failure(err): puts(read_error_label(err))
}

match write_bytes("$DATA_DIR", bytes.from_str("x")) {
  case Result.Success(_): puts("write-dir:unexpected-success")
  case Result.Failure(err): puts(write_error_label(err))
}
EOF

run_source_expect_output \
    "std.file scopes handles and flattens callback errors" \
    $'scoped:opened\nprotocol:opened\nopen:not-found\nuse:read:is-directory\nleaked:read:already-closed' <<EOF
import std.bytes
import std.file
import std.io
import std.file.FileReadError
import std.file.FileOpenError
import std.file.FileCloseError
import std.file.FileUseError

fn read_error_label(err: FileReadError) -> Str = match err {
  case FileReadError.NotFound: "read:not-found"
  case FileReadError.PermissionDenied: "read:permission-denied"
  case FileReadError.IsDirectory: "read:is-directory"
  case FileReadError.AlreadyClosed: "read:already-closed"
  case FileReadError.Other(message): "read:other:" + message
}

fn open_error_label(err: FileOpenError) -> Str = match err {
  case FileOpenError.NotFound: "open:not-found"
  case FileOpenError.PermissionDenied: "open:permission-denied"
  case FileOpenError.IsDirectory: "open:is-directory"
  case FileOpenError.Other(message): "open:other:" + message
}

fn close_error_label(err: FileCloseError) -> Str = match err {
  case FileCloseError.AlreadyClosed: "close:already-closed"
  case FileCloseError.Other(message): "close:other:" + message
}

fn use_error_label(err: FileUseError[FileReadError]) -> Str = match err {
  case FileUseError.Open(open_err): open_error_label(open_err)
  case FileUseError.Use(read_err): "use:" + read_error_label(read_err)
  case FileUseError.Close(close_err): close_error_label(close_err)
  case FileUseError.UseAndClose(read_err, close_err): "use:" + read_error_label(read_err) + "+" + close_error_label(close_err)
}

match file.open("$EXISTING_PATH", (handle) => file.read_all(handle)) {
  case Result.Success(payload): puts("scoped:" + bytes.to_str_lossy(payload))
  case Result.Failure(err): puts(use_error_label(err))
}

match file.open("$EXISTING_PATH", (handle) => io.Read.read(handle)) {
  case Result.Success(payload): puts("protocol:" + payload)
  case Result.Failure(err): puts(use_error_label(err))
}

match file.open("$MISSING_PATH", (handle) => file.read_all(handle)) {
  case Result.Success(_): puts("open:unexpected-success")
  case Result.Failure(err): puts(use_error_label(err))
}

match file.open("$EXISTING_PATH", (_handle) => file.read("$DATA_DIR")) {
  case Result.Success(_): puts("use:unexpected-success")
  case Result.Failure(err): puts(use_error_label(err))
}

match file.open("$EXISTING_PATH", (handle) => if (true) {
  Result.Success(handle)
} else {
  Result.Failure(FileReadError.NotFound)
}) {
  case Result.Success(leaked): {
    match file.read_all(leaked) {
      case Result.Success(_): puts("leaked:unexpected-success")
      case Result.Failure(err): puts("leaked:" + read_error_label(err))
    }
  }
  case Result.Failure(err): puts(use_error_label(err))
}
EOF

run_source_expect_build_failure \
    "std.file File type is private" \
    "does not export 'File'" <<'EOF'
import std.file.File

puts(0)
EOF

run_source_expect_build_failure \
    "std.file close is private shim plumbing" \
    "does not export 'close'" <<'EOF'
import std.file.close

puts(0)
EOF

run_source_expect_build_failure \
    "std.file open_handle is private shim plumbing" \
    "does not export 'open_handle'" <<'EOF'
import std.file.open_handle

puts(0)
EOF

run_source_expect_build_failure \
    "std.file old read_str helper is gone" \
    "does not export 'read_str'" <<'EOF'
import std.file.read_str

puts(0)
EOF

run_source_expect_build_failure \
    "std.file old write_str helper is gone" \
    "does not export 'write_str'" <<'EOF'
import std.file.write_str

puts(0)
EOF

run_source_emit_go_expect_protocol_shape \
    "std.io receivers reach shims and std.file path helpers stay direct" <<EOF
import std.file
import std.io
import std.io.err

let _ = io.Read.read(io.stdin())
let _ = io.Write.write(io.stdout(), "out")
let _ = io.Write.flush(io.stdout())
let _ = io.Write.write(err.stderr(), "err")
let _ = io.Write.flush(err.stderr())
let _ = file.write("$ROUNDTRIP_PATH", "path")
let _ = file.read("$ROUNDTRIP_PATH")
puts(0)
EOF

run_source_expect_output \
    "std.io writes, flushes, prints and exposes stderr helpers" \
    $'value:42\nerr:bad\ntrait:ok\ntrait-err:nope' <<'EOF'
import std.io
import std.io.err

let _ = io.write("value:")
let _ = io.print(42)
let _ = err.write("err:")
let _ = err.print("bad")
let stdout = io.stdout()
let stderr = err.stderr()
let _ = io.Write.write(stdout, "trait:")
let _ = io.Write.print(stdout, "ok")
let _ = io.Write.flush(stdout)
let _ = io.Write.write(stderr, "trait-err:")
let _ = io.Write.print(stderr, "nope")
let _ = io.Write.flush(stderr)
EOF

run_source_expect_build_failure \
    "std.io Stdin token is opaque" \
    "No inherent method 'Stdin'" <<'EOF'
import std.io.Stdin

let input = Stdin.Stdin
puts(0)
EOF

run_source_expect_build_failure \
    "std.io stderr token is opaque" \
    "No inherent method 'Stderr'" <<'EOF'
import std.io.err.Stderr

let output = Stderr.Stderr
puts(0)
EOF

run_source_with_stdin_expect_output \
    "std.io reads stdin lines and EOF" \
    $'alpha\nbeta\n' \
    $'line:alpha\nline:beta\neof' <<'EOF'
import std.io
import std.io.Error

fn read_error_label(err: Error) -> Str = match err {
  case Error.EndOfFile: "eof"
  case Error.BrokenPipe: "broken-pipe"
  case Error.Interrupted: "interrupted"
  case Error.Other(message): "other:" + message
}

match io.Read.read(io.stdin()) {
  case Result.Success(line): io.print("line:" + line)
  case Result.Failure(err): io.print(read_error_label(err))
}

match io.read() {
  case Result.Success(line): io.print("line:" + line)
  case Result.Failure(err): io.print(read_error_label(err))
}

match io.read() {
  case Result.Success(line): io.print("line:" + line)
  case Result.Failure(err): io.print(read_error_label(err))
}
EOF

run_source_expect_build_failure \
    "std.io old println helper is gone" \
    "does not export 'println'" <<'EOF'
import std.io.println

puts(0)
EOF

run_source_expect_build_failure \
    "std.io old read_line helper is gone" \
    "does not export 'read_line'" <<'EOF'
import std.io.read_line

puts(0)
EOF

run_source_expect_build_failure \
    "std.io old callback helper is gone" \
    "does not export 'map_line'" <<'EOF'
import std.io.map_line

puts(0)
EOF

run_source_expect_build_failure \
    "std.io old effect callback helper is gone" \
    "does not export 'with_line'" <<'EOF'
import std.io.with_line

puts(0)
EOF

rm -rf "$DATA_DIR"

suite_end
