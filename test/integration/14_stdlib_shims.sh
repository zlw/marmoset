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

DATA_DIR=$(mktemp -d "$REPO_ROOT/.marmoset/build/stdlib_shims_data.XXXXXX")
ROUNDTRIP_PATH="$DATA_DIR/roundtrip.txt"
MISSING_PATH="$DATA_DIR/missing.txt"
EXISTING_PATH="$DATA_DIR/existing.txt"
printf "opened" > "$EXISTING_PATH"

run_source_expect_output \
    "std.file maps bytes and filesystem errors" \
    $'roundtrip\nread:not-found\nread:is-directory\nwrite:is-directory' <<EOF
import std.bytes
import std.file.read
import std.file.write
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

match write("$ROUNDTRIP_PATH", bytes.from_str("roundtrip")) {
  case Result.Success(_): match read("$ROUNDTRIP_PATH") {
    case Result.Success(payload): puts(bytes.to_str_lossy(payload))
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

match write("$DATA_DIR", bytes.from_str("x")) {
  case Result.Success(_): puts("write-dir:unexpected-success")
  case Result.Failure(err): puts(write_error_label(err))
}
EOF

run_source_expect_output \
    "std.file scopes open handles through managed callbacks" \
    $'scoped:opened\nopen:not-found\nleaked:read:already-closed' <<EOF
import std.bytes
import std.file
import std.file.File
import std.file.FileReadError
import std.file.FileOpenError
import std.file.FileScopeError

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

fn scope_error_label(err: FileScopeError) -> Str = match err {
  case FileScopeError.Open(open_err): open_error_label(open_err)
  case FileScopeError.Close(_): "close"
}

match file.open("$EXISTING_PATH", (handle: File) => match file.read_all(handle) {
  case Result.Success(payload): "scoped:" + bytes.to_str_lossy(payload)
  case Result.Failure(err): read_error_label(err)
}) {
  case Result.Success(label): puts(label)
  case Result.Failure(err): puts(scope_error_label(err))
}

match file.open("$MISSING_PATH", (handle: File) => bytes.length(bytes.from_str("unused"))) {
  case Result.Success(_): puts("open:unexpected-success")
  case Result.Failure(err): puts(scope_error_label(err))
}

match file.open("$EXISTING_PATH", (handle: File) => handle) {
  case Result.Success(leaked): {
    match file.read_all(leaked) {
      case Result.Success(_): puts("leaked:unexpected-success")
      case Result.Failure(err): puts("leaked:" + read_error_label(err))
    }
  }
  case Result.Failure(err): puts(scope_error_label(err))
}
EOF

run_source_expect_build_failure \
    "std.file File cannot be constructed by importers" \
    "cannot be constructed" <<'EOF'
import std.file.File

let file = File(0)
puts(0)
EOF

run_source_expect_build_failure \
    "std.file close is private shim plumbing" \
    "does not export 'close'" <<'EOF'
import std.file.close

puts(0)
EOF

rm -rf "$DATA_DIR"

suite_end
