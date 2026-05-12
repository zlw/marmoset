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

run_source_emit_go_expect_stdio_shape() {
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
        grep -qF 'type std__path__Path string' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func std__file__read_string_ret_Result_string_std__file__Error' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func std__file__write_string_string' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func std__io__Read_read_std__file__File' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func std__io__Write_write_std__file__File' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func std__io__Write_flush_std__file__File' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func std__dir__read_string' "$outdir/main.go" || failures=$((failures + 1))
        if rg -q 'std__file__Path|flush_path|FlushPath|func Stdin|func Stdout|func Stderr|std__dir__Entry_Entry_tag|std__dir__RawEntry_RawEntry_tag' "$outdir"; then
            failures=$((failures + 1))
        fi

        if [ "$failures" -eq 0 ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (emitted Go stdio/path protocol shape drifted)"
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
INVALID_UTF8_PATH="$DATA_DIR/invalid.bin"
CHILD_DIR="$DATA_DIR/child"
NONEMPTY_DIR="$DATA_DIR/nonempty"
NESTED_DIR="$DATA_DIR/nested/inner"
LISTING_DIR="$DATA_DIR/listing"
LISTING_FILE="$LISTING_DIR/entry.txt"
NONEMPTY_FILE="$NONEMPTY_DIR/child.txt"
printf "opened" > "$EXISTING_PATH"
printf '\377' > "$INVALID_UTF8_PATH"
mkdir "$NONEMPTY_DIR"
mkdir "$LISTING_DIR"
printf "child" > "$NONEMPTY_FILE"
printf "entry" > "$LISTING_FILE"

run_source_expect_output \
    "std.bytes codecs expose strict decode and encode traits" \
    $'strict:hello\ntrait:hello\nbytes:5\nlossy:hello' <<'EOF'
import std.bytes
import std.bytes.Bytes
import std.bytes.DecodeError

fn decode_error_label(err: DecodeError) -> Str = match err {
  case DecodeError.InvalidUtf8: "invalid-utf8"
}

let encoded: Bytes = bytes.Encode.encode("hello")

match bytes.to_str(encoded) {
  case Result.Success(value): puts("strict:" + value)
  case Result.Failure(err): puts(decode_error_label(err))
}

let decoded: Result[Str, DecodeError] = bytes.Decode.decode(encoded)
match decoded {
  case Result.Success(value): puts("trait:" + value)
  case Result.Failure(err): puts(decode_error_label(err))
}

let raw: Bytes = encoded
puts("bytes:" + Show.show(bytes.length(raw)))
puts("lossy:" + bytes.to_str_lossy(raw))
EOF

run_source_expect_output \
    "std.path manipulates pure path values" \
    $'base:example.txt\ndir:/tmp/marmoset\next:.txt\njoin:/tmp/marmoset/child\nclean:/tmp/b\nabs:true\nrel:true' <<'EOF'
import std.path
import std.path.Path

let root: Path = path.from_str("/tmp/marmoset/example.txt")
let joined: Path = path.join(path.dirname(root), "child")

puts("base:" + path.basename(root))
puts("dir:" + path.to_str(path.dirname(root)))
puts("ext:" + path.extname(root))
puts("join:" + path.to_str(joined))
puts("clean:" + path.to_str(path.clean(path.from_str("/tmp/a/../b"))))
puts("abs:" + Show.show(path.absolute?(root)))
puts("rel:" + Show.show(path.relative?(path.from_str("notes/today.md"))))
EOF

run_source_expect_output \
    "std.file uses traits for text, bytes and filesystem errors" \
    $'bytes:9\ntext:hello\nappend:hello!\ninvalid-data\nread:not-found\nread:is-directory\nwrite:is-directory' <<EOF
import std.bytes
import std.bytes.Bytes
import std.file
import std.file.Error
import std.path
import std.path.Path

fn error_label(prefix: Str, err: Error) -> Str = match err {
  case Error.NotFound: prefix + ":not-found"
  case Error.PermissionDenied: prefix + ":permission-denied"
  case Error.AlreadyExists: prefix + ":already-exists"
  case Error.IsDirectory: prefix + ":is-directory"
  case Error.NotDirectory: prefix + ":not-directory"
  case Error.InvalidPath(message): prefix + ":invalid-path:" + message
  case Error.InvalidData(_): "invalid-data"
  case Error.AlreadyClosed: prefix + ":already-closed"
  case Error.Other(message): prefix + ":other:" + message
}

let output_path: Path = path.from_str("$ROUNDTRIP_PATH")

match file.write(output_path, bytes.from_str("roundtrip")) {
  case Result.Success(_): {
    let payload: Result[Bytes, Error] = file.read(output_path)
    match payload {
      case Result.Success(payload): puts("bytes:" + Show.show(bytes.length(payload)))
      case Result.Failure(err): puts(error_label("read", err))
    }
  }
  case Result.Failure(err): puts(error_label("write", err))
}

match file.write(output_path, "hello") {
  case Result.Success(_): {
    let text: Result[Str, Error] = file.read(output_path)
    match text {
      case Result.Success(value): puts("text:" + value)
      case Result.Failure(err): puts(error_label("read", err))
    }
  }
  case Result.Failure(err): puts(error_label("write", err))
}

match file.append(output_path, "!") {
  case Result.Success(_): {
    let text: Result[Str, Error] = file.read(output_path)
    match text {
      case Result.Success(value): puts("append:" + value)
      case Result.Failure(err): puts(error_label("read", err))
    }
  }
  case Result.Failure(err): puts(error_label("write", err))
}

let invalid_text: Result[Str, Error] = file.read("$INVALID_UTF8_PATH")
match invalid_text {
  case Result.Success(_): puts("invalid:unexpected-success")
  case Result.Failure(err): puts(error_label("read", err))
}

let missing_text: Result[Str, Error] = file.read("$MISSING_PATH")
match missing_text {
  case Result.Success(_): puts("read:unexpected-success")
  case Result.Failure(err): puts(error_label("read", err))
}

let dir_text: Result[Str, Error] = file.read("$DATA_DIR")
match dir_text {
  case Result.Success(_): puts("read-dir:unexpected-success")
  case Result.Failure(err): puts(error_label("read", err))
}

match file.write("$DATA_DIR", bytes.from_str("x")) {
  case Result.Success(_): puts("write-dir:unexpected-success")
  case Result.Failure(err): puts(error_label("write", err))
}
EOF

run_source_expect_output \
    "std.file scopes file handles and implements io protocols" \
    $'read:opened\nprotocol:opened\ngeneric:opened\nwritten:scoped!\nopen:not-found\nuse:read:is-directory\nleaked:read:already-closed' <<EOF
import std.bytes
import std.file
import std.file.Error
import std.file.UseError
import std.io

fn error_label(prefix: Str, err: Error) -> Str = match err {
  case Error.NotFound: prefix + ":not-found"
  case Error.PermissionDenied: prefix + ":permission-denied"
  case Error.AlreadyExists: prefix + ":already-exists"
  case Error.IsDirectory: prefix + ":is-directory"
  case Error.NotDirectory: prefix + ":not-directory"
  case Error.InvalidPath(message): prefix + ":invalid-path:" + message
  case Error.InvalidData(message): prefix + ":invalid-data:" + message
  case Error.AlreadyClosed: prefix + ":already-closed"
  case Error.Other(message): prefix + ":other:" + message
}

fn use_error_label(err: UseError[Error]) -> Str = match err {
  case UseError.Open(open_err): error_label("open", open_err)
  case UseError.Use(use_err): "use:" + error_label("read", use_err)
  case UseError.Close(close_err): error_label("close", close_err)
  case UseError.UseAndClose(use_err, close_err): "use:" + error_label("read", use_err) + "+" + error_label("close", close_err)
}

fn use_file[a, e](target: Str, body: (file.File) => Result[a, e]) => Result[a, UseError[e]] =
  file.open(target, file.Mode.Read, body)

let opened_text: Result[Str, UseError[Error]] = file.open("$EXISTING_PATH", file.Mode.Read, (handle) => file.read_all(handle))
match opened_text {
  case Result.Success(payload): puts("read:" + payload)
  case Result.Failure(err): puts(use_error_label(err))
}

let protocol_text: Result[Str, UseError[Error]] = file.open("$EXISTING_PATH", file.Mode.Read, (handle) => io.Read.read(handle))
match protocol_text {
  case Result.Success(payload): puts("protocol:" + payload)
  case Result.Failure(err): puts(use_error_label(err))
}

let generic_text: Result[Str, UseError[Error]] = use_file("$EXISTING_PATH", (handle) => io.Read.read(handle))
match generic_text {
  case Result.Success(payload): puts("generic:" + payload)
  case Result.Failure(err): puts(use_error_label(err))
}

let write_result: Result[Unit, UseError[Error]] = file.open("$ROUNDTRIP_PATH", file.Mode.Write, (handle) => {
  match io.Write.write(handle, "scoped") {
    case Result.Success(_): match io.Write.write(handle, "!") {
      case Result.Success(_): io.Write.flush(handle)
      case Result.Failure(err): Result.Failure(err)
    }
    case Result.Failure(err): Result.Failure(err)
  }
})
match write_result {
  case Result.Success(_): {
    let text: Result[Str, Error] = file.read("$ROUNDTRIP_PATH")
    match text {
      case Result.Success(value): puts("written:" + value)
      case Result.Failure(err): puts(error_label("read", err))
    }
  }
  case Result.Failure(err): puts(use_error_label(err))
}

let missing_result: Result[Str, UseError[Error]] = file.open("$MISSING_PATH", file.Mode.Read, (handle) => file.read_all(handle))
match missing_result {
  case Result.Success(_): puts("open:unexpected-success")
  case Result.Failure(err): puts(use_error_label(err))
}

let use_result: Result[Str, UseError[Error]] = file.open("$EXISTING_PATH", file.Mode.Read, (_handle) => {
  let dir_read: Result[Str, Error] = file.read("$DATA_DIR")
  dir_read
})
match use_result {
  case Result.Success(_): puts("use:unexpected-success")
  case Result.Failure(err): puts(use_error_label(err))
}

let leaked_result: Result[file.File, UseError[Error]] = file.open("$EXISTING_PATH", file.Mode.Read, (handle) => if (true) {
  Result.Success(handle)
} else {
  Result.Failure(Error.NotFound)
})
match leaked_result {
  case Result.Success(leaked): {
    let leaked_text: Result[Str, Error] = file.read_all(leaked)
    match leaked_text {
      case Result.Success(_): puts("leaked:unexpected-success")
      case Result.Failure(err): puts("leaked:" + error_label("read", err))
    }
  }
  case Result.Failure(err): puts(use_error_label(err))
}
EOF

run_source_expect_output \
    "std.dir exposes scalar operations and directory listings" \
    $'exists:false\nmake:ok\nexists:true\nmake-all:ok\nentry:entry.txt:file\ncurrent:true\nremove-nonempty:not-empty\nremove-all:ok' <<EOF
import std.dir
import std.dir.Entry
import std.dir.Error
import std.dir.Kind
import std.path

fn kind_label(kind: Kind) -> Str = match kind {
  case Kind.File: "file"
  case Kind.Directory: "dir"
  case Kind.Symlink: "symlink"
  case Kind.Other: "other"
}

fn error_label(prefix: Str, err: Error) -> Str = match err {
  case Error.NotFound: prefix + ":not-found"
  case Error.PermissionDenied: prefix + ":permission-denied"
  case Error.AlreadyExists: prefix + ":already-exists"
  case Error.NotDirectory: prefix + ":not-directory"
  case Error.NotEmpty: prefix + ":not-empty"
  case Error.InvalidPath(message): prefix + ":invalid-path:" + message
  case Error.Other(message): prefix + ":other:" + message
}

fn entry_label(entry: Entry) -> Str = match entry {
  case Entry(path_value, kind): path.basename(path_value) + ":" + kind_label(kind)
}

match dir.exists?("$CHILD_DIR") {
  case Result.Success(value): puts("exists:" + Show.show(value))
  case Result.Failure(err): puts(error_label("exists", err))
}

match dir.make("$CHILD_DIR") {
  case Result.Success(_): puts("make:ok")
  case Result.Failure(err): puts(error_label("make", err))
}

match dir.exists?("$CHILD_DIR") {
  case Result.Success(value): puts("exists:" + Show.show(value))
  case Result.Failure(err): puts(error_label("exists", err))
}

match dir.make_all("$NESTED_DIR") {
  case Result.Success(_): puts("make-all:ok")
  case Result.Failure(err): puts(error_label("make-all", err))
}

match dir.read("$LISTING_DIR") {
  case Result.Success(entries): puts("entry:" + entry_label(first(entries)))
  case Result.Failure(err): puts(error_label("read", err))
}

match dir.current() {
  case Result.Success(value): puts("current:" + Show.show(path.absolute?(value)))
  case Result.Failure(err): puts(error_label("current", err))
}

match dir.remove("$NONEMPTY_DIR") {
  case Result.Success(_): puts("remove-nonempty:unexpected")
  case Result.Failure(err): puts(error_label("remove-nonempty", err))
}

match dir.remove_all("$CHILD_DIR") {
  case Result.Success(_): match dir.remove_all("$DATA_DIR/nested") {
    case Result.Success(_): puts("remove-all:ok")
    case Result.Failure(err): puts(error_label("remove-all", err))
  }
  case Result.Failure(err): puts(error_label("remove-all", err))
}
EOF

run_source_expect_build_failure \
    "std.file raw close is private shim plumbing" \
    "does not export 'close_handle'" <<'EOF'
import std.file.close_handle

puts(0)
EOF

run_source_expect_build_failure \
    "std.file raw open is private shim plumbing" \
    "does not export 'open_handle'" <<'EOF'
import std.file.open_handle

puts(0)
EOF

run_source_expect_build_failure \
    "std.file old read_bytes helper is gone" \
    "does not export 'read_bytes'" <<'EOF'
import std.file.read_bytes

puts(0)
EOF

run_source_expect_build_failure \
    "std.file old write_bytes helper is gone" \
    "does not export 'write_bytes'" <<'EOF'
import std.file.write_bytes

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

run_source_emit_go_expect_stdio_shape \
    "std.io terminal helpers and std.file handles use private shims" <<EOF
import std.file
import std.file.Error
import std.io
import std.io.err
import std.dir

let _ = io.write("out")
let _ = io.flush()
let _ = err.write("err")
let _ = err.flush()
let _ = file.write("$ROUNDTRIP_PATH", "path")
let read_text: Result[Str, Error] = file.read("$ROUNDTRIP_PATH")
let dir_entries: Result[List[dir.Entry], dir.Error] = dir.read("$LISTING_DIR")
let file_protocol: Result[Unit, file.UseError[Error]] = file.open("$ROUNDTRIP_PATH", file.Mode.Append, (handle) => {
  match io.Write.write(handle, "x") {
    case Result.Success(_): io.Write.flush(handle)
    case Result.Failure(err): Result.Failure(err)
  }
})
puts(0)
EOF

run_source_expect_output \
    "std.io writes, flushes, prints and exposes stderr helpers" \
    $'value:42\nerr:bad' <<'EOF'
import std.io
import std.io.err

let _ = io.write("value:")
let _ = io.print(42)
let _ = err.write("err:")
let _ = err.print("bad")
EOF

run_source_expect_build_failure \
    "std.io stdin helper is private shim plumbing" \
    "does not export 'stdin'" <<'EOF'
import std.io.stdin

puts(0)
EOF

run_source_expect_build_failure \
    "std.io stdout helper is private shim plumbing" \
    "does not export 'stdout'" <<'EOF'
import std.io.stdout

puts(0)
EOF

run_source_expect_build_failure \
    "std.io Stdin token is private" \
    "does not export 'Stdin'" <<'EOF'
import std.io.Stdin

puts(0)
EOF

run_source_expect_build_failure \
    "std.io Stdout token is private" \
    "does not export 'Stdout'" <<'EOF'
import std.io.Stdout

puts(0)
EOF

run_source_expect_build_failure \
    "std.io stderr helper is private shim plumbing" \
    "does not export 'stderr'" <<'EOF'
import std.io.err.stderr

puts(0)
EOF

run_source_expect_build_failure \
    "std.io stderr token is private" \
    "does not export 'Stderr'" <<'EOF'
import std.io.err.Stderr

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

match io.read() {
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
