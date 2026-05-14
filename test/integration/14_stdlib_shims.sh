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
        grep -qF 'func std__file__read_std__path__Path_ret_Result_string_std__file__Error' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func std__file__write_std__path__Path_string' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func extern__std_file__read_path(path std__path__Path)' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func extern__std_file__write_path(path std__path__Path, bytes marmoset.Bytes)' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func extern__std_file__read_handle(file mapi_std_file.File)' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func extern__std_file__write_handle(file mapi_std_file.File, bytes marmoset.Bytes)' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func extern__std_file__read_line_handle(file mapi_std_file.File)' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func extern__std_file__read_chunk_handle(file mapi_std_file.File, size int64)' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func std__io__Read_read_std__file__File' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func std__io__Write_write_std__file__File' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func std__io__Write_flush_std__file__File' "$outdir/main.go" || failures=$((failures + 1))
        grep -qF 'func extern__std_dir__ls(path std__path__Path)' "$outdir/main.go" || failures=$((failures + 1))
        if rg -q 'read_data|write_data|append_data|read_all_data|write_all_data|ReadData|WriteData|AppendData|ReadAllData|WriteAllData|std__file__Path|flush_path|FlushPath|func Stdin|func Stdout|func Stderr|std__dir__Entry_Entry_tag|std__dir__RawEntry_RawEntry_tag|RawEntry' "$outdir"; then
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
LINE_PATH="$DATA_DIR/lines.txt"
CHUNK_PATH="$DATA_DIR/chunk.txt"
INVALID_UTF8_PATH="$DATA_DIR/invalid.bin"
CHILD_DIR="$DATA_DIR/child"
NONEMPTY_DIR="$DATA_DIR/nonempty"
NESTED_DIR="$DATA_DIR/nested/inner"
LISTING_DIR="$DATA_DIR/listing"
LISTING_FILE="$LISTING_DIR/entry.txt"
NONEMPTY_FILE="$NONEMPTY_DIR/child.txt"
printf "opened" > "$EXISTING_PATH"
printf "alpha\nbeta\r\ngamma" > "$LINE_PATH"
printf "abcdef" > "$CHUNK_PATH"
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
    "std.bytes shim decode errors expose canonical message" \
    $'Invalid UTF-8' <<EOF
import std.bytes
import std.bytes.Bytes
import std.error
import std.file
import std.path.Path

match file.read(Path("$INVALID_UTF8_PATH")) {
  case Result.Success(raw): match bytes.to_str(raw) {
    case Result.Success(_): puts("unexpected-success")
    case Result.Failure(err): puts(error.message(err))
  }
  case Result.Failure(_): puts("read-failed")
}
EOF

run_source_expect_output \
    "std.path manipulates pure path values" \
    $'base:example.txt\ndir:/tmp/marmoset\next:.txt\njoin:/tmp/marmoset/child\nclean:/tmp/b\nabs:true\nrel:true' <<'EOF'
import std.path
import std.path.Path

fn path_label(value: Path) -> Str = match value {
  case Path(raw): raw
}

let root: Path = Path("/tmp/marmoset/example.txt")
let joined: Path = path.join(path.dirname(root), Path("child"))

puts("base:" + path.basename(root))
puts("dir:" + path_label(path.dirname(root)))
puts("ext:" + path.extname(root))
puts("join:" + path_label(joined))
puts("clean:" + path_label(path.clean(Path("/tmp/a/../b"))))
puts("abs:" + Show.show(path.absolute?(root)))
puts("rel:" + Show.show(path.relative?(Path("notes/today.md"))))
EOF

run_source_expect_build_failure \
    "std.path old structural path helper is gone" \
    "does not export" <<'EOF'
import std.path.PathLike

puts(0)
EOF

run_source_expect_output \
    "std.file uses traits for text, bytes and filesystem errors" \
    $'bytes:9\ntext:hello\nappend:hello!\nread:invalid-data:invalid-utf8\nread:not-found\nread:is-directory\nwrite:is-directory' <<EOF
import std.bytes
import std.bytes.Bytes
import std.bytes.DecodeError
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
  case Error.InvalidData(DecodeError.InvalidUtf8): prefix + ":invalid-data:invalid-utf8"
  case Error.AlreadyClosed: prefix + ":already-closed"
  case Error.Other(message): prefix + ":other:" + message
}

let output_path: Path = Path("$ROUNDTRIP_PATH")

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

let invalid_text: Result[Str, Error] = file.read(Path("$INVALID_UTF8_PATH"))
match invalid_text {
  case Result.Success(_): puts("invalid:unexpected-success")
  case Result.Failure(err): puts(error_label("read", err))
}

let missing_text: Result[Str, Error] = file.read(Path("$MISSING_PATH"))
match missing_text {
  case Result.Success(_): puts("read:unexpected-success")
  case Result.Failure(err): puts(error_label("read", err))
}

let dir_text: Result[Str, Error] = file.read(Path("$DATA_DIR"))
match dir_text {
  case Result.Success(_): puts("read-dir:unexpected-success")
  case Result.Failure(err): puts(error_label("read", err))
}

match file.write(Path("$DATA_DIR"), bytes.from_str("x")) {
  case Result.Success(_): puts("write-dir:unexpected-success")
  case Result.Failure(err): puts(error_label("write", err))
}
EOF

run_source_expect_output \
    "std.file scopes file handles and implements io protocols" \
    $'read:opened\nprotocol:opened\ngeneric:opened\nline1:alpha\nline2:beta\nline3:gamma\nline4:none\nchunk1:ab\nchunk2:cde\nchunk3:f\nchunk4:none\nwritten:scoped!\nopen:not-found\nuse:read:is-directory\nleaked:read:already-closed' <<EOF
import std.bytes
import std.bytes.Bytes
import std.bytes.DecodeError
import std.file
import std.file.Error
import std.file.UseError
import std.io
import std.path.Path

fn error_label(prefix: Str, err: Error) -> Str = match err {
  case Error.NotFound: prefix + ":not-found"
  case Error.PermissionDenied: prefix + ":permission-denied"
  case Error.AlreadyExists: prefix + ":already-exists"
  case Error.IsDirectory: prefix + ":is-directory"
  case Error.NotDirectory: prefix + ":not-directory"
  case Error.InvalidPath(message): prefix + ":invalid-path:" + message
  case Error.InvalidData(DecodeError.InvalidUtf8): prefix + ":invalid-data:invalid-utf8"
  case Error.AlreadyClosed: prefix + ":already-closed"
  case Error.Other(message): prefix + ":other:" + message
}

fn use_error_label(err: UseError[Error]) -> Str = match err {
  case UseError.Open(open_err): error_label("open", open_err)
  case UseError.Use(use_err): "use:" + error_label("read", use_err)
  case UseError.Close(close_err): error_label("close", close_err)
  case UseError.UseAndClose(use_err, close_err): "use:" + error_label("read", use_err) + "+" + error_label("close", close_err)
}

fn use_file[a, e](target: Path, body: (file.File) => Result[a, e]) => Result[a, UseError[e]] =
  file.open(target, file.Mode.Read, body)

fn line_label(prefix: Str, result: Result[Option[Str], Error]) -> Str = match result {
  case Result.Success(option): match option {
    case Option.Some(value): prefix + ":" + value
    case Option.None: prefix + ":none"
  }
  case Result.Failure(err): error_label(prefix, err)
}

fn chunk_label(prefix: Str, result: Result[Option[Bytes], Error]) -> Str = match result {
  case Result.Success(option): match option {
    case Option.Some(value): prefix + ":" + bytes.to_str_lossy(value)
    case Option.None: prefix + ":none"
  }
  case Result.Failure(err): error_label(prefix, err)
}

let opened_text: Result[Str, UseError[Error]] = file.open(Path("$EXISTING_PATH"), file.Mode.Read, (handle) => file.read_all(handle))
match opened_text {
  case Result.Success(payload): puts("read:" + payload)
  case Result.Failure(err): puts(use_error_label(err))
}

let protocol_text: Result[Str, UseError[Error]] = file.open(Path("$EXISTING_PATH"), file.Mode.Read, (handle) => io.Read.read(handle))
match protocol_text {
  case Result.Success(payload): puts("protocol:" + payload)
  case Result.Failure(err): puts(use_error_label(err))
}

let generic_text: Result[Str, UseError[Error]] = use_file(Path("$EXISTING_PATH"), (handle) => io.Read.read(handle))
match generic_text {
  case Result.Success(payload): puts("generic:" + payload)
  case Result.Failure(err): puts(use_error_label(err))
}

let line_result: Result[Str, UseError[Error]] = file.open(Path("$LINE_PATH"), file.Mode.Read, (handle) => {
  puts(line_label("line1", file.read_line(handle)))
  puts(line_label("line2", file.read_line(handle)))
  puts(line_label("line3", file.read_line(handle)))
  puts(line_label("line4", file.read_line(handle)))
  Result.Success("ok")
})
match line_result {
  case Result.Success(_): {}
  case Result.Failure(err): puts(use_error_label(err))
}

let chunk_result: Result[Str, UseError[Error]] = file.open(Path("$CHUNK_PATH"), file.Mode.Read, (handle) => {
  puts(chunk_label("chunk1", file.read_chunk(handle, 2)))
  puts(chunk_label("chunk2", file.read_chunk(handle, 3)))
  puts(chunk_label("chunk3", file.read_chunk(handle, 3)))
  puts(chunk_label("chunk4", file.read_chunk(handle, 3)))
  Result.Success("ok")
})
match chunk_result {
  case Result.Success(_): {}
  case Result.Failure(err): puts(use_error_label(err))
}

let write_result: Result[Unit, UseError[Error]] = file.open(Path("$ROUNDTRIP_PATH"), file.Mode.Write, (handle) => {
  match file.write_all(handle, "scoped") {
    case Result.Success(_): match io.Write.write(handle, "!") {
      case Result.Success(_): file.flush(handle)
      case Result.Failure(err): Result.Failure(err)
    }
    case Result.Failure(err): Result.Failure(err)
  }
})
match write_result {
  case Result.Success(_): {
    let text: Result[Str, Error] = file.read(Path("$ROUNDTRIP_PATH"))
    match text {
      case Result.Success(value): puts("written:" + value)
      case Result.Failure(err): puts(error_label("read", err))
    }
  }
  case Result.Failure(err): puts(use_error_label(err))
}

let missing_result: Result[Str, UseError[Error]] = file.open(Path("$MISSING_PATH"), file.Mode.Read, (handle) => file.read_all(handle))
match missing_result {
  case Result.Success(_): puts("open:unexpected-success")
  case Result.Failure(err): puts(use_error_label(err))
}

let use_result: Result[Str, UseError[Error]] = file.open(Path("$EXISTING_PATH"), file.Mode.Read, (_handle) => {
  let dir_read: Result[Str, Error] = file.read(Path("$DATA_DIR"))
  dir_read
})
match use_result {
  case Result.Success(_): puts("use:unexpected-success")
  case Result.Failure(err): puts(use_error_label(err))
}

let leaked_result: Result[file.File, UseError[Error]] = file.open(Path("$EXISTING_PATH"), file.Mode.Read, (handle) => if (true) {
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
    $'exists:false\nexists-file:false\nmkdir:ok\nexists:true\nmkdir-tree:ok\nentry:entry.txt:file\npwd:true\nrmdir-file:not-directory\nrmdir-nonempty:not-empty\nrmdir-tree-file:not-directory\nrmdir-tree:ok' <<EOF
import std.dir
import std.dir.Entry
import std.dir.ExistsError
import std.dir.Kind
import std.dir.LsError
import std.dir.MkdirError
import std.dir.PwdError
import std.dir.RmdirError
import std.path
import std.path.Path

fn kind_label(kind: Kind) -> Str = match kind {
  case Kind.File: "file"
  case Kind.Directory: "dir"
  case Kind.Symlink: "symlink"
  case Kind.Other: "other"
}

fn ls_error_label(prefix: Str, err: LsError) -> Str = match err {
  case LsError.NotFound: prefix + ":not-found"
  case LsError.PermissionDenied: prefix + ":permission-denied"
  case LsError.NotDirectory: prefix + ":not-directory"
  case LsError.InvalidPath(message): prefix + ":invalid-path:" + message
  case LsError.Other(message): prefix + ":other:" + message
}

fn mkdir_error_label(prefix: Str, err: MkdirError) -> Str = match err {
  case MkdirError.NotFound: prefix + ":not-found"
  case MkdirError.PermissionDenied: prefix + ":permission-denied"
  case MkdirError.AlreadyExists: prefix + ":already-exists"
  case MkdirError.NotDirectory: prefix + ":not-directory"
  case MkdirError.InvalidPath(message): prefix + ":invalid-path:" + message
  case MkdirError.Other(message): prefix + ":other:" + message
}

fn rmdir_error_label(prefix: Str, err: RmdirError) -> Str = match err {
  case RmdirError.NotFound: prefix + ":not-found"
  case RmdirError.PermissionDenied: prefix + ":permission-denied"
  case RmdirError.NotDirectory: prefix + ":not-directory"
  case RmdirError.NotEmpty: prefix + ":not-empty"
  case RmdirError.InvalidPath(message): prefix + ":invalid-path:" + message
  case RmdirError.Other(message): prefix + ":other:" + message
}

fn exists_error_label(prefix: Str, err: ExistsError) -> Str = match err {
  case ExistsError.PermissionDenied: prefix + ":permission-denied"
  case ExistsError.InvalidPath(message): prefix + ":invalid-path:" + message
  case ExistsError.Other(message): prefix + ":other:" + message
}

fn pwd_error_label(prefix: Str, err: PwdError) -> Str = match err {
  case PwdError.NotFound: prefix + ":not-found"
  case PwdError.PermissionDenied: prefix + ":permission-denied"
  case PwdError.Other(message): prefix + ":other:" + message
}

fn entry_label(entry: Entry) -> Str = match entry {
  case Entry(entry_path, kind): path.basename(entry_path) + ":" + kind_label(kind)
}

match dir.exists?(Path("$CHILD_DIR")) {
  case Result.Success(value): puts("exists:" + Show.show(value))
  case Result.Failure(err): puts(exists_error_label("exists", err))
}

match dir.exists?(Path("$LISTING_FILE")) {
  case Result.Success(value): puts("exists-file:" + Show.show(value))
  case Result.Failure(err): puts(exists_error_label("exists-file", err))
}

match dir.mkdir(Path("$CHILD_DIR")) {
  case Result.Success(_): puts("mkdir:ok")
  case Result.Failure(err): puts(mkdir_error_label("mkdir", err))
}

match dir.exists?(Path("$CHILD_DIR")) {
  case Result.Success(value): puts("exists:" + Show.show(value))
  case Result.Failure(err): puts(exists_error_label("exists", err))
}

match dir.mkdir_tree(Path("$NESTED_DIR")) {
  case Result.Success(_): puts("mkdir-tree:ok")
  case Result.Failure(err): puts(mkdir_error_label("mkdir-tree", err))
}

match dir.ls(Path("$LISTING_DIR")) {
  case Result.Success(entries): puts("entry:" + entry_label(first(entries)))
  case Result.Failure(err): puts(ls_error_label("ls", err))
}

match dir.pwd() {
  case Result.Success(value): puts("pwd:" + Show.show(path.absolute?(value)))
  case Result.Failure(err): puts(pwd_error_label("pwd", err))
}

match dir.rmdir(Path("$LISTING_FILE")) {
  case Result.Success(_): puts("rmdir-file:unexpected")
  case Result.Failure(err): puts(rmdir_error_label("rmdir-file", err))
}

match dir.rmdir(Path("$NONEMPTY_DIR")) {
  case Result.Success(_): puts("rmdir-nonempty:unexpected")
  case Result.Failure(err): puts(rmdir_error_label("rmdir-nonempty", err))
}

match dir.rmdir_tree(Path("$LISTING_FILE")) {
  case Result.Success(_): puts("rmdir-tree-file:unexpected")
  case Result.Failure(err): puts(rmdir_error_label("rmdir-tree-file", err))
}

match dir.rmdir_tree(Path("$CHILD_DIR")) {
  case Result.Success(_): match dir.rmdir_tree(Path("$DATA_DIR/nested")) {
    case Result.Success(_): puts("rmdir-tree:ok")
    case Result.Failure(err): puts(rmdir_error_label("rmdir-tree", err))
  }
  case Result.Failure(err): puts(rmdir_error_label("rmdir-tree", err))
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
import std.path.Path

let out_write = io.write("out")
let out_flush = io.flush()
let err_write = err.write("err")
let err_flush = err.flush()
let path_write = file.write(Path("$ROUNDTRIP_PATH"), "path")
let read_text: Result[Str, Error] = file.read(Path("$ROUNDTRIP_PATH"))
let dir_entries: Result[List[dir.Entry], dir.LsError] = dir.ls(Path("$LISTING_DIR"))
let file_protocol: Result[Unit, file.UseError[Error]] = file.open(Path("$ROUNDTRIP_PATH"), file.Mode.Append, (handle) => {
  match io.Write.write(handle, "x") {
    case Result.Success(_): io.Write.flush(handle)
    case Result.Failure(err): Result.Failure(err)
  }
})
puts(0)
EOF

run_source_expect_output \
    "std.io write print puts and flush expose stdout and stderr helpers" \
    $'value:42!\nerr:bad!' <<'EOF'
import std.io
import std.io.err

match io.write("value:") {
  case Result.Success(_): true
  case Result.Failure(_): false
}

match io.print(42) {
  case Result.Success(_): true
  case Result.Failure(_): false
}

match io.puts("!") {
  case Result.Success(_): true
  case Result.Failure(_): false
}

match io.flush() {
  case Result.Success(_): true
  case Result.Failure(_): false
}

match err.write("err:") {
  case Result.Success(_): true
  case Result.Failure(_): false
}

match err.print("bad") {
  case Result.Success(_): true
  case Result.Failure(_): false
}

match err.puts("!") {
  case Result.Success(_): true
  case Result.Failure(_): false
}

match err.flush() {
  case Result.Success(_): true
  case Result.Failure(_): false
}
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
  case Result.Success(line): puts("line:" + line)
  case Result.Failure(err): puts(read_error_label(err))
}

match io.read() {
  case Result.Success(line): puts("line:" + line)
  case Result.Failure(err): puts(read_error_label(err))
}

match io.read() {
  case Result.Success(line): puts("line:" + line)
  case Result.Failure(err): puts(read_error_label(err))
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
