#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$REPO_ROOT"

require_snippet() {
    local file="$1"
    local snippet="$2"

    if ! rg --fixed-strings --quiet "$snippet" "$file"; then
        echo "Missing stdlib idiom in $file:" >&2
        echo "  $snippet" >&2
        return 1
    fi
}

reject_snippet() {
    local file="$1"
    local snippet="$2"

    if rg --fixed-strings --quiet "$snippet" "$file"; then
        echo "Avoid pre-error-model propagation style in $file:" >&2
        echo "  $snippet" >&2
        return 1
    fi
}

require_snippet std/file.mr "bytes.Decode.decode(payload) wrap Error.InvalidData"
require_snippet std/file.mr "let file = try file_shim.open_handle(path, mode) wrap UseError.Open"
# Output helpers return the write result directly; flushing is explicit.
require_snippet std/io.mr 'Write.write(writer, "#{value}")'
require_snippet std/io.mr 'Write.write(writer, "#{value}\n")'
reject_snippet std/io.mr "Result.bind((_) => Write.flush(writer))"

reject_snippet std/file.mr "let value = try bytes.Decode.decode(payload) wrap Error.InvalidData"
reject_snippet std/file.mr "Result.wrap((err: DecodeError) -> Error.InvalidData(err))"
reject_snippet std/io.mr "case Result.Failure(err): Result.Failure(err)"
