#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "PRELUDE + STDLIB TESTS"

run_project_expect_output() {
    local name="$1"
    local expected_output="$2"
    local source="$3"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local root
    local binpath
    local build_output
    local output
    root=$(mktemp -d marmoset_prelude_project.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_prelude_bin.XXXXXX")
    rm -f "$binpath"

    printf "%s" "$source" > "$root/main.mr"

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
        echo "  Build output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
    rm -rf "$root"
}

run_project_expect_output \
    "headerless entry auto-loads prelude sums, traits, and operators" \
    $'1\n42\nok\ntrue' \
    $'let opt: Option[Int] = Option.Some(42)\nlet status: Result[Str, Int] = Result.Success("ok")\nputs(Show.show(10 % 3))\nputs(Option.unwrap_or(opt, 0))\nputs(Result.unwrap_or(status, "bad"))\nputs(Eq.eq(1 + 2, 3))\n'

run_project_expect_output \
    "Option helpers are available as inherent methods without extra imports" \
    $'43\n7\ntrue\ntrue' \
    $'let absent: Option[Int] = Option.None\nlet lifted = Option.map(Option.Some(41), (x: Int) -> x + 1)\nlet bound = Option.bind(lifted, (x: Int) -> Option.Some(x + 1))\nmatch bound {\n  case Option.Some(v): puts(v)\n  case Option.None: puts(0)\n}\nputs(Option.unwrap_or(absent, 7))\nputs(Option.is_some(bound))\nputs(Option.is_none(absent))\n'

run_project_expect_output \
    "Result helpers are available as inherent methods without extra imports" \
    $'42\nerr!\n43\n0\ntrue\ntrue' \
    $'let ok_num: Result[Int, Str] = Result.Success(42)\nlet failed: Result[Int, Str] = Result.Failure("err")\nlet rendered = Result.map(ok_num, (n: Int) -> Show.show(n))\nmatch rendered {\n  case Result.Success(v): puts(v)\n  case Result.Failure(_): puts("bad")\n}\nlet boom = Result.map_fail(failed, (e: Str) -> e + "!")\nmatch boom {\n  case Result.Success(_): puts("bad")\n  case Result.Failure(msg): puts(msg)\n}\nlet next = Result.bind(ok_num, (x: Int) -> Result.Success(x + 1))\nmatch next {\n  case Result.Success(v): puts(v)\n  case Result.Failure(_): puts(0)\n}\nputs(Result.unwrap_or(failed, 0))\nputs(Result.is_ok(ok_num))\nputs(Result.is_err(failed))\n'

suite_end
