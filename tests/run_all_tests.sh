#!/bin/sh
set -eu

if ! command -v swipl >/dev/null 2>&1; then
    echo "SWI-Prolog (swipl) is required to run the Prolog tests." >&2
    exit 1
fi

if ! command -v node >/dev/null 2>&1; then
    echo "Node.js is required to run the Child Prolog engine test." >&2
    exit 1
fi

test_log=$(mktemp)
trap 'rm -f "$test_log"' EXIT HUP INT TERM

for test_file in tests/test_*.pl; do
    echo "==> $test_file"
    if ! swipl -q -s "$test_file" >"$test_log" 2>&1 ||
        grep -Eq '✗ (FAIL|FAILED)|Some tests failed|^ERROR:' "$test_log"; then
        cat "$test_log"
        exit 1
    fi
    echo "PASS: $test_file"
done

echo "==> tests/run_tests.pl (PR2/PR3 staged tests)"
if ! swipl -q -s tests/run_tests.pl >"$test_log" 2>&1 ||
    grep -q '^ERROR:' "$test_log"; then
    cat "$test_log"
    exit 1
fi
echo "PASS: staged PR2/PR3 tests"

echo "==> tests/test_child_prolog_engine.js"
node tests/test_child_prolog_engine.js

echo "All Starlog tests completed."
