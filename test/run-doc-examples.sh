#!/usr/bin/env bash
# Runs the examples in the std/ doc comments through the conformance runner, so the
# documentation is tested behaviour.
#
# An example is one line in a .NET XML doc block in a std/*.fun file:
#
#   ## <code>EXPRESSION // returns VALUE</code>
#
# The expression is written to a .fun program (the conformance runner's single-file
# mode), the runner's `VALUE <v>` output is compared against VALUE, and PASS/FAIL is
# printed per example. Exits non-zero when any example fails.
#
# Usage: test/run-doc-examples.sh [std-file...]   (default: std/*.fun)
set -u
cd "$(dirname "$0")/.."

dll=test/Fun.Conformance/bin/Debug/net10.0/Fun.Conformance.dll
# The std sources are embedded into Fun.Compiler at build time: build first so the
# examples always run against the current std/.
dotnet build test/Fun.Conformance -v q >/dev/null || { echo "build failed"; exit 1; }

files=("$@"); [ $# -eq 0 ] && files=(std/*.fun)
tmp=$(mktemp --suffix=.fun)
trap 'rm -f "$tmp"' EXIT

pass=0 fail=0
for file in "${files[@]}"; do
  while IFS= read -r line; do
    body=${line#*'<code>'}
    body=${body%'</code>'}
    expr=${body%%' // returns '*}
    expected=${body#*' // returns '}
    printf '%s\n' "$expr" >"$tmp"
    out=$(dotnet "$dll" --file "$tmp")
    if [ "$out" = "VALUE $expected" ]; then
      printf 'PASS %s\n' "$expr"
      pass=$((pass + 1))
    else
      printf 'FAIL %s\n     want: VALUE %s\n     got:  %s\n' "$expr" "$expected" "$out"
      fail=$((fail + 1))
    fi
  done < <(grep -h '^## <code>.* // returns .*' "$file")
done

printf 'doc examples: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
