#!/usr/bin/env bash
# Differential harness: every .fun program in the repo, through both runners,
# compared by outcome CLASS (value / elaboration error / evaluation error /
# hang), never by error text. The two runners print different messages, so the
# comparison is on the class and, for a value, on the normalized value produced
# by describe_value (OCaml) and Driver.Describe (C#).
#
# Agreement is decided first (same class, and for a value the same normalized
# string); only a disagreement is adjudicated, against the case's .expect, to
# say which runner is the wrong side. dotnet/std/*.fun have no .expect, so they
# are compared by raw class only.
#
# Usage:
#   dune build bin/differential.exe            # build the OCaml half once
#   dotnet build dotnet/test/Fun.Conformance   # build the C# half once
#   scripts/differential.sh
#
# Output: one line per file. SKIP lines print a file and why it was not run;
# every non-agreement prints the path, both outcomes, and whether the case is a
# known prototype divergence. The final line is the tally.
set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OCAML="$ROOT/_build/default/bin/differential.exe"
CS_DLL="$ROOT/dotnet/test/Fun.Conformance/bin/Debug/net10.0/Fun.Conformance.dll"
DIVERGENCES="$ROOT/test/conformance/prototype-divergences.txt"
TIMEOUT=60

[ -x "$OCAML" ] || { echo "build the OCaml half first: dune build bin/differential.exe" >&2; exit 2; }
[ -f "$CS_DLL" ] || { echo "build the C# half first: dotnet build dotnet/test/Fun.Conformance" >&2; exit 2; }

declare -A KNOWN=()
while IFS= read -r line; do
  line="${line%%#*}"
  read -r case _ <<< "$line"
  [ -n "$case" ] && KNOWN["$case"]=1
done < "$DIVERGENCES"

# One outcome per runner: "TAG msg" where TAG is VALUE / OK / ELAB / EVAL,
# or HANG (the 60s bound fired), or RUNNER-ERR (the runner itself died).
run_ocaml() {
  local out rc tag
  out="$(timeout -k 5 "$TIMEOUT" "$OCAML" "$1" 2>&1)"; rc=$?
  [ "$rc" -eq 124 ] && { echo "HANG"; return; }
  [ "$rc" -ne 0 ] && { echo "RUNNER-ERR rc=$rc $out"; return; }
  tag="${out%% *}"
  case "$tag" in OK|VALUE|ELAB|EVAL) printf '%s' "$out" ;; *) echo "RUNNER-ERR $out" ;; esac
}

run_cs() {
  local out rc tag
  out="$(timeout -k 5 "$TIMEOUT" dotnet "$CS_DLL" --file "$1" 2>&1)"; rc=$?
  [ "$rc" -eq 124 ] && { echo "HANG"; return; }
  [ "$rc" -ne 0 ] && { echo "RUNNER-ERR rc=$rc $out"; return; }
  tag="${out%% *}"
  case "$tag" in OK|VALUE|ELAB|EVAL|HANG) printf '%s' "$out" ;; *) echo "RUNNER-ERR $out" ;; esac
}

# Does an outcome (TAG + msg) satisfy the case's .expect?
right() {
  local tag=$1 msg=$2 expect=$3
  case "$expect" in
    ok) [ "$tag" = OK ] ;;
    error)
      # A refusal that is not a language error (unported path, invariant
      # failure) must not satisfy a case expecting `error`.
      case "$msg" in "not ported:"*|"invariant ("*) return 1 ;; esac
      [ "$tag" = ELAB ] || [ "$tag" = EVAL ] ;;
    *) [ "$tag" = VALUE ] && [ "$msg" = "$expect" ] ;;
  esac
}

agree=0; port_fails=0; proto_fails=0; both_fail=0; hang=0; runner_err=0; ran=0; skipped=0

mapfile -t FILES < <(
  find "$ROOT" -name '*.fun' \
    -not -path '*/_build/*' -not -path '*/bin/*' -not -path '*/obj/*' | sort)

for f in "${FILES[@]}"; do
  rel="${f#"$ROOT"/}"
  if [[ "$(basename "$f")" == *.unit-*.fun ]]; then
    printf 'SKIP %s unit file (imported by its case, not a standalone program)\n' "$rel"
    skipped=$((skipped+1))
    continue
  fi

  oc="$(run_ocaml "$f")"; oc_tag="${oc%% *}"; oc_msg="${oc#* }"
  cs="$(run_cs "$f")";     cs_tag="${cs%% *}"; cs_msg="${cs#* }"

  # cross-reference the known prototype divergences by "<area>/<name>"
  case_key="${rel#test/conformance/cases/}"; case_key="${case_key%.fun}"
  known=""; [ -n "${KNOWN[$case_key]+x}" ] && known=" [known prototype divergence]"

  if [ "$oc_tag" = HANG ] || [ "$cs_tag" = HANG ]; then
    hang=$((hang+1))
    printf 'HANG %s | OCaml: %s | C#: %s\n' "$rel" "$oc_tag" "$cs_tag"
  elif [ "$oc_tag" = RUNNER-ERR ] || [ "$cs_tag" = RUNNER-ERR ]; then
    runner_err=$((runner_err+1))
    printf 'RUNNER-ERR %s | OCaml: %s | C#: %s\n' "$rel" "$oc" "$cs"
  elif [ "$oc_tag" = "$cs_tag" ] && { [ "$oc_tag" != VALUE ] || [ "$oc_msg" = "$cs_msg" ]; }; then
    agree=$((agree+1))
  else
    # They disagree. Adjudicate against .expect where one exists.
    expect_file="${f%.fun}.expect"
    if [ -f "$expect_file" ]; then
      expect="$(tr -d '\r' < "$expect_file" | sed 's/[[:space:]]*$//')"
      if right "$oc_tag" "$oc_msg" "$expect"; then oc_right=1; else oc_right=0; fi
      if right "$cs_tag" "$cs_msg" "$expect"; then cs_right=1; else cs_right=0; fi
      if [ "$oc_right" -eq 0 ] && [ "$cs_right" -eq 1 ]; then
        proto_fails=$((proto_fails+1))
        printf 'PROTOTYPE-FAILS %s%s | OCaml: %s | C#: %s\n' "$rel" "$known" "$oc" "$cs"
      elif [ "$oc_right" -eq 1 ] && [ "$cs_right" -eq 0 ]; then
        port_fails=$((port_fails+1))
        printf 'PORT-FAILS %s%s | OCaml: %s | C#: %s\n' "$rel" "$known" "$oc" "$cs"
      else
        both_fail=$((both_fail+1))
        printf 'BOTH-FAIL %s%s | OCaml: %s | C#: %s\n' "$rel" "$known" "$oc" "$cs"
      fi
    else
      # No .expect (dotnet/std/*.fun): no spec to adjudicate, report the split.
      both_fail=$((both_fail+1))
      printf 'BOTH-FAIL %s (no .expect) | OCaml: %s | C#: %s\n' "$rel" "$oc" "$cs"
    fi
  fi
  ran=$((ran+1))
done

printf 'enumerated: %d files, ran: %d, skipped: %d\n' "${#FILES[@]}" "$ran" "$skipped"
printf 'agree: %d | port-fails: %d | prototype-fails: %d | both-fail: %d | hang: %d | runner-err: %d\n' \
  "$agree" "$port_fails" "$proto_fails" "$both_fail" "$hang" "$runner_err"
