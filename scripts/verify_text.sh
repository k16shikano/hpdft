#!/usr/bin/env bash
# Golden tests: compare full-text extraction of data/fixtures PDFs
# against expected output. Fixtures are authored in-repo (see
# gen_fixtures.py) and safe for CI, unlike data/sample.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FIXDIR="$ROOT/data/fixtures"
EXPDIR="$FIXDIR/expected"

if [[ -z "${HPDFT:-}" ]]; then
  HPDFT="$(find "$ROOT/dist-newstyle/build" -path '*/hpdft/hpdft' -type f -executable 2>/dev/null | head -1 || true)"
fi

if [[ -z "${HPDFT:-}" || ! -x "$HPDFT" ]]; then
  echo "Build hpdft first (cabal build) or set HPDFT to the binary path" >&2
  exit 1
fi

if [[ "${1:-}" == "--update" ]]; then
  mkdir -p "$EXPDIR"
  for pdf in "$FIXDIR"/*.pdf; do
    name="$(basename "$pdf" .pdf)"
    "$HPDFT" "$pdf" > "$EXPDIR/$name.txt"
    echo "updated $EXPDIR/$name.txt"
  done
  exit 0
fi

fail=0
shopt -s nullglob
for pdf in "$FIXDIR"/*.pdf; do
  name="$(basename "$pdf" .pdf)"
  expected="$EXPDIR/$name.txt"
  printf '%-45s ' "$name"
  if [[ ! -f "$expected" ]]; then
    echo "MISSING (run with --update)"
    fail=1
    continue
  fi
  if diff -q <("$HPDFT" "$pdf" 2>/dev/null) "$expected" >/dev/null 2>&1; then
    echo OK
  else
    echo FAIL
    diff <("$HPDFT" "$pdf" 2>/dev/null) "$expected" | head -10 || true
    fail=1
  fi
done

exit "$fail"
