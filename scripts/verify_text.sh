#!/usr/bin/env bash
# Golden tests: compare full-text extraction of data/fixtures PDFs
# against expected output, for both the default (tagged->geometry)
# pipeline and the pre-0.3 legacy extractor (--legacy). Fixtures are
# authored in-repo (see gen_fixtures.hs) and safe for CI, unlike
# data/sample.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FIXDIR="$ROOT/data/fixtures"
EXPDIR="$FIXDIR/expected"
EXPLEGACYDIR="$FIXDIR/expected-legacy"

if [[ -z "${HPDFT:-}" ]]; then
  HPDFT="$(find "$ROOT/dist-newstyle/build" -path '*/hpdft/hpdft' -type f -executable 2>/dev/null | head -1 || true)"
fi

if [[ -z "${HPDFT:-}" || ! -x "$HPDFT" ]]; then
  echo "Build hpdft first (cabal build) or set HPDFT to the binary path" >&2
  exit 1
fi

if [[ "${1:-}" == "--update" ]]; then
  mkdir -p "$EXPDIR" "$EXPLEGACYDIR"
  for pdf in "$FIXDIR"/*.pdf; do
    name="$(basename "$pdf" .pdf)"
    "$HPDFT" "$pdf" > "$EXPDIR/$name.txt"
    echo "updated $EXPDIR/$name.txt"
    "$HPDFT" --legacy "$pdf" > "$EXPLEGACYDIR/$name.txt"
    echo "updated $EXPLEGACYDIR/$name.txt"
  done
  exit 0
fi

fail=0
shopt -s nullglob

run_check() {
  local label=$1 expected=$2
  shift 2
  printf '%-45s ' "$label"
  if [[ ! -f "$expected" ]]; then
    echo "MISSING (run with --update)"
    fail=1
    return
  fi
  if diff -q <("$@" 2>/dev/null) "$expected" >/dev/null 2>&1; then
    echo OK
  else
    echo FAIL
    diff <("$@" 2>/dev/null) "$expected" | head -10 || true
    fail=1
  fi
}

for pdf in "$FIXDIR"/*.pdf; do
  name="$(basename "$pdf" .pdf)"
  run_check "$name" "$EXPDIR/$name.txt" "$HPDFT" "$pdf"
  run_check "$name (legacy)" "$EXPLEGACYDIR/$name.txt" "$HPDFT" --legacy "$pdf"
done

exit "$fail"
