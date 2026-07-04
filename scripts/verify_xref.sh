#!/usr/bin/env bash
# Smoke-test PDFs for xref table vs xref stream handling.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
HPDFT="${HPDFT:-$ROOT/dist-newstyle/build/x86_64-linux/ghc-9.10.1/hpdft-0.1.1.3/x/hpdft/build/hpdft/hpdft}"

if [[ ! -x "$HPDFT" ]]; then
  echo "Build hpdft first (cabal build --with-compiler=ghc-9.10)" >&2
  exit 1
fi

check() {
  local label=$1 file=$2
  printf '%-45s ' "$label"
  if "$HPDFT" "$file" -I >/dev/null 2>&1; then
    echo OK
  else
    echo FAIL
    return 1
  fi
}

fail=0
check "xref-table sample" "$ROOT/data/sample/140120220426528457.pdf" || fail=1
check "xref-table test.pdf" "$ROOT/test.pdf" || fail=1
check "xref-stream 56.pdf" "$ROOT/data/sample/56.pdf" || fail=1
check "xref-stream heatstroke" "$ROOT/data/sample/heatstroke_sokuhouti_20220620.pdf" || fail=1
check "xref-stream 000791761" "$ROOT/data/sample/000791761.pdf" || fail=1
check "xref-stream 45530" "$ROOT/45530.pdf" || fail=1

exit "$fail"
