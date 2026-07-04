#!/usr/bin/env bash
# Smoke-test PDFs under data/sample (local fixtures, not in the public repo).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SAMPLE_DIR="$ROOT/data/sample"

if [[ -z "${HPDFT:-}" ]]; then
  HPDFT="$(find "$ROOT/dist-newstyle/build" -path '*/hpdft/hpdft' -type f -executable 2>/dev/null | head -1 || true)"
fi

if [[ -z "${HPDFT:-}" || ! -x "$HPDFT" ]]; then
  echo "Build hpdft first (cabal build) or set HPDFT to the binary path" >&2
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

check_pw() {
  local label=$1 file=$2 pw=$3
  printf '%-45s ' "$label"
  if "$HPDFT" -P "$pw" "$file" -I >/dev/null 2>&1; then
    echo OK
  else
    echo FAIL
    return 1
  fi
}

fail=0
shopt -s nullglob
pdfs=("$SAMPLE_DIR"/*.pdf)

if (( ${#pdfs[@]} == 0 )); then
  echo "No PDFs in data/sample; add local test files and re-run." >&2
  exit 0
fi

for pdf in "${pdfs[@]}"; do
  check "$(basename "$pdf")" "$pdf" || fail=1
done

if command -v pdftk >/dev/null 2>&1; then
  seed="$SAMPLE_DIR/56.pdf"
  if [[ -f "$seed" ]]; then
    pdftk "$seed" output /tmp/hpdft-enc-rc4.pdf user_pw test123 encrypt_40bit 2>/dev/null || true
    pdftk "$seed" output /tmp/hpdft-enc-aes.pdf user_pw test123 2>/dev/null || true
    if [[ -f /tmp/hpdft-enc-rc4.pdf ]]; then
      check_pw "encrypted RC4 (R2)" /tmp/hpdft-enc-rc4.pdf test123 || fail=1
    fi
    if [[ -f /tmp/hpdft-enc-aes.pdf ]]; then
      check_pw "encrypted AES (R4)" /tmp/hpdft-enc-aes.pdf test123 || fail=1
    fi
  fi
fi

exit "$fail"
