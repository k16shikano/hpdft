#!/bin/bash
set -euo pipefail
HPDFT=$(cabal list-bin exe:hpdft)
PDF=data/sample/book.pdf
echo "legacy:" && time "$HPDFT" --legacy "$PDF" > /dev/null
echo "geom:" && time "$HPDFT" --geom "$PDF" > /dev/null
echo "-p1:" && time "$HPDFT" -p 1 "$PDF" > /dev/null
