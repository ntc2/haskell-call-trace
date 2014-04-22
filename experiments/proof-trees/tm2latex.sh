#!/bin/bash

if (($# != 4)); then
  echo "usage: $0 GEN OUT CTX TERM"                                      > /dev/stderr
  echo ""                                                                > /dev/stderr
  echo "Run GEN MODE CTX TERM > tmp/GEN.OUT.MODE.tex for both modes and" > /dev/stderr
  echo "process to PDF. The GEN should be tmp/proof-tree or"             > /dev/stderr
  echo "tmp/self-contained-proof-tree"                                   > /dev/stderr
  exit 2
fi

gen="$1"
out="$2"
ctx="$3"
tm="$4"
file=tmp/"$(basename "$gen")"."$out"

for mode in True False; do
  "$gen" "$mode" "$ctx" "$tm" > "$file"."$mode".tex && \
  make "$file"."$mode".pdf
done
