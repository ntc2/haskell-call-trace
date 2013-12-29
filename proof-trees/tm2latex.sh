#!/bin/bash

if (($# != 3)); then
  echo "usage: $0 OUT CTX TERM"                                               > /dev/stderr
  echo ""                                                                     > /dev/stderr
  echo "Run ./tmp/proof-tree MODE CTX TERM > OUT.MODE.tex for both modes and" > /dev/stderr
  echo "process to PDF."                                                      > /dev/stderr
  exit 2
fi

out="$1"
ctx="$2"
tm="$3"

for mode in True False; do
  ./tmp/proof-tree "$mode" "$ctx" "$tm" > tmp/"$out"."$mode".tex && \
  make tmp/"$out"."$mode".pdf
done
