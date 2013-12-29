#!/bin/bash

# usage: $0 MODE TERM

make && ./tmp/proof-tree "$1" "$2" > tmp/proof.tex && make tmp/proof.pdf 
