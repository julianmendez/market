#!/bin/bash

# This script translates the Soda source files into Scala and Lean.
#
# 2024-02-17


pathToMain="core/src/main"
pathToSodaFiles="${pathToMain}/scala/soda/se/umu/cs/soda/prototype/example/market"
pathToLeanDir="${pathToMain}/lean/soda/se/umu/cs/soda/prototype/example/market"


# This translates all Soda files into Scala
soda scala .


# This translates all Soda files into Lean
files=" \
  Basic \
  MyList \
  Market \
"

for file in ${files}; do
  soda lean ${pathToSodaFiles}/${file}.soda ${pathToLeanDir}/${file}.lean
done


# This compiles a binary in Lean
cd "${pathToMain}/lean"
lake clean
lake build


# This is updates Lean
elan update


