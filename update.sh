#!/bin/bash

# This script translates the Soda source files into Scala and Lean.
#
# 2024-02-17


pathToSodaDir="core/src/main/scala/soda/se/umu/cs/soda/prototype/example/market/core"
pathToLeanDir="Soda/se/umu/cs/soda/prototype/example/market/core"


# This translates all Soda files into Scala
soda scala .


# This translates all Soda files into Lean
files=" \
  Basic \
  MyList \
  Market \
"

for file in ${files}; do
  soda lean ${pathToSodaDir}/${file}.soda ${pathToLeanDir}/${file}.lean
done


# This compiles a binary in Lean
lake clean
lake build


# This is updates Lean
elan update


