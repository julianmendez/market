#!/bin/bash


pathToMain="core/src/main"
pathToSodaFiles="${pathToMain}/scala/soda/se/umu/cs/soda/prototype/example/market"
pathToLeanDir="${pathToMain}/lean/soda/se/umu/cs/soda/prototype/example/market"

soda .

files=" \
  Basic \
  MyList \
  Market \
"

for file in ${files}; do
  soda lean ${pathToSodaFiles}/${file}.soda ${pathToLeanDir}/${file}.lean
done


