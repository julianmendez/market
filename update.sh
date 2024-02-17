#!/bin/bash


pathToMain="core/src/main"
pathToSodaFiles="${pathToMain}/scala/soda/se/umu/cs/soda/prototype/example/market"
pathToLeanDir="${pathToMain}/lean/soda/se/umu/cs/soda/prototype/example/market"

soda .
soda lean ${pathToSodaFiles}/Market.soda ${pathToLeanDir}/Market.lean

