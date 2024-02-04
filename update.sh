#!/bin/bash


pathToFile="market-core/src/main/scala/soda/se/umu/cs/soda/prototype/example/market"

soda .
cd ${pathToFile}
soda lean Market.soda Market.lean

