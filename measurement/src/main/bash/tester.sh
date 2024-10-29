#!/bin/bash

#
# This script tests several instances of the market.
#
# 2024-10-29
#

users=1
items=8
transactions=65536
userFactor=2
itemFactor=2
transactionFactor=1
iterations=12

resultFile="results.csv"

marketBin="market"
testInstanceGenBin="testInstanceGen"
timeBin="/usr/bin/time"

echo -e "This scripts runs the tests with the following parameters:"
echo -e " "
echo -e " users : initial=${users}, growth=${userFactor}"
echo -e " items : initial=${items}, growth=${itemFactor}"
echo -e " transactions : initial=${transactions}, growth=${transactionFactor}"
echo -e " iterations : ${iterations}"
echo -e " "

echo -e "index\tusers\titems\ttransac\tseconds" > ${resultFile}

for (( index = 0 ; index < iterations ; index++ )) ; do

  echo -e "Running:  index=${index}  users=${users}  items=${items}  transactions=${transactions} ..."

  inputFile=input-${index}.yaml
  outputFile=output-${index}.yaml

  row="${index}\t${users}\t${items}\t${transactions}\t%e"

  ${testInstanceGenBin} ${users} ${items} ${transactions} > ${inputFile}
  ${timeBin} --format="${row}" --output=${resultFile} --append ${marketBin} ${inputFile} > ${outputFile}

  users=$(( userFactor*users ))
  items=$(( itemFactor*items ))
  transactions=$(( transactionFactor*transactions ))

done

echo "Done."



