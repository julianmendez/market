#!/bin/bash

#
# This script builds the binary file.
# It requires `sbt` [https://www.scala-sbt.org/].
#

sbt scalaVersion sbtVersion version clean compile test package assembly

scalaVersion="3.4.2"
binaryFile="market"
executableStub="exec java -jar \$0 \"\$@\" ; exit"
jarFile="target/scala-${scalaVersion}/${binaryFile}-*.jar"
exampleFile="core/src/test/resources/example/example0.yaml"
localExampleFile="example0.yaml"

echo ${executableStub} >${binaryFile}
cat ${jarFile} >>${binaryFile}
chmod u+x ${binaryFile}

if [ ! -f ${localExampleFile} ]; then
  cp -p ${exampleFile} ${localExampleFile}
fi


