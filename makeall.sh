#!/bin/bash

#
# This script builds the binary files.
# It requires `sbt` [https://www.scala-sbt.org/].
#

scalaVersion="3.3.4"
executableStub="exec java -jar \$0 \"\$@\" ; exit"

sbt scalaVersion sbtVersion version clean compile test package assembly

# Build the main binary file

mainBinaryFile="market"
mainJarFile="target/scala-${scalaVersion}/${mainBinaryFile}-*.jar"

echo ${executableStub} >${mainBinaryFile}
cat ${mainJarFile} >>${mainBinaryFile}
chmod u+x ${mainBinaryFile}

# Build the tool to create instances

toolBinaryFile="testInstanceGen"
toolModuleName="measurement"
toolJarFile="${toolModuleName}/target/scala-${scalaVersion}/${toolModuleName}-*.jar"

echo ${executableStub} >${toolBinaryFile}
cat ${toolJarFile} >>${toolBinaryFile}
chmod u+x ${toolBinaryFile}

# Copy an example

exampleFile="core/src/test/resources/example/example0.yaml"
localExampleFile="example0.yaml"

if [ ! -f ${localExampleFile} ]; then
  cp -p ${exampleFile} ${localExampleFile}
fi


