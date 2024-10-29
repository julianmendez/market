# [Market](https://julianmendez.github.io/market/)

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)][license]
[![build](https://github.com/julianmendez/market/workflows/Scala%20CI/badge.svg)][build-status]

This is a multi-agent prototype to show a market modeled in [Soda][soda].


## Where to start

Steps to run the example :
1. To run these steps, you need to install:
   - a. [Scala 3][scala]
   - b. [sbt][sbt] (if you need to build the binaries)
   - c. [Java][java] (to execute JAR files)
   - d. [Lean 4][lean] (to verify the proofs)
   - e. [Soda][soda] (to translate the examples)
2. Get the Soda translator binary by either doing the following:
   - a. download the Linux binary from [releases][soda-releases]
   - b. **or** clone the [Soda repository][soda-repo] and compile it, by either:
      - i. run `bash makeall.sh`, from a Linux compatible environment
      - ii. **or** run `sbt` to get an executable JAR file as indicated in the
        [Soda release notes][soda-release-notes]. The command itself is described in `build`
        and the file is `release`. To execute a JAR file, you need a [Java][java] environment
        installed, and you need to run `java -jar filename.jar`, for a JAR file named
        `filename.jar`.
   - c. make sure that the binary file `soda` is reachable from the command line, for
     example by updating the environment variable `PATH`.
3. Once you got the binary translator, go to an empty directory and try
   `soda manual`. It will output a piece of code with many examples, but most importantly,
   this mini-manual is a "Hello, World!" program itself. Write `soda manual > Manual.soda` and
   you get the manual.
4. To compile this project, run `bash makeall.sh`, from a Bash terminal. It creates a file named
   `market` and copies the example `example0.yaml` to the project root directory.
5. Try running `./market example0.yaml > example0Output.yaml`, and modify its values, to test how it works.
   The supported operations are:
   - **deposit** *user* *amount* : declare a user account if necessary, and put money into the
     account;
   - **assign** *item* *user* : declare an item if necessary, and define its owner;
   - **price** *item* *amount* : define the price of an item, or hide it if its price is 0;
   - **sell** *item* *user* : exchange an item and its price between a buyer and a seller.
6. Try running the test instance generator with `./testInstanceGen 8000 13000 21000 > testExample.yaml`.
   This creates an instance of a market with 8000 users, 13000 items, and 21000 transactions.
7. Try running the market with the test instance `./market testExample.yaml > testExampleOutput.yaml`.
8. To translate the Soda files into Scala and Lean, and then compile the Lean files, run
   `bash update.sh`.
9. You can edit the Soda files with [IntelliJ][intellij] and compile them with the `soda`
   binary.
10. You can see and verify the Lean translations with [Visual Studio Code][vscode]. The
   translated files are in directory [Soda][soda-lean-translation], and in particular the
   package [core][soda-lean-translation-core].
11. The Bash script [tester.sh][tester] tests the market with synthetic data. It requires to
   have `market` and `testInstanceGen` reachable in the path.


## How to learn Soda

Soda is a functional language intended to be **easy to learn and to read**. However, writing
purely functional style requires some practice, as some things are different from the
imperative style. In addition, Soda includes an object-oriented notation to align it with
mainstream object-oriented programming languages, and to make its notation familiar to users
acquainted to those languages. For more details about Soda, see the [Soda manual][soda-manual].

To get familiar with the verification possibilities, a good way to start proving theorems in
Lean is to follow the tutorials at the [Lean Game Server][lean-game-server], like the
**Natural Number Game**.


## Build

The project can be built with [sbt][sbt] with
`sbt clean compile test package assembly`

A Linux binary can be created with the script `makeall.sh`.

More detailed information can be found in the [release notes][release-notes].


## Author

[Julian Alfredo Mendez][author]

[author]: https://julianmendez.github.io
[license]: https://www.apache.org/licenses/LICENSE-2.0.txt
[build-status]: https://github.com/julianmendez/market/actions
[soda-manual]: https://soda-lang.readthedocs.io/en/latest/
[soda-release-notes]: https://julianmendez.github.io/soda/RELEASE-NOTES.html
[release-notes]: https://julianmendez.github.io/market/RELEASE-NOTES.html
[soda]: https://julianmendez.github.io/soda/
[soda-repo]: https://github.com/julianmendez/soda
[soda-releases]: https://github.com/julianmendez/soda/releases
[market-repo]: https://github.com/julianmendez/market
[market-releases]: https://github.com/julianmendez/market/releases
[examples-test]: https://github.com/julianmendez/soda/tree/master/examples/src/test/scala/soda/example
[lean-game-server]: https://adam.math.hhu.de
[sbt]: https://www.scala-sbt.org
[scala]: https://scala-lang.org
[java]: https://www.oracle.com/java/technologies/
[lean]: https://lean-lang.org
[sbt]: https://www.scala-sbt.org
[intellij]: https://www.jetbrains.com/idea/
[intellij-conf]: https://github.com/julianmendez/soda/blob/master/translator/src/main/resources/soda/translator/documentation/soda_for_intellij.txt
[vscode]: https://code.visualstudio.com
[soda-lean-translation]: https://github.com/julianmendez/market/tree/master/Soda
[soda-lean-translation-core]: https://github.com/julianmendez/market/tree/master/Soda/se/umu/cs/soda/prototype/example/market/core
[tester]: https://github.com/julianmendez/market/tree/development/measurement/src/main/bash/tester.sh


