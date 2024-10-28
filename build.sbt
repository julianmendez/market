import sbt.Keys.scalacOptions

lazy val scala3_5 = "3.5.2"

lazy val commonSettings =
  Seq(
    organization := "se.umu.cs.soda.prototype",
    version := "0.1.0-SNAPSHOT",
    description := "Prototype of a market modeled in Soda",
    homepage := Some(url("https://github.com/julianmendez/market")),
    startYear := Some(2024),
    licenses := Seq("Apache License Version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    organizationName := "Umea University",
    organizationHomepage := Some(url("https://www.umu.se/en/department-of-computing-science/")),
    developers := List(
      Developer("julianmendez", "Julian Alfredo Mendez", "julian.mendez@gmail.com", url("https://julianmendez.github.io"))
    ),

    /**
     * Scala
     * [[https://www.scala-lang.org]]
     * [[https://github.com/scala/scala]]
     * [[https://repo1.maven.org/maven2/org/scala-lang/scalap/]]
     * [[https://repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/]]
     */
    crossScalaVersions := Seq(scala3_5),
    scalaVersion := scala3_5,

    /**
     * ScalaTest
     * [[http://www.scalatest.org]]
     * [[https://github.com/scalatest/scalatest]]
     * [[https://repo1.maven.org/maven2/org/scalatest/]]
     */
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
    resolvers += Resolver.mavenLocal,
    publishTo := Some(Resolver.mavenLocal),
    publishMavenStyle := true,
    scalacOptions ++= Seq("-deprecation", "-feature")
  )

lazy val core =
  project
    .withId("core")
    .in(file("core"))
    .settings(
      commonSettings,
      /**
       * YAML 1.2 parser
       * [[https://bitbucket.org/asomov/snakeyaml-engine]]
       * [[https://repo1.maven.org/maven2/org/snakeyaml/snakeyaml-engine/]]
       */
      libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % "2.8",
      assembly / assemblyJarName := "core-" + version.value + ".jar"
    )

lazy val measurement =
  project
    .withId("measurement")
    .in(file("measurement"))
    .aggregate(core)
    .dependsOn(core)
    .settings(
      commonSettings,
      assembly / mainClass := Some("soda.se.umu.cs.soda.prototype.example.market.measurement.EntryPoint"),
      assembly / assemblyJarName := "measurement-" + version.value + ".jar"
    )

lazy val root =
  project
    .withId("market")
    .in(file("."))
    .aggregate(core, measurement)
    .dependsOn(core, measurement)
    .settings(
      commonSettings,
      assembly / mainClass := Some("soda.se.umu.cs.soda.prototype.example.market.main.EntryPoint"),
      assembly / assemblyJarName := "market-" + version.value + ".jar"
    )

