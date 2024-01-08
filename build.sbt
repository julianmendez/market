import sbt.Keys.scalacOptions

lazy val scala2_13 = "2.13.12"

lazy val scala3_3 = "3.3.1"

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
      Developer("julianmendez", "Julian Alfredo Mendez", "julian.mendez@gmail.com", new URL
      ("https://julianmendez.github.io"))
    ),

    /**
     * Scala
     * [[https://www.scala-lang.org]]
     * [[https://github.com/scala/scala]]
     * [[https://repo1.maven.org/maven2/org/scala-lang/scalap/]]
     * [[https://repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/]]
     */
    crossScalaVersions := Seq(scala2_13, scala3_3),
    scalaVersion := scala3_3,

    /**
     * ScalaTest
     * [[http://www.scalatest.org]]
     * [[https://github.com/scalatest/scalatest]]
     * [[https://repo1.maven.org/maven2/org/scalatest/]]
     */
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test",
    resolvers += Resolver.mavenLocal,
    publishTo := Some(Resolver.mavenLocal),
    publishMavenStyle := true,
    scalacOptions ++= Seq("-deprecation", "-feature")
  )

lazy val marketCore =
  project
    .withId("market-core")
    .in(file("market-core"))
    .settings(
      commonSettings
    )


lazy val root =
  project
    .withId("market")
    .in(file("."))
    .aggregate(marketCore)
    .dependsOn(marketCore)
    .settings(
      commonSettings,
      assembly / mainClass := Some("soda.se.umu.cs.soda.prototype.example.market.main.EntryPoint"),
      assembly / assemblyJarName := "market-" + version.value + ".jar"
    )

