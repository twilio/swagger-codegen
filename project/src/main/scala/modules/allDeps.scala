package dev.guardrail.sbt.modules

import sbt._
import sbt.Keys._

import dev.guardrail.sbt.Build._
import dev.guardrail.sbt.Dependencies._

object allDeps {
  val project = Project("allDeps", file("modules/alldeps"))
    .settings(commonSettings)
    .settings(
      publish / skip := true,
      libraryDependencies ++= scalaAkkaHttp.dependencies,
      libraryDependencies ++= scalaZioHttp.dependencies,
      libraryDependencies ++= scalaAkkaHttp.dependenciesJackson,
      libraryDependencies ++= scalaHttp4s.dependencies,
      libraryDependencies ++= javaSpringMvc.dependencies,
      libraryDependencies ++= javaDropwizard.dependencies,
      libraryDependencies ++= javaDropwizard.dependenciesVavr,
      libraryDependencies ++= scalaDropwizard.dependencies,
    )
}
