package dev.guardrail.sbt.modules

import sbt._
import sbt.Keys._

import dev.guardrail.sbt.Build._

object root {
  val project = Project("root", file("."))
    .settings(commonSettings)
    .settings(publish / skip := true)
    .settings(libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.13")
}
