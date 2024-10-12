package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object scalaSupport {
  val project =
    commonModule("scala-support")
      .settings(
        libraryDependencies += "org.scalameta" %% "scalameta" % "4.10.2"
      )
}
