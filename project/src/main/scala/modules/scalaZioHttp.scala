package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._
import sbt._
import sbt.Keys._
import wartremover.WartRemover.autoImport._

object scalaZioHttp {
  val scalatestVersion = "3.2.18"

  val dependencies: Seq[ModuleID] = Seq(
    "dev.zio"       %% "zio-http"  % "3.0.0-RC8",
    "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    "org.scalameta" %% "scalameta" % "4.8.14"
  ).map(_.cross(CrossVersion.for3Use2_13))

  val project = commonModule("scala-zio-http")

  val sample =
    buildSampleProject("zioHttp", dependencies)
      .settings(Compile / compile / wartremoverWarnings --= Seq(Wart.NonUnitStatements, Wart.Throw))
}
