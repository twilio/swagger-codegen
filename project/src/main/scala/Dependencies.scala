package dev.guardrail.sbt

import sbt._
import sbt.Keys._

object Dependencies {
  val scalatestVersion       = "3.2.18"

  val testDependencies = Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    "org.scalacheck" %% "scalacheck" % "1.17.1" % Test,
    "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
  ).map(_.cross(CrossVersion.for3Use2_13))

}
