package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object scalaHttp4s {
  val catsVersion            = "2.10.0"
  val circeVersion           = "0.14.6"
  val javaxAnnotationVersion = "1.3.2"
  val jaxbApiVersion         = "2.3.1"
  val scalatestVersion       = "3.2.18"

  val dependenciesV0_22 = {
    val catsEffectVersion      = "2.5.4"
    val http4sVersion          = "0.22.7"
    val refinedVersion         = "0.11.0"

    Seq(
      "javax.annotation" %  "javax.annotation-api"  % javaxAnnotationVersion, // for jdk11
      "javax.xml.bind"   % "jaxb-api"               % jaxbApiVersion, // for jdk11
    ) ++ Seq(
      "eu.timepit"       %% "refined"               % refinedVersion,
      "eu.timepit"       %% "refined-cats"          % refinedVersion,
      "io.circe"         %% "circe-core"            % circeVersion,
      "io.circe"         %% "circe-parser"          % circeVersion,
      "io.circe"         %% "circe-refined"         % circeVersion,
      "org.http4s"       %% "http4s-blaze-client"   % http4sVersion,
      "org.http4s"       %% "http4s-blaze-server"   % http4sVersion,
      "org.http4s"       %% "http4s-circe"          % http4sVersion,
      "org.http4s"       %% "http4s-dsl"            % http4sVersion,
      "org.scalatest"    %% "scalatest"             % scalatestVersion % Test,
      "org.typelevel"    %% "cats-core"             % catsVersion,
      "org.typelevel"    %% "cats-effect"           % catsEffectVersion
    ).map(_.cross(CrossVersion.for3Use2_13))
  }

  val dependencies = {
    val catsEffectVersion      = "3.5.3"
    val http4sVersion          = "0.23.29"
    val http4sBlazeVersion     = "0.23.15"
    val refinedVersion         = "0.11.0"

    Seq(
      "javax.annotation" %  "javax.annotation-api"  % javaxAnnotationVersion, // for jdk11
      "javax.xml.bind"   % "jaxb-api"               % jaxbApiVersion, // for jdk11
    ) ++ Seq(
      "eu.timepit"       %% "refined"               % refinedVersion,
      "eu.timepit"       %% "refined-cats"          % refinedVersion,
      "io.circe"         %% "circe-core"            % circeVersion,
      "io.circe"         %% "circe-parser"          % circeVersion,
      "io.circe"         %% "circe-refined"         % circeVersion,
      "org.http4s"       %% "http4s-blaze-client"   % http4sBlazeVersion,
      "org.http4s"       %% "http4s-blaze-server"   % http4sBlazeVersion,
      "org.http4s"       %% "http4s-circe"          % http4sVersion,
      "org.http4s"       %% "http4s-dsl"            % http4sVersion,
      "org.scalatest"    %% "scalatest"             % scalatestVersion % Test,
      "org.typelevel"    %% "cats-core"             % catsVersion,
      "org.typelevel"    %% "cats-effect"           % catsEffectVersion
    ).map(_.cross(CrossVersion.for3Use2_13))
  }

  val project = commonModule("scala-http4s")

  val sampleV0_22 = buildSampleProject("http4s-v0_22", dependenciesV0_22)
  val sample = buildSampleProject("http4s", dependencies)
}
