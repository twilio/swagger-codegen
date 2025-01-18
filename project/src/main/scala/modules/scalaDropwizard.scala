package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object scalaDropwizard {
  val catsVersion            = "2.10.0"
  val dropwizardScalaVersion = "1.3.7-1"
  val dropwizardVersion      = "4.0.12"
  val jacksonVersion         = "2.16.0"
  val javaxAnnotationVersion = "1.3.2"
  val jaxbApiVersion         = "2.3.1"
  val jerseyVersion          = "2.25.1"
  val scalatestVersion       = "3.2.18"

  val dependencies = Seq(
    "javax.annotation"               %  "javax.annotation-api"    % javaxAnnotationVersion, // for jdk11
    "javax.xml.bind"                 %  "jaxb-api"                % jaxbApiVersion, // for jdk11
    "io.dropwizard"                  %  "dropwizard-core"         % dropwizardVersion,
    "io.dropwizard"                  %  "dropwizard-forms"        % dropwizardVersion,
    "com.fasterxml.jackson.datatype" %  "jackson-datatype-jsr310" % jacksonVersion,
    "junit"                          %  "junit"                   % "4.13.2"             % Test,
    "com.github.sbt"                   %  "junit-interface"         % "0.13.3"             % Test,
    "com.github.tomakehurst"         %  "wiremock"                % "2.27.2"           % Test,
    "io.dropwizard"                  %  "dropwizard-testing"      % dropwizardVersion  % Test,
    "org.glassfish.jersey.test-framework.providers" % "jersey-test-framework-provider-grizzly2" % jerseyVersion % Test,
  ) ++ Seq(
    "com.datasift.dropwizard.scala"  %% "dropwizard-scala-core"   % dropwizardScalaVersion,
    "com.fasterxml.jackson.module"   %% "jackson-module-scala"    % jacksonVersion,
    "org.typelevel"                  %% "cats-core"               % catsVersion,
    "org.scala-lang.modules"         %% "scala-java8-compat"      % "1.0.2"            % Test,
    "org.scalatest"                  %% "scalatest"               % scalatestVersion   % Test,
    "org.mockito"                    %% "mockito-scala-scalatest" % "1.17.30"           % Test,
  ).map(_.cross(CrossVersion.for3Use2_13))

  val project = commonModule("scala-dropwizard")

  val sample = buildSampleProject("dropwizardScala", dependencies).settings(scalacOptions -= "-Xfatal-warnings")
}
