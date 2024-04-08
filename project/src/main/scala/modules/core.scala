package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._
import dev.guardrail.sbt.interop.InteropKeys
import dev.guardrail.sbt.interop.InteropTasks

import sbt._
import sbt.Keys._

object core {
  val catsVersion = "2.10.0"

  // Generate POJOs with convenience methods for the following classes:
  def emitSources(sourceManagedPath: java.io.File): List[(scala.meta.Source, java.io.File)] = {
    import _root_.scala.meta._
    List(
      (
        InteropTasks.buildPOJO(
          t"dev.guardrail.Args",
          List(
            param"kind: dev.guardrail.CodegenTarget",
            param"specPath: Option[String]",
            param"outputPath: Option[String]",
            param"packageName: Option[List[String]]",
            param"dtoPackage: List[String]",
            param"printHelp: Boolean",
            param"context: Context",
            param"defaults: Boolean",
            param"imports: List[String]",
          ),
          List(
            q"""
              val empty: Args = new Args(CodegenTarget.Client, Option.empty, Option.empty, Option.empty, List.empty, false, Context.empty, false, List.empty)
            """,
            q"""
              def isEmpty: Args => Boolean = { args =>
                args.specPath.isEmpty &&
                args.outputPath.isEmpty &&
                args.packageName.isEmpty &&
                !args.printHelp
              }
            """
          ),
          List(
            q"""
              def modifyContext(func: Context => Context): Args =
                this.withContext(func(this.context))
            """,
            q"""
              def copyPropertyRequirement(
                  encoder: dev.guardrail.terms.protocol.PropertyRequirement.OptionalRequirement = this.context.propertyRequirement.encoder,
                  decoder: dev.guardrail.terms.protocol.PropertyRequirement.OptionalRequirement = this.context.propertyRequirement.decoder
              ): Args =
                modifyContext(ctx =>
                  ctx.withPropertyRequirement(
                    ctx.propertyRequirement.copy(
                      encoder = encoder,
                      decoder = decoder
                    )
                  )
                )
            """
          ),
        ),
        sourceManagedPath / "dev" / "guardrail" / "Args.scala"
      ),
      (
        InteropTasks.buildPOJO(
          t"dev.guardrail.Context",
          List(
            param"framework: Option[String]",
            param"customExtraction: Boolean",
            param"tracing: Boolean",
            param"modules: List[String]",
            param"propertyRequirement: dev.guardrail.terms.protocol.PropertyRequirement.Configured",
            param"tagsBehaviour: dev.guardrail.TagsBehaviour",
            param"authImplementation: dev.guardrail.AuthImplementation",
          ),
          List(
            q"""
              val empty: Context = Context(
                None,
                customExtraction = false,
                tracing = false,
                modules = List.empty,
                propertyRequirement = dev.guardrail.terms.protocol.PropertyRequirement.Configured(
                  dev.guardrail.terms.protocol.PropertyRequirement.OptionalLegacy,
                  dev.guardrail.terms.protocol.PropertyRequirement.OptionalLegacy
                ),
                tagsBehaviour = TagsBehaviour.TagsAreIgnored,
                authImplementation = AuthImplementation.Disable
              )
            """
          ),
          Nil,
        ),
        sourceManagedPath / "dev" / "guardrail" / "Context.scala"
      )
    )
  }

  val project =
    commonModule("core")
      .settings(
        libraryDependencies ++= Seq(
          "io.swagger.parser.v3"        % "swagger-parser"                % "2.1.22",
        ) ++ Seq(
          "org.scala-lang.modules"      %% "scala-collection-compat"      % "2.11.0",
          "org.tpolecat"                %% "atto-core"                    % "0.9.5",
          "org.typelevel"               %% "cats-core"                    % catsVersion,
          "org.typelevel"               %% "cats-kernel"                  % catsVersion,
          "org.scala-lang.modules"      %% "scala-java8-compat"           % "1.0.2",
        ).map(_.cross(CrossVersion.for3Use2_13)),
        Compile / sourceGenerators += InteropKeys.generateTask,
        InteropKeys.generateTask := {
          val sourceManagedPath = (Compile / sourceManaged).value
          InteropTasks.writeFiles(emitSources(sourceManagedPath)),
        }
      )
}
