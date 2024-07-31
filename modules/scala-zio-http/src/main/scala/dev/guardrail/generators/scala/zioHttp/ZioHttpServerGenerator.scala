package dev.guardrail.generators.scala.zioHttp

import _root_.io.swagger.v3.oas.models.Components
import cats.data.NonEmptyList
import dev.guardrail._
import dev.guardrail.core.Tracker
import dev.guardrail.generators.{Server, Servers}
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.spi.{ModuleLoadResult, ServerGeneratorLoader}
import dev.guardrail.terms._
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol._
import dev.guardrail.terms.server._
import _root_.scala.meta._
import cats.implicits._
import dev.guardrail.generators.scala.zioHttp.server.{Handlers, RenderClass, Routes}

class ZioHttpServerGeneratorLoader extends ServerGeneratorLoader {
  type L = ScalaLanguage
  override def reified = scala.reflect.runtime.universe.typeTag[Target[ScalaLanguage]]
  val apply =
    ModuleLoadResult.forProduct1(
      ServerGeneratorLoader.label -> Seq(ZioHttpVersion.mapping)
    ) { (zioHttpVersion) =>
      ZioHttpServerGenerator(zioHttpVersion)
    }
}

object ZioHttpServerGenerator {
  def apply(version: ZioHttpVersion): ServerTerms[ScalaLanguage, Target] =
    new ZioHttpServerGenerator(version)

  def getExtraImports() =
    Target.pure(
      List(
        q"import zio._",
        q"import zio.http._",
      )
    )

}

class ZioHttpServerGenerator(zioHttpVersion: ZioHttpVersion) extends ServerTerms[ScalaLanguage, Target] {

  override def fromSpec(context: Context, supportPackage: NonEmptyList[String], basePath: Option[String], frameworkImports: List[ScalaLanguage#Import])(
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(
      protocolElems: List[StrictProtocolElems[ScalaLanguage]],
      securitySchemes: Map[String, SecurityScheme[ScalaLanguage]],
      components: Tracker[Option[Components]]
  )(implicit
      Fw: FrameworkTerms[ScalaLanguage, Target],
    languageTerms: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target]
  ): Target[Servers[ScalaLanguage]] = {

    for {
      _ <- ZioHttpServerGenerator.getExtraImports()
      servers <- groupedRoutes.traverse { case (className, unsortedRoutes) =>
        val routes = unsortedRoutes
          .groupBy(_.path.unwrapTracker.indexOf('{'))
          .view
          .mapValues(_.sortBy(r => (r.path.unwrapTracker, r.method)))
          .toList
          .sortBy(_._1)
          .flatMap(_._2)

        for {
          resourceName <- languageTerms.formatTypeName(className.lastOption.getOrElse(""), Some("Resource"))
          handlerName  <- languageTerms.formatTypeName(className.lastOption.getOrElse(""), Some("Handler"))

          responseServerPair <- routes.traverse(meta => Routes.generateRouteMeta(meta)(protocolElems, components))

          responseDefinitions = responseServerPair.map(_.definitions)
          serverOperations = responseServerPair.map(_.routeGenMeta)

          securityExposure = serverOperations.flatMap(_.routeMeta.securityRequirements) match {
            case Nil => SecurityExposure.Undefined
            case xs  => if (xs.exists(_.optional)) SecurityExposure.Optional else SecurityExposure.Required
          }
          renderedRoutes <- Routes.generateRoutes(
            tracing = context.tracing,
            resourceName = resourceName,
            handlerName = handlerName,
            basePath = basePath,
            routes = serverOperations,
            protocolElems = protocolElems,
            securitySchemes = securitySchemes,
            securityExposure = securityExposure,
            authImplementation = context.authImplementation
          )
          handlerSrc <- Handlers.renderHandler(
            handlerName = handlerName,
            methodSigs = renderedRoutes.methodSigs,
            handlerDefinitions = renderedRoutes.handlerDefinitions,
            responseDefinitions = responseDefinitions.flatten,
            customExtraction = context.customExtraction,
            authImplementation = context.authImplementation,
            securityExposure = securityExposure
          )
//          extraRouteParams <- getExtraRouteParams(
//            resourceName,
//            context.customExtraction,
//            context.tracing,
//            context.authImplementation,
//            securityExposure
//          )
          classSrc <- Handlers.renderClass(
            resourceName = resourceName,
            handlerName = handlerName,
            annotations = renderedRoutes.classAnnotations,
            combinedRouteTerms = renderedRoutes.routes,
            extraRouteParams = List.empty,
            responseDefinitions = responseDefinitions.flatten,
            supportDefinitions = renderedRoutes.supportDefinitions,
            securitySchemesDefinitions = renderedRoutes.securitySchemesDefinitions,
            customExtraction = context.customExtraction,
            authImplementation = context.authImplementation
          )
        } yield Server[ScalaLanguage](
          className, frameworkImports,
          handlerDefinition = handlerSrc,
          serverDefinitions = classSrc
        )

      }
    } yield Servers[ScalaLanguage](servers, List.empty)

  }

}
