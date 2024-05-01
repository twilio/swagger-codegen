package dev.guardrail.generators.scala.zioHttp.server

import dev.guardrail.AuthImplementation.Native
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.{AuthImplementation, Target}
import dev.guardrail.terms.server.SecurityExposure
import scala.meta._

object Handlers {

  def renderHandler(
      handlerName: String,
      methodSigs: List[scala.meta.Decl.Def],
      handlerDefinitions: List[scala.meta.Stat],
      responseDefinitions: List[scala.meta.Defn],
      customExtraction: Boolean,
      authImplementation: AuthImplementation,
      securityExposure: SecurityExposure
  ): Target[ScalaLanguage#Definition] =
    Target.log.function("renderHandler")(
      for {
        _ <- Target.log.debug(s"Args: ${handlerName}, ${methodSigs}")
        extractType = List.empty
        authType    = List.empty
      } yield q"""
        trait ${Type.Name(handlerName)} {
          ..${methodSigs ++ handlerDefinitions}
        }
      """
    )


  private def renderClass(
                           resourceName: String,
                           handlerName: String,
                           annotations: List[scala.meta.Mod.Annot],
                           combinedRouteTerms: List[scala.meta.Stat],
                           extraRouteParams: List[scala.meta.Term.Param],
                           responseDefinitions: List[scala.meta.Defn],
                           supportDefinitions: List[scala.meta.Defn],
                           securitySchemesDefinitions: List[scala.meta.Defn],
                           customExtraction: Boolean,
                           authImplementation: AuthImplementation
                         ): Target[List[Defn]] =
    Target.log.function("renderClass")(
      for {
        _ <- Target.log.debug(s"Args: ${resourceName}, ${handlerName}, <combinedRouteTerms>, ${extraRouteParams}")
        resourceTParams = List(tparam"F[_]")
        handlerTParams = List(Type.Name("F"))
        routesParams = List(param"handler: ${Type.Name(handlerName)}[..$handlerTParams]")
        routesDefinition =  q"""
            def routes(..${routesParams}): HttpRoutes[F] = HttpRoutes.of {
                ..${combinedRouteTerms}
              }
          """
      } yield List(
        q"""
          class ${Type.Name(resourceName)}[..$resourceTParams](..$extraRouteParams)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
            import ${Term.Name(resourceName)}._

            ..${supportDefinitions};
            $routesDefinition
          }
        """,
        q"""object ${Term.Name(resourceName)} {
            ..${securitySchemesDefinitions}

            ..${responseDefinitions}
        }"""
      )
    )


}
