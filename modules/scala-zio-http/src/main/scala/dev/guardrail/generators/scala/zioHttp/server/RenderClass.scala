package dev.guardrail.generators.scala.zioHttp.server

import dev.guardrail.AuthImplementation.Native

import _root_.scala.meta.Defn
import _root_.scala.meta.Mod.Annot
import _root_.scala.meta.Term.Param
import _root_.scala.meta.Stat
import _root_.scala.meta.Type

object RenderClass {

//  def renderClass(
//                           resourceName: String,
//                           handlerName: String,
//                           annotations: List[Annot],
//                           combinedRouteTerms: List[Stat],
//                           extraRouteParams: List[Param],
//                           responseDefinitions: List[Defn],
//                           supportDefinitions: List[Defn],
//                           securitySchemesDefinitions: List[Defn],
//                           customExtraction: Boolean,
//                           authImplementation: AuthImplementation
//                         ): Target[List[Defn]] =
//    Target.log.function("renderClass")(
//      for {
//        _ <- Target.log.debug(s"Args: ${resourceName}, ${handlerName}, <combinedRouteTerms>, ${extraRouteParams}")
//        extractType     = List(customExtractionTypeName).map(x => tparam"$x").filter(_ => customExtraction)
//        authType        = List(tparam"$authContextTypeName").filter(_ => securitySchemesDefinitions.nonEmpty || authImplementation == AuthImplementation.Native)
//        resourceTParams = List(tparam"F[_]") ++ extractType ++ authType
//        handlerTParams = List(Type.Name("F")) ++
//          List(customExtractionTypeName).filter(_ => customExtraction) ++
//          List(authContextTypeName).filter(_ => securitySchemesDefinitions.nonEmpty || authImplementation == AuthImplementation.Native)
//        routesParams = List(param"handler: ${Type.Name(handlerName)}[..$handlerTParams]")
//        routesDefinition = authImplementation match {
//          case Native => q"""
//            def routes(..${routesParams}): AuthedRoutes[$authContextTypeName, F] = AuthedRoutes.of {
//                ..${combinedRouteTerms}
//              }
//          """
//          case _ => q"""
//            def routes(..${routesParams}): HttpRoutes[F] = HttpRoutes.of {
//                ..${combinedRouteTerms}
//              }
//          """
//        }
//      } yield List(
//        q"""
//          class ${Type.Name(resourceName)}[..$resourceTParams](..$extraRouteParams)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
//            import ${Term.Name(resourceName)}._
//
//            ..${supportDefinitions};
//            $routesDefinition
//          }
//        """,
//        q"""object ${Term.Name(resourceName)} {
//            ..${securitySchemesDefinitions}
//
//            ..${responseDefinitions}
//        }"""
//      )
//    )

}
