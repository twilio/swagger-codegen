package dev.guardrail.generators.scala.zioHttp.server

import dev.guardrail.AuthImplementation.Native
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.{ AuthImplementation, Target }
import dev.guardrail.terms.server.SecurityExposure
import _root_.scala.meta._

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

  def renderClass(
      resourceName: String,
      handlerName: String,
      annotations: List[scala.meta.Mod.Annot],
      combinedRouteTerms: List[scala.meta.Stat], //handler methods like `def getOrderById(respond: StoreResource.GetOrderByIdResponse.type)(orderId: Long): Task[StoreResource.GetOrderByIdResponse]`
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
        routesParams    = List(param"impl: ${Type.Name(handlerName)}")
//        routesDefinition = q"""
//            def routes(..${routesParams}): Routes[Any, Response] = Routes (
//                ..${combinedRouteTerms}
//            )
//          """
      } yield {
        val clsName = Term.Name(resourceName)

        List(
          q"""
          object ${clsName} {
            import ${clsName}._

            ..${supportDefinitions};


            ..${securitySchemesDefinitions}

            ..${responseDefinitions}
          }"""
        )
      }
    )

}
