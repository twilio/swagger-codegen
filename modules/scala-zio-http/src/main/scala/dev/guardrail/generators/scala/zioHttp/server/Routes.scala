package dev.guardrail.generators.scala.zioHttp.server

import dev.guardrail.Target
import dev.guardrail.generators.LanguageParameter
import dev.guardrail.generators.scala.{ResponseADTHelper, ScalaLanguage}
import dev.guardrail.terms.{CollectionsLibTerms, LanguageTerms, OpenAPITerms, Responses, RouteMeta}
import dev.guardrail.terms.protocol.StrictProtocolElems
import dev.guardrail.terms.server.GenerateRouteMeta
import io.swagger.v3.oas.models.Components
import io.swagger.v3.oas.models.PathItem.HttpMethod

import _root_.scala.meta._
import cats.implicits._
import dev.guardrail.core.Tracker
import dev.guardrail.terms.framework.FrameworkTerms

object Routes {

  case class RenderedRoute(methodName: String,
                                   methodSig: Decl.Def,
                                   supportDefinitions: List[Defn],
                                   handlerDefinitions: List[Stat])

  case class ResponseDefinitionsForGeneratedRoute(definitions: List[Defn],
                                                  routeGenMeta: GenerateRouteMeta[ScalaLanguage])

  //todo
  //import zio._
  //import zio.http._

  def generateRouteMeta(route: RouteMeta)(
    protocolElems: List[StrictProtocolElems[ScalaLanguage]],
    components: Tracker[Option[Components]]
  )(implicit openApiTerms: OpenAPITerms[ScalaLanguage, Target],
    frameworkTerms: FrameworkTerms[ScalaLanguage, Target],
    languageTerms: LanguageTerms[ScalaLanguage, Target],
    collectionLibTerms: CollectionsLibTerms[ScalaLanguage, Target],
  ): Target[ResponseDefinitionsForGeneratedRoute] = {
    for {
      operationId <- openApiTerms.getOperationId(route.operation)
      responses   <- Responses.getResponses[ScalaLanguage, Target](
        operationId = operationId,
        operation = route.operation,
        protocolElems = protocolElems,
        components = components)
      responseClassName <- languageTerms.formatTypeName(operationId, Some("Response"))
      responseDefinitions <- Target.pure(
        ResponseADTHelper.generateResponseDefinitions(responseClassName, responses, protocolElems)
      )
      methodName   <- languageTerms.formatMethodName(operationId)
      parameters            <- route.getParameters[ScalaLanguage, Target](components, protocolElems)

    } yield ResponseDefinitionsForGeneratedRoute(
      responseDefinitions,
      GenerateRouteMeta(
        operationId = operationId,
        methodName = methodName,
        responseClsName = responseClassName,
        customExtractionField = None, //todo: implement
        tracingField = None, //todo: implement
        routeMeta = route,
        parameters = parameters,
        responses = responses)
    )

  }


  def generateRoute(resourceName: String, basePath: Option[String])
                   (meta: GenerateRouteMeta[ScalaLanguage]): Target[Option[RenderedRoute]] = {


    for {
      _ <- Target.log.debug(s"Args: ${resourceName}, ${basePath}, ${meta}")
      formArgs   <- prepareParameters(meta.parameters.formParams)
      headerArgs <- prepareParameters(meta.parameters.headerParams)
      pathArgs <- prepareParameters(meta.parameters.pathParams)
      qsArgs <- prepareParameters(meta.parameters.queryStringParams)
      zioHttpMethod <- httpMethodToZioHttp(meta.routeMeta.method)
    } yield {

      val responseType: Type = {
        t"Response[F]"
      }

      val params: List[List[Term.Param]] = {
//        (formArgs ++ headerArgs ++ pathArgs ++ qsArgs)
//          .map(_.param.decltpe)
        List.empty
      }


      val methodSignature: Decl.Def = q"""def ${Term.Name(meta.methodName)}(...${params}): F[$responseType]"""

      Option(
        RenderedRoute(
          methodName = meta.methodName,
          methodSig = methodSignature,
          supportDefinitions = List.empty,
          handlerDefinitions = List.empty
        )
      )
    }

  }

  private def httpMethodToZioHttp(method: HttpMethod) = method match {
    case HttpMethod.GET => Target.pure(Term.Name("Method.GET"))
    case HttpMethod.POST => Target.pure(Term.Name("Method.POST"))
    case other => Target.raiseUserError(s"Unknown method: ${other}")
  }


  /** It's not possible to use backticks inside pattern matching as it has different semantics: backticks inside match are just references to an already
   * existing bindings.
   */
  private def prepareParameters(parameters: List[LanguageParameter[ScalaLanguage]])
  : Target[List[LanguageParameter[ScalaLanguage]]] =
    if (parameters.exists(param => param.paramName.syntax != param.paramName.value)) {
      // let's try to prefix them all with underscore and see if it helps
      for {
        _ <- Target.log.debug("Found that not all parameters could be represented as unescaped terms")
        res <- parameters.traverse[Target, LanguageParameter[ScalaLanguage]] { param =>
          for {
            _ <- Target.log.debug(s"Escaping param ${param.argName.value}")
            newName = Term.Name(s"_${param.paramName.value}")
            res <-
              if (newName.syntax == newName.value) Target.pure(param.withParamName(newName))
              else Target.raiseUserError(s"Can't escape parameter with name ${param.argName.value}.")
          } yield res
        }
      } yield res
    } else {
      Target.pure(parameters)
    }


}
