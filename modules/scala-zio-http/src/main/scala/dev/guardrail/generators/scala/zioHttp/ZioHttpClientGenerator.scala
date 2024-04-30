package dev.guardrail.generators.scala.zioHttp

import _root_.io.swagger.v3.oas.models.Components
import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod
import cats.data.NonEmptyList
import cats.syntax.all._
import dev.guardrail.Target
import dev.guardrail._
import dev.guardrail.core.SupportDefinition
import dev.guardrail.core.Tracker
import dev.guardrail.generators.Client
import dev.guardrail.generators.Clients
import dev.guardrail.generators.LanguageParameter
import dev.guardrail.generators.LanguageParameters
import dev.guardrail.generators.RawParameterName
import dev.guardrail.generators.RenderedClientOperation
import dev.guardrail.generators.scala.{ResponseADTHelper, ScalaLanguage}
import dev.guardrail.generators.scala.syntax._
import dev.guardrail.generators.spi.ClientGeneratorLoader
import dev.guardrail.generators.spi.ModuleLoadResult
import dev.guardrail.generators.syntax._
import dev.guardrail.shims._
import dev.guardrail.terms.ApplicationJson
import dev.guardrail.terms.ContentType
import dev.guardrail.terms.Header
import dev.guardrail.terms.MultipartFormData
import dev.guardrail.terms.OctetStream
import dev.guardrail.terms.Responses
import dev.guardrail.terms.RouteMeta
import dev.guardrail.terms.SecurityScheme
import dev.guardrail.terms.TextPlain
import dev.guardrail.terms._
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.StaticDefns
import dev.guardrail.terms.protocol.StrictProtocolElems

import java.net.URI
import scala.collection.immutable.Seq
import scala.meta._
import scala.reflect.runtime.universe.typeTag

class ZioHttpClientGeneratorLoader extends ClientGeneratorLoader {
  type L = ScalaLanguage
  def reified = typeTag[Target[ScalaLanguage]]
  val apply   = ModuleLoadResult.forProduct1(ClientGeneratorLoader.label -> Seq(ZioHttpVersion.mapping))(version => ZioHttpClientGenerator(version))
}

object ZioHttpClientGenerator {
  def apply(): ClientTerms[ScalaLanguage, Target] =
    new ZioHttpClientGenerator(ZioHttpVersion)

  def apply(version: ZioHttpVersion): ClientTerms[ScalaLanguage, Target] =
    new ZioHttpClientGenerator(version)
}

class ZioHttpClientGenerator(version: ZioHttpVersion) extends ClientTerms[ScalaLanguage, Target] {
  def splitOperationParts(operationId: String): (List[String], String) = {
    val parts = operationId.split('.')
    (parts.drop(1).toList, parts.last)
  }

  private[this] def formatClientName(clientName: Option[String]): Term.Param =
    clientName.fold(
      param"clientName: String"
    )(name => param"clientName: String = ${Lit.String(name.toDashedCase)}")

  private[this] def formatHost(serverUrls: Option[NonEmptyList[URI]]): Term.Param =
    serverUrls
      .fold(param"host: String")(v => param"host: String = ${Lit.String(v.head.toString())}")

  override def fromSpec(context: Context, frameworkImports: List[ScalaLanguage#Import])(
    serverUrls: Option[NonEmptyList[URI]],
    basePath: Option[String],
    groupedRoutes: List[(List[String], List[RouteMeta])]
  )(
                         protocolElems: List[StrictProtocolElems[ScalaLanguage]],
                         securitySchemes: Map[String, SecurityScheme[ScalaLanguage]],
                         components: Tracker[Option[Components]]
                       )(implicit
                         Fw: FrameworkTerms[ScalaLanguage, Target],
                         Sc: LanguageTerms[ScalaLanguage, Target],
                         Cl: CollectionsLibTerms[ScalaLanguage, Target],
                         Sw: OpenAPITerms[ScalaLanguage, Target]
                       ): Target[Clients[ScalaLanguage]] = {
    import Sc._
    import Sw._

    for {
      clientImports      <- getImports(context.tracing)
      clientExtraImports <- getExtraImports(context.tracing)
      supportDefinitions <- generateSupportDefinitions(context.tracing, securitySchemes)
      clients <- groupedRoutes.traverse { case (className, unsortedRoutes) =>
        val routes = unsortedRoutes.sortBy(r => (r.path.unwrapTracker, r.method))
        for {
          clientName <- formatTypeName(className.lastOption.getOrElse(""), Some("Client"))
          responseClientPair <- routes.traverse { case route @ RouteMeta(path, method, operation, securityRequirements) =>
            for {
              operationId         <- getOperationId(operation)
              responses           <- Responses.getResponses[ScalaLanguage, Target](operationId, operation, protocolElems, components)
              responseClsName     <- formatTypeName(operationId, Some("Response"))
              responseDefinitions <- generateResponseDefinitions(responseClsName, responses, protocolElems)
              parameters          <- route.getParameters[ScalaLanguage, Target](components, protocolElems)
              methodName          <- formatMethodName(operationId)
              clientOp <- generateClientOperation(className, responseClsName, context.tracing, securitySchemes, parameters)(route, methodName, responses)
            } yield (responseDefinitions, clientOp)
          }
          (responseDefinitions, clientOperations) = responseClientPair.unzip
          tracingName                             = Option(className.mkString("-")).filterNot(_.isEmpty)
          ctorArgs    <- clientClsArgs(tracingName, serverUrls, context.tracing)
          staticDefns <- buildStaticDefns(clientName, tracingName, serverUrls, ctorArgs, context.tracing)
          client <- buildClient(
            clientName,
            tracingName,
            serverUrls,
            basePath,
            ctorArgs,
            clientOperations.map(_.clientOperation),
            clientOperations.flatMap(_.supportDefinitions),
            context.tracing
          )
        } yield Client[ScalaLanguage](
          className,
          clientName,
          clientImports ++ frameworkImports ++ clientExtraImports,
          staticDefns,
          client,
          responseDefinitions.flatten
        )
      }
    } yield Clients[ScalaLanguage](clients, supportDefinitions)
  }

  def generateClientOperation(
                               className: List[String],
                               responseClsName: String,
                               tracing: Boolean,
                               securitySchemes: Map[String, SecurityScheme[ScalaLanguage]],
                               parameters: LanguageParameters[ScalaLanguage]
                             )(
                               route: RouteMeta,
                               methodName: String,
                               responses: Responses[ScalaLanguage]
                             ): Target[RenderedClientOperation[ScalaLanguage]] = {
    val RouteMeta(pathStr, httpMethod, operation, securityRequirements) = route
    val containerTransformations = Map[String, Term => Term](
      "Iterable"   -> identity _,
      "List"       -> (term => q"$term.toList"),
      "Vector"     -> (term => q"$term.toVector"),
      "Seq"        -> (term => q"$term.toSeq"),
      "IndexedSeq" -> (term => q"$term.toIndexedSeq")
    )

    def generateUrlWithParams(
                               path: Tracker[String],
                               pathArgs: List[LanguageParameter[ScalaLanguage]],
                               qsArgs: List[LanguageParameter[ScalaLanguage]]
                             ): Target[Term] =
      Target.log.function("generateUrlWithParams") {
        for {
          _    <- Target.log.debug(s"Using ${path.unwrapTracker} and ${pathArgs.map(_.argName)}")
          base <- generateUrlPathParams(path, pathArgs)

          _ <- Target.log.debug(s"QS: ${qsArgs}")

          suffix =
            if (path.unwrapTracker.contains("?")) {
              Lit.String("&")
            } else {
              Lit.String("?")
            }

          _ <- Target.log.debug(s"QS: ${qsArgs}")

          result = NonEmptyList
            .fromList(qsArgs.toList)
            .fold(base) {
              _.foldLeft[Term](q"${base} + ${suffix}") { case (a, LanguageParameter(_, _, paramName, argName, _)) =>
                q""" $a + Formatter.addArg(${Lit
                  .String(argName.value)}, ${paramName})"""
              }
            }
        } yield q"URL.fromURI(java.net.URI.create(${result})).get"
      }

    def generateFormDataParams(parameters: List[LanguageParameter[ScalaLanguage]], consumes: List[ContentType]): Option[Term] =
      if (parameters.isEmpty) {
        None
      } else if (consumes.exists(ContentType.isSubtypeOf[MultipartFormData])) {
        def liftOptionFileTerm(tParamName: Term, tName: RawParameterName) =
          q"$tParamName.map(v => Part.fileData[F](${tName.toLit}, v._1, v._2))"

        def liftFileTerm(tParamName: Term, tName: RawParameterName) =
          q"Some(Part.fileData[F](${tName.toLit}, ${tParamName}._1, ${tParamName}._2))"

        def liftOptionTerm(tParamName: Term, tName: RawParameterName) =
          q"$tParamName.map(v => Part.formData[F](${tName.toLit}, Formatter.show(v)))"

        def liftTerm(tParamName: Term, tName: RawParameterName) =
          q"Some(Part.formData[F](${tName.toLit}, Formatter.show($tParamName)))"

        val lifter: Term.Param => (Term, RawParameterName) => Term = {
          case param"$_: Option[fs2.Stream[F,Byte]]" =>
            liftOptionFileTerm _
          case param"$_: Option[fs2.Stream[F,Byte]] = $_" =>
            liftOptionFileTerm _
          case param"$_: fs2.Stream[F,Byte]"      => liftFileTerm _
          case param"$_: fs2.Stream[F,Byte] = $_" => liftFileTerm _
          case param"$_: Option[$_]"              => liftOptionTerm _
          case param"$_: Option[$_] = $_"         => liftOptionTerm _
          case _                                  => liftTerm _
        }

        val args: List[Term] = parameters.map { case LanguageParameter(_, param, paramName, argName, _) =>
          lifter(param)(paramName, argName)
        }
        Some(q"List(..$args)")
      } else {
        def liftTerm(tParamName: Term, tName: RawParameterName) =
          q"List((${tName.toLit}, Formatter.show($tParamName)))"

        def liftIterable(tParamName: Term, tName: RawParameterName) =
          q"$tParamName.toList.map(x => (${tName.toLit}, Formatter.show(x)))"

        def liftOptionTerm(tpe: Type)(tParamName: Term, tName: RawParameterName) = {
          val lifter = tpe match {
            case t"$container[$_]" if containerTransformations.contains(container.syntax) => liftIterable _
            case _                                                                        => liftTerm _
          }
          q"${tParamName}.toList.flatMap(${Term.Block(List(q" x => ${lifter(Term.Name("x"), tName)}"))})"
        }

        val lifter: Term.Param => (Term, RawParameterName) => Term = {
          case param"$_: Option[$tpe]"                                                                 => liftOptionTerm(tpe) _
          case param"$_: Option[$tpe] = $_"                                                            => liftOptionTerm(tpe) _
          case param"$_: $container[$tpe]" if containerTransformations.contains(container.syntax)      => liftIterable _
          case param"$_: $container[$tpe] = $_" if containerTransformations.contains(container.syntax) => liftIterable _
          case _                                                                                       => liftTerm _
        }

        val args: List[Term] = parameters.map { case LanguageParameter(_, param, paramName, argName, _) =>
          lifter(param)(paramName, argName)
        }
        Some(q"List(..$args)")
      }

    def generateHeaderParams(parameters: List[LanguageParameter[ScalaLanguage]]): Term = {
      def liftOptionTerm(tParamName: Term.Name, tName: RawParameterName) =
        q"$tParamName.map(v => (${tName.toLit}, Formatter.show(v)))"

      def liftTerm(tParamName: Term.Name, tName: RawParameterName) =
        q"Some((${tName.toLit}, Formatter.show($tParamName)))"

      val lifter: Term.Param => (Term.Name, RawParameterName) => Term = {
        case param"$_: Option[$_]"      => liftOptionTerm _
        case param"$_: Option[$_] = $_" => liftOptionTerm _
        case _                          => liftTerm _
      }

      val args: List[Term] = parameters.map { case LanguageParameter(_, param, paramName, argName, _) =>
        lifter(param)(paramName, argName)
      }
      q"Headers(List[Option[Header]](..$args).flatten)"
    }

    def contentTypeToMediaTypeTerm(ct: ContentType): Option[Term] = {
      val rawHeader = ct match {
        case applicationJson: ApplicationJson =>
          Some(q"""MediaType("application", "json")""")
        case textPlain: TextPlain =>
          Some(q"""MediaType("text", "plain")""")
        case octetStream: OctetStream =>
          Some(q"""MediaType("application", "octet-stream")""")
        case other =>
          None
      }

      rawHeader.map(h => q"Header.Accept.MediaTypeWithQFactor($h, Some(1.0))")
    }

    def build(
               methodName: String,
               responseClsName: String,
               httpMethod: HttpMethod,
               urlWithParams: Term,
               formDataParams: Option[Term],
               headerParams: Term,
               responses: Responses[ScalaLanguage],
               produces: Seq[ContentType],
               consumes: Seq[ContentType],
               tracing: Boolean
             )(
               tracingArgsPre: List[LanguageParameter[ScalaLanguage]],
               tracingArgsPost: List[LanguageParameter[ScalaLanguage]],
               pathArgs: List[LanguageParameter[ScalaLanguage]],
               qsArgs: List[LanguageParameter[ScalaLanguage]],
               formArgs: List[LanguageParameter[ScalaLanguage]],
               body: Option[LanguageParameter[ScalaLanguage]],
               headerArgs: List[LanguageParameter[ScalaLanguage]],
               extraImplicits: List[Term.Param]
             ): Target[RenderedClientOperation[ScalaLanguage]] =
      for {
        _ <- Target.pure(())
        acceptHeader =
          NonEmptyList.fromList(produces.flatMap(contentTypeToMediaTypeTerm).toList).fold[Term](q"Headers.empty") { case NonEmptyList(first, rest) =>
            q"Headers(Header.Accept(${first}, ..${rest}))"
          }
        implicitParams                 = Option(extraImplicits).filter(_.nonEmpty)
        defaultHeaders                 = param"headers: Headers = Headers.empty"
        safeBody: Option[(Term, Type)] = body.map(sp => (sp.paramName, sp.argType))

        formDataNeedsMultipart = consumes.exists(ContentType.isSubtypeOf[MultipartFormData])
        formEntity: Option[Term] = formDataParams.map { formDataParams =>
          if (formDataNeedsMultipart) {
            q"""_multipart"""
          } else {
            q"""UrlForm($formDataParams.flatten: _*)"""
          }
        }

        (tracingExpr, httpClientName) =
          if (tracing)
            (List(q"""val tracingHttpClient = traceBuilder(s"$${clientName}:$${methodName}")(httpClient)"""), q"tracingHttpClient")
          else
            (List(), q"httpClient")
        embedMultipart =
          formDataParams
            .filter(_ => formDataNeedsMultipart)
            .map(formDataParams =>
              (inner: List[Stat]) => q"""Multiparts.forSync[F].flatMap(_.multipart($formDataParams.flatten.toVector).flatMap { _multipart => ..${inner} })"""
            )
            .getOrElse[List[Stat] => Term](Term.Block(_))
        headersExpr =
          if (formDataNeedsMultipart) {
            List(
              q"val allHeaders: Headers = $acceptHeader ++ headers ++ $headerParams ++ _multipart.headers.headers.map(Header.ToRaw.rawToRaw)"
            )
          } else {
            List(q"val allHeaders: Headers = $acceptHeader ++ headers ++ $headerParams")
          }
        methodExpr = q"Method.${Term.Name(httpMethod.toString.toUpperCase)}"

        bodyBinding = q"_body"
        _body = formEntity
          .map(e => q"$e")
          .orElse(safeBody.map { case (name, tpe) => q"Body.fromString(io.circe.Encoder[$tpe].apply($name).noSpaces)" })
          .getOrElse(q"Body.empty")
        reqBinding = q"req"
        req =
          q"Request(body = ${bodyBinding}, headers = allHeaders, method = ${methodExpr}, url = ${urlWithParams}, version = Version.`HTTP/1.1`, remoteAddress = None)"
        reqExpr = List(
          q"val ${Pat.Var(bodyBinding)} = ${_body}",
          q"val ${Pat.Var(reqBinding)} = $req"
        )

        buildHeaders = (_: List[Header[ScalaLanguage]])
          .map { header =>
            val headerName = Lit.String(header.name.toLowerCase)
            if (header.isRequired) {
              q"parseRequiredHeader(resp, $headerName)"
            } else {
              q"parseOptionalHeader(resp, $headerName)"
            }
          }
        responseCompanionTerm = Term.Name(responseClsName)
        isGeneric             = ResponseADTHelper.isDefinitionGeneric(responses)
        baseResponseTypeRef   = Type.Name(responseClsName)
        cases <- responses.value.traverse[Target, Case] { resp =>
          val responseTerm = Term.Name(s"${resp.statusCodeName.value}")
          val statusCode   = Term.Select(p"_root_.zio.http.Status", resp.statusCodeName)
          (resp.value, resp.headers.value) match {
            case (None, Nil) =>
              Target.pure(p"case ${statusCode} => ZIO.succeed($responseCompanionTerm.$responseTerm): UIO[$baseResponseTypeRef]")
            case (maybeBody, headers) =>
              if (maybeBody.size + headers.size > 22) {
                // we have hit case class limitation
                // https://github.com/guardrail-dev/guardrail/pull/382
                Target.raiseUserError(
                  s"Failed to generate client for method $methodName and status code ${resp.statusCode}. It's currently not possible to have more than 22 properties (payload, HTTP headers). See https://github.com/guardrail-dev/guardrail/pull/382."
                )
              } else {
                val decodeValue = maybeBody.map { _ =>
                  q"${Term.Name(s"$methodName${resp.statusCodeName}Decoder")}.decode(resp, strict = false).value.flatMap(F.fromEither)"
                }
                val decodeHeaders = buildHeaders(headers)
                val mapArgs       = decodeValue.toList ++ decodeHeaders
                val mapTerm       = if (mapArgs.size == 1) q"map" else Term.Name(s"map${mapArgs.size}")

                if (isGeneric)
                  Target.pure(p"case $statusCode => F.$mapTerm(..$mapArgs)($responseCompanionTerm.$responseTerm.apply): UIO[$baseResponseTypeRef[F]]")
                else
                  Target.pure(p"case $statusCode => F.$mapTerm(..$mapArgs)($responseCompanionTerm.$responseTerm.apply): UIO[$baseResponseTypeRef]")

              }
          }
        }
        // Get the response type
        unexpectedCase  = p"""case _ => ZIO.fail(new Exception("Unexpected response code"))"""
        responseTypeRef = t"UIO[$baseResponseTypeRef]"
        executeReqExpr  =
          List(q"""$httpClientName.request(${reqBinding}).flatMap(resp => ${Term.Match(q"resp.status", cases :+ unexpectedCase, Nil)}).orDie""")
        methodBody: Term = embedMultipart(tracingExpr ++ headersExpr ++ reqExpr ++ executeReqExpr)

        formParams = formArgs.map(scalaParam =>
          scalaParam.param.copy(
            decltpe = if (scalaParam.isFile) {
              if (scalaParam.required) {
                Some(t"(String, Stream[F, Byte])")
              } else {
                Some(t"Option[(String, Stream[F, Byte])]")
              }
            } else {
              scalaParam.param.decltpe
            }
          )
        )

        arglists: List[List[Term.Param]] = List(
          Some(
            (tracingArgsPre.map(_.param) ++ pathArgs.map(_.param) ++ qsArgs
              .map(_.param) ++ formParams ++ body
              .map(_.param) ++ headerArgs.map(_.param) ++ tracingArgsPost
              .map(_.param)) :+ defaultHeaders
          ),
          implicitParams
        ).flatten
      } yield RenderedClientOperation[ScalaLanguage](
        q"""
              def ${Term
          .Name(methodName)}(...${arglists}): $responseTypeRef = $methodBody
            """,
        generateCodecs(methodName, body, responses, produces, consumes)
      )

    Target.log.function("generateClientOperation")(for {
      // Placeholder for when more functions get logging
      _ <- Target.pure(())

      consumes = operation.unwrapTracker.consumes.toList.flatMap(ContentType.unapply(_))
      produces = operation.unwrapTracker.produces.toList.flatMap(ContentType.unapply(_))

      headerArgs = parameters.headerParams
      pathArgs   = parameters.pathParams
      qsArgs     = parameters.queryStringParams
      bodyArgs   = parameters.bodyParams
      formArgs   = parameters.formParams

      _ <- Target.log.debug(s"pathArgs: ${pathArgs}")

      // Generate the url with path, query parameters
      urlWithParams <- generateUrlWithParams(pathStr, pathArgs, qsArgs)

      _ <- Target.log.debug(s"Generated: ${urlWithParams}")
      // Generate FormData arguments
      formDataParams = generateFormDataParams(formArgs, consumes)
      // Generate header arguments
      headerParams = generateHeaderParams(headerArgs)

      tracingArgsPre =
        if (tracing)
          List(LanguageParameter.fromParam(param"traceBuilder: TraceBuilder[F]"))
        else List.empty
      tracingArgsPost =
        if (tracing)
          List(LanguageParameter.fromParam(param"methodName: String = ${Lit.String(methodName.toDashedCase)}"))
        else List.empty
      extraImplicits = List.empty

      renderedClientOperation <- build(
        methodName,
        responseClsName,
        httpMethod,
        urlWithParams,
        formDataParams,
        headerParams,
        responses,
        produces,
        consumes,
        tracing
      )(
        tracingArgsPre,
        tracingArgsPost,
        pathArgs,
        qsArgs,
        formArgs,
        bodyArgs,
        headerArgs,
        extraImplicits
      )
    } yield renderedClientOperation)
  }
  def getImports(tracing: Boolean): Target[List[scala.meta.Import]]      = Target.pure(List.empty)
  def getExtraImports(tracing: Boolean): Target[List[scala.meta.Import]] = Target.pure(List.empty)
  def clientClsArgs(tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean): Target[List[Term.ParamClause]] = {
    val ihc = param"httpClient: Client"
    Target.pure(
      List(
        Term.ParamClause(
          List(formatHost(serverUrls)) ++ (if (tracing)
            Some(formatClientName(tracingName))
          else None),
          None
        ),
        Term.ParamClause(List(ihc), Some(Mod.Implicit()))
      )
    )
  }
  def generateResponseDefinitions(
                                   responseClsName: String,
                                   responses: Responses[ScalaLanguage],
                                   protocolElems: List[StrictProtocolElems[ScalaLanguage]]
                                 ): Target[List[scala.meta.Defn]] =
    Target.pure(ResponseADTHelper.generateResponseDefinitions(responseClsName, responses, protocolElems))

  def generateSupportDefinitions(
                                  tracing: Boolean,
                                  securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
                                ): Target[List[SupportDefinition[ScalaLanguage]]] = Target.pure(List.empty)

  def buildStaticDefns(
                        clientName: String,
                        tracingName: Option[String],
                        serverUrls: Option[NonEmptyList[URI]],
                        ctorArgs: List[Term.ParamClause],
                        tracing: Boolean
                      ): Target[StaticDefns[ScalaLanguage]] = {
    def extraConstructors(
                           tracingName: Option[String],
                           serverUrls: Option[NonEmptyList[URI]],
                           tpe: Type.Name,
                           ctorCall: Term.New,
                           tracing: Boolean
                         ): List[Defn] = {
      val tracingParams: List[Term.Param] = if (tracing) {
        List(formatClientName(tracingName))
      } else {
        List.empty
      }

      List(
        q"""
            def httpClient(httpClient: Client, ${formatHost(serverUrls)}, ..${tracingParams}): ${tpe} = ${ctorCall}
          """
      )
    }

    def paramsToArgs(params: List[Term.ParamClause]): List[Term.ArgClause] =
      params.map { params =>
        Term.ArgClause(
          params
            .map(_.name.value)
            .map(v => Term.Assign(Term.Name(v), Term.Name(v)))
            .toList
        )
      }.toList

    val ctorCall: Term.New = q"""new ${Type.Name(clientName)}(...${paramsToArgs(ctorArgs)})"""

    val decls: List[Defn] = q"""def apply(...${ctorArgs}): ${Type.Name(clientName)} = ${ctorCall}""" +:
      extraConstructors(tracingName, serverUrls, Type.Name(clientName), ctorCall, tracing)
    Target.pure(
      StaticDefns[ScalaLanguage](
        className = clientName,
        extraImports = List.empty,
        definitions = decls,
        statements = List.empty
      )
    )
  }
  def buildClient(
                   clientName: String,
                   tracingName: Option[String],
                   serverUrls: Option[NonEmptyList[URI]],
                   basePath: Option[String],
                   ctorArgs: List[Term.ParamClause],
                   clientCalls: List[scala.meta.Defn],
                   supportDefinitions: List[scala.meta.Defn],
                   tracing: Boolean
                 ): Target[NonEmptyList[Either[scala.meta.Defn.Trait, scala.meta.Defn.Class]]] = {
    val client =
      q"""
          class ${Type.Name(clientName)}(...${ctorArgs}) {
            val basePath: String = ${Lit.String(basePath.getOrElse(""))}

            // private def parseOptionalHeader(response: Response[F], header: String): F[Option[String]] =
            //   F.pure(response.headers.get(CIString(header)).map(_.head.value))
            //
            // private def parseRequiredHeader(response: Response[F], header: String): F[String] =
            //   response.headers
            //     .get(CIString(header))
            //     .map(_.head.value)
            //     .fold[F[String]](
            //       F.raiseError(
            //         ParseFailure(
            //           "Missing required header.",
            //           s"HTTP header '$$header' is not present."
            //         )
            //       )
            //     )(F.pure)

            ..${supportDefinitions};
            ..$clientCalls
          }
        """
    Target.pure(NonEmptyList(Right(client), Nil))
  }

  def generateCodecs(
                      methodName: String,
                      bodyArgs: Option[LanguageParameter[ScalaLanguage]],
                      responses: Responses[ScalaLanguage],
                      produces: Seq[ContentType],
                      consumes: Seq[ContentType]
                    ): List[Defn.Val] =
    // generateEncoders(methodName, bodyArgs, consumes) ++ generateDecoders(methodName, responses, produces)
    List.empty

  def generateEncoders(methodName: String, bodyArgs: Option[LanguageParameter[ScalaLanguage]], consumes: Seq[ContentType]): List[Defn.Val] = {
//    bodyArgs.toList.flatMap { case LanguageParameter(_, _, _, _, argType) =>
//      List(q"private[this] val ${Pat.Var(Term.Name(s"${methodName}Encoder"))} = ${ResponseADTHelper.generateEncoder(argType, consumes)}")
//    }
    List.empty
  }

  def generateDecoders(methodName: String, responses: Responses[ScalaLanguage], produces: Seq[ContentType]): List[Defn.Val] = {
//    for {
//      resp <- responses.value
//      tpe  <- resp.value.map(_._2)
//    } yield {
//      val contentTypes = resp.value.map(_._1).map(List(_)).getOrElse(produces) // for OpenAPI 3.x we should take ContentType from the response
//      q"private[this] val ${Pat.Var(Term.Name(s"$methodName${resp.statusCodeName}Decoder"))} = ${ResponseADTHelper.generateDecoder(tpe, contentTypes)}"
//    }
    List.empty
  }
}