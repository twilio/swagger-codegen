package com.twilio.guardrail

import _root_.io.swagger.models._
import _root_.io.swagger.models.parameters._
import _root_.io.swagger.models.properties._
import cats.{ FlatMap, Foldable, MonadError }
import cats.data.{ EitherK, EitherT }
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.terms.{ ScalaTerm, ScalaTerms, SwaggerTerm, SwaggerTerms }
import com.twilio.guardrail.extract.{ Default, ScalaType }
import com.twilio.guardrail.generators.{ GeneratorSettings, ScalaGenerator, ScalaParameter, SwaggerGenerator }
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.languages.LA
import java.util.{ Map => JMap }
import scala.language.reflectiveCalls
import scala.meta._
import com.twilio.guardrail.languages.ScalaLanguage

object SwaggerUtil {
  sealed trait ResolvedType[L <: LA]
  case class Resolved[L <: LA](tpe: L#Type, classDep: Option[L#TermName], defaultValue: Option[L#Term]) extends ResolvedType[L]
  sealed trait LazyResolvedType[L <: LA]                                                                extends ResolvedType[L]
  case class Deferred[L <: LA](value: String)                                                           extends LazyResolvedType[L]
  case class DeferredArray[L <: LA](value: String)                                                      extends LazyResolvedType[L]
  case class DeferredMap[L <: LA](value: String)                                                        extends LazyResolvedType[L]
  object ResolvedType {
    implicit class FoldableExtension[F[_]](F: Foldable[F]) {
      import cats.{ Alternative, Monoid }
      def partitionEither[A, B, C](value: F[A])(f: A => Either[B, C])(implicit A: Alternative[F]): (F[B], F[C]) = {

        implicit val mb: Monoid[F[B]] = A.algebra[B]
        implicit val mc: Monoid[F[C]] = A.algebra[C]

        F.foldMap(value)(
          a =>
            f(a) match {
              case Left(b)  => (A.pure(b), A.empty[C])
              case Right(c) => (A.empty[B], A.pure(c))
          }
        )
      }
    }

    def resolveReferences[M[_]](
        values: List[(String, ResolvedType[ScalaLanguage])]
    )(implicit M: MonadError[M, String]): M[List[(String, Resolved[ScalaLanguage])]] = {
      val (lazyTypes, resolvedTypes) = Foldable[List].partitionEither(values) {
        case (clsName, x: Resolved[ScalaLanguage])         => Right((clsName, x))
        case (clsName, x: LazyResolvedType[ScalaLanguage]) => Left((clsName, x))
      }

      def lookupTypeName(clsName: String, tpeName: String, resolvedTypes: List[(String, Resolved[ScalaLanguage])])(
          f: Type => Type
      ): Option[(String, Resolved[ScalaLanguage])] =
        resolvedTypes
          .find(_._1 == tpeName)
          .map(_._2.tpe)
          .map(x => (clsName, Resolved[ScalaLanguage](f(x), None, None)))

      FlatMap[M]
        .tailRecM[(List[(String, LazyResolvedType[ScalaLanguage])], List[(String, Resolved[ScalaLanguage])]), List[(String, Resolved[ScalaLanguage])]](
          (lazyTypes, resolvedTypes)
        ) {
          case (lazyTypes, resolvedTypes) =>
            if (lazyTypes.isEmpty) {
              M.pure(Right(resolvedTypes))
            } else {
              val (newLazyTypes, newResolvedTypes) =
                Foldable[List].partitionEither(lazyTypes) {
                  case x @ (clsName, Deferred(tpeName)) =>
                    Either.fromOption(lookupTypeName(clsName, tpeName, resolvedTypes)(identity), x)
                  case x @ (clsName, DeferredArray(tpeName)) =>
                    Either.fromOption(lookupTypeName(clsName, tpeName, resolvedTypes)(tpe => t"IndexedSeq[${tpe}]"), x)
                  case x @ (clsName, DeferredMap(tpeName)) =>
                    Either.fromOption(lookupTypeName(clsName, tpeName, resolvedTypes)(tpe => t"Map[String, ${tpe}]"), x)
                }

              M.pure(Left((newLazyTypes, resolvedTypes ++ newResolvedTypes)))
            }
        }
    }

    def resolve[M[_]](value: ResolvedType[ScalaLanguage],
                      protocolElems: List[StrictProtocolElems[ScalaLanguage]])(implicit M: MonadError[M, String]): M[Resolved[ScalaLanguage]] =
      value match {
        case x @ Resolved(tpe, _, default) => M.pure(x)
        case Deferred(name) =>
          M.fromOption(protocolElems.find(_.name == name), s"Unable to resolve ${name}")
            .map {
              case RandomType(name, tpe) => Resolved[ScalaLanguage](tpe, None, None)
              case ClassDefinition(name, tpe, cls, companion, _) =>
                Resolved[ScalaLanguage](tpe, None, None)
              case EnumDefinition(name, tpe, elems, cls, companion) =>
                Resolved[ScalaLanguage](tpe, None, None)
              case ADT(_, tpe, _, _) =>
                Resolved[ScalaLanguage](tpe, None, None)
            }
        case DeferredArray(name) =>
          M.fromOption(protocolElems.find(_.name == name), s"Unable to resolve ${name}")
            .map {
              case RandomType(name, tpe) =>
                Resolved[ScalaLanguage](t"IndexedSeq[${tpe}]", None, None)
              case ClassDefinition(name, tpe, cls, companion, _) =>
                Resolved[ScalaLanguage](t"IndexedSeq[${tpe}]", None, None)
              case EnumDefinition(name, tpe, elems, cls, companion) =>
                Resolved[ScalaLanguage](t"IndexedSeq[${tpe}]", None, None)
              case ADT(_, tpe, _, _) =>
                Resolved[ScalaLanguage](t"IndexedSeq[$tpe]", None, None)
            }
        case DeferredMap(name) =>
          M.fromOption(protocolElems.find(_.name == name), s"Unable to resolve ${name}")
            .map {
              case RandomType(name, tpe) =>
                Resolved[ScalaLanguage](t"Map[String, ${tpe}]", None, None)
              case ClassDefinition(_, tpe, _, _, _) =>
                Resolved[ScalaLanguage](t"Map[String, ${tpe}]", None, None)
              case EnumDefinition(_, tpe, _, _, _) =>
                Resolved[ScalaLanguage](t"Map[String, ${tpe}]", None, None)
              case ADT(_, tpe, _, _) =>
                Resolved[ScalaLanguage](t"Map[String, $tpe]", None, None)
            }
      }
  }

  sealed class ModelMetaTypePartiallyApplied[L <: LA, F[_]](val dummy: Boolean = true) {
    def apply[T <: Model](model: T)(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, ResolvedType[L]] = {
      import Sc._
      import Sw._
      model match {
        case ref: RefModel =>
          for {
            ref <- getSimpleRef(ref)
          } yield Deferred[L](ref)
        case arr: ArrayModel =>
          for {
            items <- getItems(arr)
            meta  <- propMetaF[L, F](items)
            res <- meta match {
              case Resolved(inner, dep, default) =>
                (liftVectorType(inner), default.traverse(x => liftVectorTerm(x))).mapN(Resolved[L](_, dep, _))
              case x: Deferred[L]      => embedArray(x)
              case x: DeferredArray[L] => embedArray(x)
              case x: DeferredMap[L]   => embedArray(x)
            }
          } yield res
        case impl: ModelImpl =>
          for {
            tpeName <- getType(impl)
            tpe     <- typeNameF[L, F](tpeName, Option(impl.getFormat()), ScalaType(impl))
          } yield Resolved[L](tpe, None, None)
      }
    }
  }

  def modelMetaType[T <: Model](model: T, gs: GeneratorSettings[ScalaLanguage]): Target[ResolvedType[ScalaLanguage]] =
    new ModelMetaTypePartiallyApplied[ScalaLanguage, EitherK[ScalaTerm[ScalaLanguage, ?], SwaggerTerm[ScalaLanguage, ?], ?]]()
      .apply(model)
      .value
      .foldMap(ScalaGenerator.ScalaInterp.or(SwaggerGenerator.SwaggerInterp))
  def modelMetaTypeF[L <: LA, F[_]]: ModelMetaTypePartiallyApplied[L, F] =
    new ModelMetaTypePartiallyApplied[L, F]()

  // Standard type conversions, as documented in http://swagger.io/specification/#data-types-12
  def typeNameF[L <: LA, F[_]](typeName: String, format: Option[String], customType: Option[String])(implicit Sc: ScalaTerms[L, F]): Free[F, L#Type] = {
    import Sc._

    def log(fmt: Option[String], t: L#Type): L#Type = {
      fmt.foreach { fmt =>
        println(s"Warning: Deprecated behavior: Unsupported type '$fmt', falling back to $t. Please switch definitions to x-scala-type for custom types")
      }

      t
    }
    def liftCustomType(s: String): Free[F, Option[L#Type]] = {
      val tpe = s.trim
      if (tpe.nonEmpty) {
        parseType(tpe)
      } else Free.pure(Option.empty[L#Type])
    }

    customType
      .flatTraverse(liftCustomType _)
      .flatMap(
        _.fold({
          (typeName, format) match {
            case ("string", Some("date"))      => dateType()
            case ("string", Some("date-time")) => dateTimeType()
            case ("string", fmt)               => stringType(fmt).map(log(fmt, _))
            case ("number", Some("float"))     => floatType()
            case ("number", Some("double"))    => doubleType()
            case ("number", fmt)               => numberType(fmt).map(log(fmt, _))
            case ("integer", Some("int32"))    => intType()
            case ("integer", Some("int64"))    => longType()
            case ("integer", fmt)              => integerType(fmt).map(log(fmt, _))
            case ("boolean", fmt)              => booleanType(fmt).map(log(fmt, _))
            case ("array", fmt)                => arrayType(fmt).map(log(fmt, _))
            case ("file", fmt)                 => fileType(fmt).map(log(fmt, _))
            case ("object", fmt)               => objectType(fmt).map(log(fmt, _))
            case (tpe, fmt)                    => fallbackType(tpe, fmt)
          }
        })(Free.pure(_))
      )
  }

  def typeName(typeName: String, format: Option[String], customType: Option[String], gs: GeneratorSettings[ScalaLanguage]): Type =
    Target.unsafeExtract(
      typeNameF[ScalaLanguage, ScalaTerm[ScalaLanguage, ?]](typeName, format, customType)
        .foldMap(ScalaGenerator.ScalaInterp),
      gs
    )

  def propMetaF[L <: LA, F[_]](property: Property)(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, ResolvedType[L]] = {
    import Sc._
    import Sw._
    property match {
      case p: ArrayProperty =>
        for {
          items <- getItemsP(p)
          rec   <- propMetaF[L, F](items)
          res <- rec match {
            case Resolved(inner, dep, default) =>
              (liftVectorType(inner), default.traverse(liftVectorTerm)).mapN(Resolved[L](_, dep, _): ResolvedType[L])
            case x: DeferredMap[L]   => embedArray(x)
            case x: DeferredArray[L] => embedArray(x)
            case x: Deferred[L]      => embedArray(x)
          }
        } yield res
      case m: MapProperty =>
        for {
          rec <- propMetaF[L, F](m.getAdditionalProperties)
          res <- rec match {
            case Resolved(inner, dep, _) => liftMapType(inner).map(Resolved[L](_, dep, None))
            case x: DeferredMap[L]       => embedMap(x)
            case x: DeferredArray[L]     => embedMap(x)
            case x: Deferred[L]          => embedMap(x)
          }
        } yield res
      case o: ObjectProperty =>
        jsonType().map(Resolved[L](_, None, None)) // TODO: o.getProperties
      case r: RefProperty =>
        getSimpleRefP(r).map(Deferred[L](_))
      case b: BooleanProperty =>
        (typeNameF[L, F]("boolean", None, ScalaType(b)), Default(b).extract[Boolean].traverse(litBoolean(_))).mapN(Resolved[L](_, None, _))
      case s: StringProperty =>
        (typeNameF[L, F]("string", Option(s.getFormat()), ScalaType(s)), Default(s).extract[String].traverse(litString(_)))
          .mapN(Resolved[L](_, None, _))

      case d: DateProperty =>
        typeNameF[L, F]("string", Some("date"), ScalaType(d)).map(Resolved[L](_, None, None))
      case d: DateTimeProperty =>
        typeNameF[L, F]("string", Some("date-time"), ScalaType(d)).map(Resolved[L](_, None, None))

      case l: LongProperty =>
        (typeNameF[L, F]("integer", Some("int64"), ScalaType(l)), Default(l).extract[Long].traverse(litLong(_))).mapN(Resolved[L](_, None, _))
      case i: IntegerProperty =>
        (typeNameF[L, F]("integer", Some("int32"), ScalaType(i)), Default(i).extract[Int].traverse(litInt(_))).mapN(Resolved[L](_, None, _))
      case f: FloatProperty =>
        (typeNameF[L, F]("number", Some("float"), ScalaType(f)), Default(f).extract[Float].traverse(litFloat(_))).mapN(Resolved[L](_, None, _))
      case d: DoubleProperty =>
        (typeNameF[L, F]("number", Some("double"), ScalaType(d)), Default(d).extract[Double].traverse(litDouble(_))).mapN(Resolved[L](_, None, _))
      case d: DecimalProperty =>
        typeNameF[L, F]("number", None, ScalaType(d)).map(Resolved[L](_, None, None))
      case u: UntypedProperty =>
        jsonType().map(Resolved[L](_, None, None))
      case p: AbstractProperty if Option(p.getType).exists(_.toLowerCase == "integer") =>
        typeNameF[L, F]("integer", None, ScalaType(p)).map(Resolved[L](_, None, None))
      case p: AbstractProperty if Option(p.getType).exists(_.toLowerCase == "number") =>
        typeNameF[L, F]("number", None, ScalaType(p)).map(Resolved[L](_, None, None))
      case p: AbstractProperty if Option(p.getType).exists(_.toLowerCase == "string") =>
        typeNameF[L, F]("string", None, ScalaType(p)).map(Resolved[L](_, None, None))
      case x =>
        fallbackPropertyTypeHandler(x).map(Resolved[L](_, None, None))
    }
  }

  def propMeta(property: Property, gs: GeneratorSettings[ScalaLanguage]): Target[ResolvedType[ScalaLanguage]] = {
    type Program[T] = EitherK[ScalaTerm[ScalaLanguage, ?], SwaggerTerm[ScalaLanguage, ?], T]
    val interp = ScalaGenerator.ScalaInterp.or(SwaggerGenerator.SwaggerInterp)
    propMetaF[ScalaLanguage, Program](property).value
      .foldMap(interp)
  }

  /*
    Required \ Default  || Defined  || Undefined / NULL ||
    =====================================================
    TRUE                || a: T = v || a: T             ||
    FALSE / NULL        || a: T = v || a: Opt[T] = None ||
   */

  private[this] val successCodesWithEntities =
    List(200, 201, 202, 203, 206, 226).map(_.toString)
  private[this] val successCodesWithoutEntities = List(204, 205).map(_.toString)

  private[this] def getBestSuccessResponse(responses: JMap[String, Response]): Option[Response] =
    successCodesWithEntities
      .find(responses.containsKey)
      .flatMap(code => Option(responses.get(code)))
  private[this] def hasEmptySuccessType(responses: JMap[String, Response]): Boolean =
    successCodesWithoutEntities.exists(responses.containsKey)

  def getResponseTypeF[L <: LA, F[_]](httpMethod: HttpMethod, operation: Operation, ignoredType: L#Type)(
      implicit Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ResolvedType[L]] = {
    import Sc._
    if (httpMethod == HttpMethod.GET || httpMethod == HttpMethod.PUT || httpMethod == HttpMethod.POST) {
      Option(operation.getResponses)
        .flatMap { responses =>
          getBestSuccessResponse(responses)
            .flatMap(resp => Option(resp.getSchema))
            .map(propMetaF[L, F](_))
            .orElse(
              if (hasEmptySuccessType(responses))
                Some(Free.pure[F, ResolvedType[L]](Resolved[L](ignoredType, None, None)))
              else None
            )
        }
        .getOrElse(Free.pure(Resolved[L](ignoredType, None, None): ResolvedType[L]))
    } else {
      Free.pure(Resolved[L](ignoredType, None, None): ResolvedType[L])
    }
  }

  def getResponseType(httpMethod: HttpMethod,
                      operation: Operation,
                      ignoredType: Type,
                      gs: GeneratorSettings[ScalaLanguage]): Target[ResolvedType[ScalaLanguage]] = {
    type Program[T] = EitherK[ScalaTerm[ScalaLanguage, ?], SwaggerTerm[ScalaLanguage, ?], T]
    val interp = ScalaGenerator.ScalaInterp.or(SwaggerGenerator.SwaggerInterp)
    getResponseTypeF[ScalaLanguage, Program](httpMethod, operation, ignoredType).value
      .foldMap(interp)
  }

  object paths {
    import atto._, Atto._

    private[this] def lookupName[T](bindingName: String,
                                    pathArgs: List[ScalaParameter[ScalaLanguage]])(f: ScalaParameter[ScalaLanguage] => Parser[T]): Parser[T] =
      pathArgs
        .find(_.argName.value == bindingName)
        .fold[Parser[T]](
          err(s"Unable to find argument ${bindingName}")
        )(param => f(param))

    private[this] val variable: Parser[String] = char('{') ~> many(notChar('}'))
      .map(_.mkString("")) <~ char('}')

    def generateUrlPathParams(path: String, pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[Term] = {
      val term: Parser[Term.Apply] = variable.flatMap { binding =>
        lookupName(binding, pathArgs) { param =>
          ok(q"Formatter.addPath(${param.paramName})")
        }
      }
      val other: Parser[String]                             = many1(notChar('{')).map(_.toList.mkString)
      val pattern: Parser[List[Either[String, Term.Apply]]] = many(either(term, other).map(_.swap: Either[String, Term.Apply]))

      for {
        parts <- pattern
          .parseOnly(path)
          .either
          .fold(Target.error(_), Target.pure(_))
        result = parts
          .map({
            case Left(part)  => Lit.String(part)
            case Right(term) => term
          })
          .foldLeft[Term](q"host + basePath")({ case (a, b) => q"${a} + ${b}" })
      } yield result
    }

    class Extractors[T, TN <: T](
        pathSegmentConverter: (ScalaParameter[ScalaLanguage], Option[T]) => Either[String, T],
        buildParamConstraint: ((String, String)) => T,
        joinParams: (T, T) => T,
        stringPath: String => T,
        liftBinding: Term.Name => TN,
        litRegex: (String, Term.Name, String) => T
    ) {
      // (Option[TN], T) is (Option[Binding], Segment)
      type P  = Parser[(Option[TN], T)]
      type LP = Parser[List[(Option[TN], T)]]

      val plainString      = many(noneOf("{}/?")).map(_.mkString)
      val plainNEString    = many1(noneOf("{}/?")).map(_.toList.mkString)
      val stringSegment: P = plainNEString.map(s => (None, stringPath(s)))
      def regexSegment(implicit pathArgs: List[ScalaParameter[ScalaLanguage]]): P =
        (plainString ~ variable ~ plainString).flatMap {
          case ((before, binding), after) =>
            lookupName(binding, pathArgs) {
              case param @ ScalaParameter(_, _, paramName, argName, _) =>
                val value = if (before.nonEmpty || after.nonEmpty) {
                  pathSegmentConverter(param, Some(litRegex(before.mkString, paramName, after.mkString)))
                    .fold(err, ok)
                } else {
                  pathSegmentConverter(param, None).fold(err, ok)
                }
                value.map((Some(liftBinding(paramName)), _))
            }
        }

      def segments(implicit pathArgs: List[ScalaParameter[ScalaLanguage]]): LP =
        sepBy1(choice(regexSegment(pathArgs), stringSegment), char('/'))
          .map(_.toList)

      val qsValueOnly: Parser[(String, String)] = ok("") ~ (char('=') ~> opt(many(noneOf("&")))
        .map(_.fold("")(_.mkString)))
      val staticQSArg: Parser[(String, String)] = many1(noneOf("=&"))
        .map(_.toList.mkString) ~ opt(char('=') ~> many(noneOf("&")))
        .map(_.fold("")(_.mkString))
      val staticQSTerm: Parser[T] =
        choice(staticQSArg, qsValueOnly).map(buildParamConstraint)
      val trailingSlash: Parser[Boolean] = opt(char('/')).map(_.nonEmpty)
      val staticQS: Parser[Option[T]] = (opt(
        char('?') ~> sepBy1(staticQSTerm, char('&'))
          .map(_.reduceLeft(joinParams))
      ) | opt(char('?')).map { _ =>
        None
      })
      val emptyPath: Parser[(List[(Option[TN], T)], (Boolean, Option[T]))]   = endOfInput ~> ok((List.empty[(Option[TN], T)], (false, None)))
      val emptyPathQS: Parser[(List[(Option[TN], T)], (Boolean, Option[T]))] = ok(List.empty[(Option[TN], T)]) ~ (ok(false) ~ staticQS)
      def pattern(implicit pathArgs: List[ScalaParameter[ScalaLanguage]]): Parser[(List[(Option[TN], T)], (Boolean, Option[T]))] =
        (segments ~ (trailingSlash ~ staticQS) <~ endOfInput) | emptyPathQS | emptyPath
    }

    object akkaExtractor
        extends Extractors[Term, Term.Name](
          pathSegmentConverter = {
            case (ScalaParameter(_, param, _, argName, argType), base) =>
              base.fold {
                argType match {
                  case t"String" => Right(q"Segment")
                  case t"Double" => Right(q"DoubleNumber")
                  case t"BigDecimal" =>
                    Right(q"Segment.map(BigDecimal.apply _)")
                  case t"Int"    => Right(q"IntNumber")
                  case t"Long"   => Right(q"LongNumber")
                  case t"BigInt" => Right(q"Segment.map(BigInt.apply _)")
                  case tpe @ Type.Name(_) =>
                    Right(q"Segment.flatMap(str => io.circe.Json.fromString(str).as[${tpe}].toOption)")
                }
              } { segment =>
                argType match {
                  case t"String" => Right(segment)
                  case t"BigDecimal" =>
                    Right(q"${segment}.map(BigDecimal.apply _)")
                  case t"BigInt" => Right(q"${segment}.map(BigInt.apply _)")
                  case tpe @ Type.Name(_) =>
                    Right(q"${segment}.flatMap(str => io.circe.Json.fromString(str).as[${tpe}].toOption)")
                }
              }
          },
          buildParamConstraint = {
            case (k, v) =>
              q" parameter(${Lit.String(k)}).require(_ == ${Lit.String(v)}) "
          },
          joinParams = { (l, r) =>
            q"${l} & ${r}"
          },
          stringPath = Lit.String(_),
          liftBinding = identity,
          litRegex = (before, _, after) =>
            q"""new scala.util.matching.Regex("^" + ${Lit
              .String(before)} + "(.*)" + ${Lit.String(after)} + ${Lit.String("$")})"""
        )

    object http4sExtractor
        extends Extractors[Pat, Term.Name](
          pathSegmentConverter = {
            case (ScalaParameter(_, param, paramName, argName, argType), base) =>
              base.fold[Either[String, Pat]] {
                argType match {
                  case t"String"     => Right(Pat.Var(paramName))
                  case t"Double"     => Right(p"DoubleVar($paramName)")
                  case t"BigDecimal" => Right(p"BigDecimalVar(${Pat.Var(paramName)})")
                  case t"Int"        => Right(p"IntVar(${Pat.Var(paramName)})")
                  case t"Long"       => Right(p"LongVar(${Pat.Var(paramName)})")
                  case t"BigInt"     => Right(p"BigIntVar(${Pat.Var(paramName)})")
                  case tpe @ Type.Name(_) =>
                    Right(p"${Term.Name(s"${tpe}Var")}(${Pat.Var(paramName)})")
                }
              } { _ =>
                //todo add support for regex segment
                Left("Unsupported feature")
              }
          },
          buildParamConstraint = {
            case (k, v) =>
              p"${Term.Name(s"${k.capitalize}Matcher")}(${Lit.String(v)})"
          },
          joinParams = { (l, r) =>
            p"${l} +& ${r}"
          },
          stringPath = Lit.String(_),
          liftBinding = identity,
          litRegex = (before, _, after) =>
            //todo add support for regex segment
            throw new UnsupportedOperationException
        )

    def generateUrlAkkaPathExtractors(path: String, pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[Term] = {
      import akkaExtractor._
      for {
        partsQS <- pattern(pathArgs)
          .parse(path)
          .done
          .either
          .fold(Target.error(_), Target.pure(_))
        (parts, (trailingSlash, queryParams)) = partsQS
        (directive, bindings) = parts
          .foldLeft[(Term, List[Term.Name])]((q"pathEnd", List.empty))({
            case ((q"pathEnd   ", bindings), (termName, b)) =>
              (q"path(${b}       )", bindings ++ termName)
            case ((q"path(${a })", bindings), (termName, c)) =>
              (q"path(${a} / ${c})", bindings ++ termName)
          })
        trailingSlashed = if (trailingSlash) {
          directive match {
            case q"path(${a })" => q"pathPrefix(${a}) & pathEndOrSingleSlash"
            case q"pathEnd"     => q"pathEndOrSingleSlash"
          }
        } else directive
        result = queryParams.fold(trailingSlashed) { qs =>
          q"${trailingSlashed} & ${qs}"
        }
      } yield result
    }

    def generateUrlHttp4sPathExtractors(path: String, pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[(Pat, Option[Pat])] = {
      import http4sExtractor._
      for {
        partsQS <- pattern(pathArgs)
          .parse(path)
          .done
          .either
          .fold(Target.error(_), Target.pure(_))
        (parts, (trailingSlash, queryParams)) = partsQS
        (directive, bindings) = parts
          .foldLeft[(Pat, List[Term.Name])]((p"${Term.Name("Root")}", List.empty))({
            case ((acc, bindings), (termName, c)) =>
              (p"$acc / ${c}", bindings ++ termName)
          })
        trailingSlashed = if (trailingSlash) {
          p"$directive / ${Lit.String("")}"
        } else directive
      } yield (trailingSlashed, queryParams)
    }
  }
}
