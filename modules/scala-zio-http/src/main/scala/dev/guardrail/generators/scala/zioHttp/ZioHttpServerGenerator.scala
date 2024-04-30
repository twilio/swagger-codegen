package dev.guardrail.generators.scala.zioHttp

import _root_.io.swagger.v3.oas.models.Components
import cats.data.NonEmptyList
import dev.guardrail._
import dev.guardrail.core.Tracker
import dev.guardrail.generators.Servers
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.spi.{ModuleLoadResult, ServerGeneratorLoader}
import dev.guardrail.terms._
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol._
import dev.guardrail.terms.server._

class ZioHttpServerGeneratorLoader extends ServerGeneratorLoader {
  type L = ScalaLanguage
  override def reified = scala.reflect.runtime.universe.typeTag[Target[ScalaLanguage]]
  val apply =
    ModuleLoadResult.forProduct1(
      ServerGeneratorLoader.label -> Seq(ZioHttpVersion.mapping),
//      ProtocolGeneratorLoader.label -> Seq(
//        CirceModelGenerator.mapping
//      )
    ) { (zioHttpVersion) =>
      ZioHttpServerGenerator(zioHttpVersion)
    }
}

object ZioHttpServerGenerator {
  def apply(version: ZioHttpVersion): ServerTerms[ScalaLanguage, Target] =
    new ZioHttpServerGenerator(version)

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
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target]
  ): Target[Servers[ScalaLanguage]] =
    Target.raiseUserError("not implemented")

}
