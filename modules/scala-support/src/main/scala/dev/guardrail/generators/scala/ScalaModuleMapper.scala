package dev.guardrail.generators.scala

import dev.guardrail.generators.spi.ModuleMapperLoader
import dev.guardrail.Target
import scala.reflect.runtime.universe.typeTag

class ScalaModuleMapper extends ModuleMapperLoader {
  type L = ScalaLanguage
  def reified = typeTag[Target[ScalaLanguage]]
  def apply(frameworkName: String): Option[Set[String]] = frameworkName match {
    case "akka-http"         => Some(Set("akka-http", "circe"))
    case "zio-http"         => Some(Set("zio-http", "circe")) // use zio - json
    case "http4s"            => Some(Set("circe", "http4s"))
    case "http4s-v0.23"      => Some(Set("circe", "http4s-v0.23"))
    case "http4s-v0.22"      => Some(Set("circe", "http4s-v0.22"))
    case "akka-http-jackson" => Some(Set("akka-http", "jackson"))
    case "dropwizard"        => Some(Set("dropwizard", "jackson"))
    case _                   => None
  }
}
