package dev.guardrail.generators.scala.zioHttp


sealed abstract class ZioHttpVersion(val value: String)
object ZioHttpVersion extends ZioHttpVersion("zio-http") {
  val mapping: Map[String, ZioHttpVersion] = Map(
    "zio-http" -> ZioHttpVersion
  )
}

