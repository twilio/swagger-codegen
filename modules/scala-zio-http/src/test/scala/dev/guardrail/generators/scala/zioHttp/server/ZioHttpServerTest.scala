package dev.guardrail.generators.scala.zioHttp.server

import dev.guardrail.Context
import dev.guardrail.generators.{Server, Servers}
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.zioHttp.ZioHttpVersion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import support.{ScalaMetaMatchers, SwaggerSpecRunner}

class ZioHttpServerTest  extends AnyFreeSpec with Matchers with SwaggerSpecRunner with ScalaMetaMatchers {

  "handlers" in {

//    val version = ZioHttpVersion.`3.0.0-RC4`

    val (
      _,
      _,
      Servers(Server(_, _, genHandler, genResource :: _) :: Nil, Nil)
      ) = runSwaggerSpec(scalaInterpreter)(Helpers.spec)(Context.empty, "zio-http")
  }

}
