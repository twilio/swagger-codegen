package dev.guardrail.generators.scala.zioHttp.server

import dev.guardrail.core.Tracker
import dev.guardrail.core.extract.ServerRawResponse
import io.swagger.v3.oas.models.Operation

import _root_.scala.meta._

object ResponsesHelper {


  def test(resourceName: String,
           responseClassName: String,
           operation: Tracker[Operation]) = {
    val resourceTerm = Term.Name(resourceName)

    val (responseCompanionTerm, responseCompanionType) =
      (Term.Name(responseClassName), Type.Name(responseClassName))

    ServerRawResponse(operation).filter(_ == true)
      .fold[Type](t"${resourceTerm}.$responseCompanionType")(Function.const(t"Response[F]"))

  }
}
