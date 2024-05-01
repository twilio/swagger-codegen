package dev.guardrail.generators.scala.zioHttp.server

import dev.guardrail.{AuthImplementation, Context}
import dev.guardrail.generators.{LanguageParameter, OpenAPIGenerator, Server, Servers}
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.zioHttp.{ZioHttpGenerator, ZioHttpVersion}
import dev.guardrail.terms.server.SecurityExposure
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import support.{ScalaMetaMatchers, SwaggerSpecRunner}

import scala.meta.XtensionQuasiquoteTerm
import scala.meta.XtensionQuasiquoteType
import dev.guardrail._
import dev.guardrail.core.Tracker
import dev.guardrail.generators.scala.{ScalaCollectionsGenerator, ScalaGenerator, ScalaGeneratorLoader, ScalaLanguage}
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{CollectionsLibTerms, LanguageTerms, OpenAPITerms, RouteMeta}
import dev.guardrail.terms.protocol.{ClassDefinition, StaticDefns, StrictProtocolElems}
import io.swagger.v3.oas.models.media.IntegerSchema
import io.swagger.v3.oas.models.parameters.Parameter
import io.swagger.v3.oas.models.responses.{ApiResponse, ApiResponses}
import io.swagger.v3.oas.models.{Operation, PathItem}

import java.util

class ZioHttpServerTest extends AnyFreeSpec with Matchers with SwaggerSpecRunner with ScalaMetaMatchers {

  trait DefaultInstances {
    implicit val languageTerms: LanguageTerms[ScalaLanguage, Target] = ScalaGenerator.apply()
    implicit val collectionsTerms: CollectionsLibTerms[ScalaLanguage, Target] = ScalaCollectionsGenerator()
    implicit val openApiTerms: OpenAPITerms[ScalaLanguage, Target] = OpenAPIGenerator.apply()
    implicit val frameworkTerms: FrameworkTerms[ScalaLanguage, Target] = ZioHttpGenerator.apply()
  }

  "route meta" in new DefaultInstances {

    val op: Operation = {
      val inner = new Operation()
      inner.setOperationId("getOrderById")
      inner
    }

    val orderIdParam = new Parameter()
    orderIdParam.setName("order_id")
    orderIdParam.setIn("path")
    orderIdParam.setSchema {
      val schema = new IntegerSchema()
      schema.setFormat("int64")
      schema.setType("integer")
      schema
    }

    op.setParameters( {
      val list = new util.ArrayList[Parameter]()
      list.add(orderIdParam)
      list
    })

    op.setResponses({
      val responses = new ApiResponses()
        .addApiResponse("200", new ApiResponse())
      responses
    })

    val route = RouteMeta(
      path = Tracker.unsafe("/store/order/{order_id}", Vector(".paths", "./store/order/{order_id}")),
      method = PathItem.HttpMethod.GET,
      operation = Tracker.unsafe(op, Vector.empty),
      securityRequirements = None
    )


    val protocolElems = ClassDefinition[ScalaLanguage](
      name = "Order",
      tpe = t"Order",
      fullType = t"Order",
      cls = q"case class Order(id: Option[Long] = None, petId: Option[Long] = None, quantity: Option[Int] = None, shipDate: Option[java.time.OffsetDateTime] = None, status: Option[Order.Status] = None, complete: Option[Boolean] = Option(false))",
      staticDefns = StaticDefns[ScalaLanguage](
        className = "Order",
        extraImports = List.empty,
        definitions = List.empty,
        statements = List.empty
      )
    )

    val result = Routes.generateRouteMeta(route)(
      List[StrictProtocolElems[ScalaLanguage]](protocolElems), Tracker.unsafe(None, Vector.empty))

    private val value1: Routes.ResponseDefinitionsForGeneratedRoute = result.value
    print(value1)
  }

  "handler" in {
    val result = Handlers.renderHandler(
      handlerName = "StoreHandler",
      methodSigs = List(
        q"def getRoot: F[Response[F]]"
      ),
      handlerDefinitions = List.empty,
      responseDefinitions = List.empty,
      customExtraction = false,
      authImplementation = AuthImplementation.Native,
      securityExposure = SecurityExposure.Undefined
    )

    print(result.value.syntax)

  }

  "handlers" in {
    val (
      _,
      _,
      Servers(server :: Nil, Nil)
    ) = runSwaggerSpec(scalaInterpreter)(Helpers.spec)(Context.empty, "zio-http")

    println(server.handlerDefinition)
  }

}
