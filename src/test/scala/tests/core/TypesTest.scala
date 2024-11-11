package tests.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.meta._

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.terms.protocol.ClassDefinition

class TypesTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  test("Generate no definitions") {
    val spec: String = s"""
      |swagger: "2.0"
      |info:
      |  title: Whatever
      |  version: 1.0.0
      |host: localhost:1234
      |definitions:
      |  Types:
      |    type: object
      |    properties:
      |      array:
      |        type: array
      |        items:
      |          type: boolean
      |      map:
      |        type: object
      |        additionalProperties:
      |          type: boolean
      |      obj:
      |        type: object
      |      bool:
      |        type: boolean
      |      string:
      |        type: string
      |      date:
      |        type: string
      |        format: date
      |      date_time:
      |        type: string
      |        format: date-time
      |      byte:
      |        type: string
      |        format: byte
      |      long:
      |        type: integer
      |        format: int64
      |      int:
      |        type: integer
      |        format: int32
      |      float:
      |        type: number
      |        format: float
      |      double:
      |        type: number
      |        format: double
      |      number:
      |        type: number
      |      integer:
      |        type: integer
      |      untyped:
      |        description: Untyped
      |      custom:
      |        type: string
      |        x-scala-type: Foo
      |      customComplex:
      |        type: string
      |        x-scala-type: Foo[Bar]
      |      nested:
      |        type: object
      |        properties:
      |          prop1:
      |            type: string
      |      nestedArray:
      |        type: array
      |        items:
      |          type: object
      |          properties:
      |            prop1:
      |              type: string
      |      requiredArray:
      |        type: array
      |        items:
      |          type: string
      |    required:
      |      - requiredArray
      |""".stripMargin
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Types(
        array: Option[Vector[Boolean]] = None,
        map: Option[Map[String, Boolean]] = None,
        obj: Option[io.circe.Json] = None,
        bool: Option[Boolean] = None,
        string: Option[String] = None,
        date: Option[java.time.LocalDate] = None,
        dateTime: Option[java.time.OffsetDateTime] = None,
        byte: Option[Base64String] = None,
        long: Option[Long] = None,
        int: Option[Int] = None,
        float: Option[Float] = None,
        double: Option[Double] = None,
        number: Option[BigDecimal] = None,
        integer: Option[BigInt] = None,
        untyped: Option[io.circe.Json] = None,
        custom: Option[Foo] = None,
        customComplex: Option[Foo[Bar]] = None,
        nested: Option[Types.Nested] = None,
        nestedArray: Option[Vector[Types.NestedArray]] = None,
        requiredArray: Vector[String] = Vector.empty
      )
    """

    val companion = q"""
      object Types {
        implicit val encodeTypes: _root_.io.circe.Encoder.AsObject[Types] = {
          _root_.io.circe.Encoder.AsObject.instance[Types](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("array", a.array.asJson), ("map", a.map.asJson), ("obj", a.obj.asJson), ("bool", a.bool.asJson), ("string", a.string.asJson), ("date", a.date.asJson), ("date_time", a.dateTime.asJson), ("byte", a.byte.asJson), ("long", a.long.asJson), ("int", a.int.asJson), ("float", a.float.asJson), ("double", a.double.asJson), ("number", a.number.asJson), ("integer", a.integer.asJson), ("untyped", a.untyped.asJson), ("custom", a.custom.asJson), ("customComplex", a.customComplex.asJson), ("nested", a.nested.asJson), ("nestedArray", a.nestedArray.asJson), ("requiredArray", a.requiredArray.asJson))))
        }
        implicit val decodeTypes: _root_.io.circe.Decoder[Types] = new _root_.io.circe.Decoder[Types] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Types] = for (v0 <- c.downField("array").as[Option[Vector[Boolean]]]; v1 <- c.downField("map").as[Option[Map[String, Boolean]]]; v2 <- c.downField("obj").as[Option[io.circe.Json]]; v3 <- c.downField("bool").as[Option[Boolean]]; v4 <- c.downField("string").as[Option[String]]; v5 <- c.downField("date").as[Option[java.time.LocalDate]]; v6 <- c.downField("date_time").as[Option[java.time.OffsetDateTime]]; v7 <- c.downField("byte").as[Option[Base64String]]; v8 <- c.downField("long").as[Option[Long]]; v9 <- c.downField("int").as[Option[Int]]; v10 <- c.downField("float").as[Option[Float]]; v11 <- c.downField("double").as[Option[Double]]; v12 <- c.downField("number").as[Option[BigDecimal]]; v13 <- c.downField("integer").as[Option[BigInt]]; v14 <- c.downField("untyped").as[Option[io.circe.Json]]; v15 <- c.downField("custom").as[Option[Foo]]; v16 <- c.downField("customComplex").as[Option[Foo[Bar]]]; v17 <- c.downField("nested").as[Option[Types.Nested]]; v18 <- c.downField("nestedArray").as[Option[Vector[Types.NestedArray]]]; v19 <- c.downField("requiredArray").as[Vector[String]]) yield Types(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19) }
        case class Nested(prop1: Option[String] = None)
        object Nested {
          implicit val encodeNested: _root_.io.circe.Encoder.AsObject[Types.Nested] = {
            _root_.io.circe.Encoder.AsObject.instance[Types.Nested](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("prop1", a.prop1.asJson))))
          }
          implicit val decodeNested: _root_.io.circe.Decoder[Types.Nested] = new _root_.io.circe.Decoder[Types.Nested] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Types.Nested] = for (v0 <- c.downField("prop1").as[Option[String]]) yield Types.Nested(v0) }
        }
        case class NestedArray(prop1: Option[String] = None)
        object NestedArray {
          implicit val encodeNestedArray: _root_.io.circe.Encoder.AsObject[Types.NestedArray] = {
            _root_.io.circe.Encoder.AsObject.instance[Types.NestedArray](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("prop1", a.prop1.asJson))))
          }
          implicit val decodeNestedArray: _root_.io.circe.Decoder[Types.NestedArray] = new _root_.io.circe.Decoder[Types.NestedArray] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Types.NestedArray] = for (v0 <- c.downField("prop1").as[Option[String]]) yield Types.NestedArray(v0) }
        }
      }
    """

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }

  test("Generates from composed schema") {
    val spec: String = s"""
      |swagger: "2.0"
      |info:
      |  title: Whatever
      |  version: 1.0.0
      |host: localhost:1234
      |definitions:
      |  That:
      |    type: object
      |    properties:
      |      string:
      |        type: string
      |  Types:
      |    type: object
      |    properties:
      |      composed:
      |        allOf:
      |          - $$ref: '#/definitions/That'
      |          - type: object
      |            properties:
      |              int:
      |                type: integer
      |                format: int32
      |
      |""".stripMargin
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, _, _, _) :: ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""case class Types(composed: Option[Types.Composed] = None)"""

    val companion = q"""
      object Types {
        implicit val encodeTypes: _root_.io.circe.Encoder.AsObject[Types] = {
          _root_.io.circe.Encoder.AsObject.instance[Types](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("composed", a.composed.asJson))))
        }
        implicit val decodeTypes: _root_.io.circe.Decoder[Types] = new _root_.io.circe.Decoder[Types] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Types] = for (v0 <- c.downField("composed").as[Option[Types.Composed]]) yield Types(v0) }
        case class Composed(string: Option[String] = None, int: Option[Int] = None)
        object Composed {
          implicit val encodeComposed: _root_.io.circe.Encoder.AsObject[Types.Composed] = {
            _root_.io.circe.Encoder.AsObject.instance[Types.Composed](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("string", a.string.asJson), ("int", a.int.asJson))))
          }
          implicit val decodeComposed: _root_.io.circe.Decoder[Types.Composed] = new _root_.io.circe.Decoder[Types.Composed] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Types.Composed] = for (v0 <- c.downField("string").as[Option[String]]; v1 <- c.downField("int").as[Option[Int]]) yield Types.Composed(v0, v1) }
        }
      }
    """

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }

  test("Deeply nested structure is generated") {
    val spec =
      s"""
      |swagger: "2.0"
      |info:
      |  title: Whatever
      |  version: 1.0.0
      |host: localhost:1234
      |definitions:
      |  First:
      |    type: object
      |    properties:
      |      Second:
      |        type: object
      |        properties:
      |          Third:
      |            type: object
      |            properties:
      |              Fourth:
      |                type: string
      |""".stripMargin
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""case class First(second: Option[First.Second] = None)"""

    val companion = q"""
      object First {
        implicit val encodeFirst: _root_.io.circe.Encoder.AsObject[First] = {
          _root_.io.circe.Encoder.AsObject.instance[First](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("Second", a.second.asJson))))
        }
        implicit val decodeFirst: _root_.io.circe.Decoder[First] = new _root_.io.circe.Decoder[First] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[First] = for (v0 <- c.downField("Second").as[Option[First.Second]]) yield First(v0) }
        case class Second(third: Option[First.Second.Third] = None)
        object Second {
          implicit val encodeSecond: _root_.io.circe.Encoder.AsObject[First.Second] = {
            _root_.io.circe.Encoder.AsObject.instance[First.Second](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("Third", a.third.asJson))))
          }
          implicit val decodeSecond: _root_.io.circe.Decoder[First.Second] = new _root_.io.circe.Decoder[First.Second] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[First.Second] = for (v0 <- c.downField("Third").as[Option[First.Second.Third]]) yield First.Second(v0) }
          case class Third(fourth: Option[String] = None)
          object Third {
            implicit val encodeThird: _root_.io.circe.Encoder.AsObject[First.Second.Third] = {
              _root_.io.circe.Encoder.AsObject.instance[First.Second.Third](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("Fourth", a.fourth.asJson))))
            }
            implicit val decodeThird: _root_.io.circe.Decoder[First.Second.Third] = new _root_.io.circe.Decoder[First.Second.Third] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[First.Second.Third] = for (v0 <- c.downField("Fourth").as[Option[String]]) yield First.Second.Third(v0) }
          }
        }
      }
    """

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }

  test("Inherited nested structure work") {
    val spec =
      s"""
         |swagger: "2.0"
         |info:
         |  title: Whatever
         |  version: 1.0.0
         |host: localhost:1234
         |definitions:
         |  First:
         |    allOf:
         |    - $$ref: '#/definitions/Second'
         |  Second:
         |    type: object
         |    properties:
         |      value:
         |        type: string
         |      nested:
         |        type: object
         |        properties:
         |          value:
         |            type: boolean
         |""".stripMargin
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, _, _) :: ClassDefinition(_, _, _, _, staticDefns, _) :: Nil, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")

    val cmp = companionForStaticDefns(staticDefns)

    val companion =
      q"""
        object Second {
          implicit val encodeSecond: _root_.io.circe.Encoder.AsObject[Second] = {
            _root_.io.circe.Encoder.AsObject.instance[Second](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("value", a.value.asJson), ("nested", a.nested.asJson))))
          }
          implicit val decodeSecond: _root_.io.circe.Decoder[Second] = new _root_.io.circe.Decoder[Second] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Second] = for (v0 <- c.downField("value").as[Option[String]]; v1 <- c.downField("nested").as[Option[Second.Nested]]) yield Second(v0, v1) }
          case class Nested(value: Option[Boolean] = None)
          object Nested {
            implicit val encodeNested: _root_.io.circe.Encoder.AsObject[Second.Nested] = {
              _root_.io.circe.Encoder.AsObject.instance[Second.Nested](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("value", a.value.asJson))))
            }
            implicit val decodeNested: _root_.io.circe.Decoder[Second.Nested] = new _root_.io.circe.Decoder[Second.Nested] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Second.Nested] = for (v0 <- c.downField("value").as[Option[Boolean]]) yield Second.Nested(v0) }
          }
        }
       """
    val definition = q"""case class First(value: Option[String] = None, nested: Option[Second.Nested] = None)"""

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }

  test("Optional fields works") {
    val spec =
      s"""
         |swagger: "2.0"
         |info:
         |  title: Whatever
         |  version: 1.0.0
         |host: localhost:1234
         |definitions:
         |  TestObject:
         |    type: object
         |    required:
         |      - required
         |      - required-nullable
         |    properties:
         |      required:
         |        type: string
         |      required-nullable:
         |        type: string
         |        x-nullable: true
         |      optional:
         |        type: string
         |        x-nullable: false
         |      optional-nullable:
         |        type: string
         |        x-nullable: true
         |      legacy:
         |        type: string
         |""".stripMargin
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "http4s")

    val cmp = companionForStaticDefns(staticDefns)

    val companion =
      q"""
        object TestObject {
          implicit val encodeTestObject: _root_.io.circe.Encoder.AsObject[TestObject] = {
            _root_.io.circe.Encoder.AsObject.instance[TestObject](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("required", a.required.asJson), ("required-nullable", a.requiredNullable.asJson), ("legacy", a.legacy.asJson)) ++ a.optional.fold(ifAbsent = None, ifPresent = value => Some("optional" -> value.asJson)) ++ a.optionalNullable.fold(ifAbsent = None, ifPresent = value => Some("optional-nullable" -> value.asJson))))
          }
          implicit val decodeTestObject: _root_.io.circe.Decoder[TestObject] = new _root_.io.circe.Decoder[TestObject] {
            final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[TestObject] = for (v0 <- c.downField("required").as[String]; v1 <- c.downField("required-nullable").as[_root_.io.circe.Json].flatMap(_.as[Option[String]]); v2 <- ((c: _root_.io.circe.HCursor) => c.value.asObject.filter(!_.contains("optional")).fold(c.downField("optional").as[String].map(x => support.Presence.present(x))) {
              _ => _root_.scala.Right(support.Presence.absent)
            })(c); v3 <- ((c: _root_.io.circe.HCursor) => c.value.asObject.filter(!_.contains("optional-nullable")).fold(c.downField("optional-nullable").as[Option[String]].map(x => support.Presence.present(x))) {
              _ => _root_.scala.Right(support.Presence.absent)
            })(c); v4 <- c.downField("legacy").as[Option[String]]) yield TestObject(v0, v1, v2, v3, v4)
          }
        }
       """
    val definition =
      q"""case class TestObject(required: String, requiredNullable: Option[String] = None, optional: support.Presence[String] = support.Presence.Absent, optionalNullable: support.Presence[Option[String]], legacy: Option[String] = None)"""

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }
}
