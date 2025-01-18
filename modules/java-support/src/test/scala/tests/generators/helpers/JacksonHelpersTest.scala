package tests.generators.helpers

import dev.guardrail.core.LiteralRawType
import dev.guardrail.generators.java.JavaGenerator
import dev.guardrail.generators.java.jackson.JacksonHelpers
import dev.guardrail.{ Target, TargetValue }
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import com.github.javaparser.ast
import com.github.javaparser.ast.expr
import com.github.javaparser.StaticJavaParser
import scala.jdk.CollectionConverters._

class JacksonHelpersTest extends AnyFreeSpec with Matchers {
  private implicit val lt = JavaGenerator()

  implicit class TargetValues[T](private val t: Target[T]) {
    def value: T = t match {
      case TargetValue(v, _) => v
      case _                 => throw new NoSuchElementException(s"Target had no value: $t")
    }
  }

  val BIG_INTEGER_FQ_TYPE = StaticJavaParser.parseClassOrInterfaceType("java.math.BigInteger")
  val BIG_DECIMAL_FQ_TYPE = StaticJavaParser.parseClassOrInterfaceType("java.math.BigDecimal")

  def discriminatorExpression(value: String, tpe: String, fmt: Option[String] = None): Target[ast.Node] =
    JacksonHelpers.discriminatorExpression(
      "discrim",
      value,
      LiteralRawType(Some(tpe), fmt)
    )(
      v => Target.pure[ast.Node](new expr.ObjectCreationExpr(null, BIG_INTEGER_FQ_TYPE, new ast.NodeList(new expr.StringLiteralExpr(v)))),
      v => Target.pure[ast.Node](new expr.ObjectCreationExpr(null, BIG_DECIMAL_FQ_TYPE, new ast.NodeList(new expr.StringLiteralExpr(v)))),
      _ => Target.raiseUserError("foo")
    )

  object StringLiteralExpr {
    def unapply(value: expr.StringLiteralExpr): Some[String] = Some(value.getValue())
  }

  object BooleanLiteralExpr {
    def unapply(value: expr.BooleanLiteralExpr): Some[Boolean] = Some(value.getValue())
  }

  object IntegerLiteralExpr {
    def unapply(value: expr.IntegerLiteralExpr): Some[String] = Some(value.getValue())
  }

  object LongLiteralExpr {
    def unapply(value: expr.LongLiteralExpr): Some[String] = Some(value.getValue())
  }

  object DoubleLiteralExpr {
    def unapply(value: expr.DoubleLiteralExpr): Some[String] = Some(value.getValue())
  }

  object ObjectCreationExpr {
    def unapply(value: ast.Node) = value match {
      case oc: expr.ObjectCreationExpr => Some((oc.getScope(), oc.getType(), oc.getArguments().iterator().asScala.toList))
      case _                           => None
    }
  }

  "Jackson can build a discriminator value expression" - {
    "from a string" in {
      discriminatorExpression("foobar", "string").value match {
        case StringLiteralExpr(foobar) => foobar mustBe "foobar"
        case _                         => fail()
      }
    }

    "from a boolean" in {
      discriminatorExpression("true", "boolean").value match {
        case BooleanLiteralExpr(bool) => bool mustBe true
        case _                        => fail()
      }
    }

    "from an int32" in {
      discriminatorExpression("42", "integer", Some("int32")).value match {
        case IntegerLiteralExpr(int) => int mustBe "42"
        case _                       => fail()
      }
    }

    "from an int64" in {
      discriminatorExpression(Long.MaxValue.toString, "integer", Some("int64")).value match {
        case LongLiteralExpr(long) => long mustBe Long.MaxValue.toString()
        case _                     => fail()
      }
    }

    "from a bigint" in {
      discriminatorExpression("12345678901234567890", "integer").value match {
        case ObjectCreationExpr(_, BIG_INTEGER_FQ_TYPE, List(StringLiteralExpr(bigintStr))) => bigintStr mustBe "12345678901234567890"
        case _                                                                              => fail()
      }
    }

    "from a float" in {
      discriminatorExpression("42.42", "number", Some("float")).value match {
        case DoubleLiteralExpr(floatStr) => floatStr mustBe "42.41999816894531" // Not "42.42" but kinda close enough I guess.
        case _                           => fail()
      }
    }

    "from a double" in {
      discriminatorExpression("42.42", "number", Some("double")).value match {
        case DoubleLiteralExpr(doubleStr) => doubleStr mustBe "42.42" // Why isn't this inaccurate like "from a float"?
        case _                            => fail()
      }
    }

    "from a bigdecimal" in {
      discriminatorExpression("12345678901234567890.0987654321", "number").value match {
        case ObjectCreationExpr(_, BIG_DECIMAL_FQ_TYPE, List(StringLiteralExpr(bigdecStr))) => bigdecStr mustBe "12345678901234567890.0987654321"
        case _                                                                              => fail()
      }
    }
  }

  "Jackson can't build discriminators out of" - {
    "date types" in {
      intercept[NoSuchElementException](discriminatorExpression("2010-04-01T00:00:00Z", "string", Some("date-time")).value)
      intercept[NoSuchElementException](discriminatorExpression("2010-04-01", "string", Some("date")).value)
    }

    "arbitrary numeric types" in {
      intercept[NoSuchElementException](discriminatorExpression("3424", "integer", Some("foobaz")).value)
      intercept[NoSuchElementException](discriminatorExpression("3424.3123", "number", Some("foobaz")).value)
    }

    "random types" in
      intercept[NoSuchElementException](discriminatorExpression("asdsd", "foobaz").value)

    "non-matching string types" in
      intercept[NoSuchElementException](discriminatorExpression("asdasd", "integer", Some("int32")).value)
  }
}
