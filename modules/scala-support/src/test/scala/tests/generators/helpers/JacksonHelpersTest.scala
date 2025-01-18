package tests.generators.helpers

import dev.guardrail.core.LiteralRawType
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.jackson.JacksonHelpers
import dev.guardrail.{ Target, TargetValue }
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.meta._

class JacksonHelpersTest extends AnyFreeSpec with Matchers {
  private implicit val lt = ScalaGenerator()

  implicit class TargetValues[T](private val t: Target[T]) {
    def value: T = t match {
      case TargetValue(v, _) => v
      case _                 => throw new NoSuchElementException(s"Target had no value: $t")
    }
  }

  def discriminatorExpression(value: String, tpe: String, fmt: Option[String] = None): Target[Term] =
    JacksonHelpers.discriminatorExpression(
      "discrim",
      value,
      LiteralRawType(Some(tpe), fmt)
    )(
      v => Target.pure[Term](q"""BigInt(${Lit.String(v)})"""),
      v => Target.pure[Term](q"""BigDecimal(${Lit.String(v)})"""),
      _ => Target.raiseUserError("foo")
    )

  "Jackson can build a discriminator value expression" - {
    "from a string" in {
      val Lit.String(foobar) = discriminatorExpression("foobar", "string").value
      foobar mustBe "foobar"
    }

    "from a boolean" in {
      val Lit.Boolean(bool) = discriminatorExpression("true", "boolean").value
      bool mustBe true
    }

    "from an int32" in {
      val Lit.Int(int) = discriminatorExpression("42", "integer", Some("int32")).value
      int mustBe 42
    }

    "from an int64" in {
      val Lit.Long(long) = discriminatorExpression(Long.MaxValue.toString, "integer", Some("int64")).value
      long mustBe Long.MaxValue
    }

    "from a bigint" in {
      val Term.Apply.After_4_6_0(Term.Name("BigInt"), Term.ArgClause(List(Lit.String(bigintStr)), None)) =
        discriminatorExpression("12345678901234567890", "integer").value
      bigintStr mustBe "12345678901234567890"
    }

    "from a float" in {
      val Lit.Float(floatStr) = discriminatorExpression("42.42", "number", Some("float")).value
      floatStr mustBe "42.42"
    }

    "from a double" in {
      val Lit.Double(doubleStr) = discriminatorExpression("42.42", "number", Some("double")).value
      doubleStr mustBe "42.42"
    }

    "from a bigdecimal" in {
      val Term.Apply.After_4_6_0(Term.Name("BigDecimal"), Term.ArgClause(List(Lit.String(bigdecStr)), None)) =
        discriminatorExpression("12345678901234567890.0987654321", "number").value
      bigdecStr mustBe "12345678901234567890.0987654321"
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
