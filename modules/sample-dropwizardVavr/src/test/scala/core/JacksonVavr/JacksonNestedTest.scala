package core.JacksonVavr

import java.lang.reflect.Modifier

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import polymorphismNested.client.dropwizardVavr.definitions.{ A, B, C, TestResponse }

class JacksonNestedTest extends AnyFreeSpec with Matchers {
  "Jackson nested schemas" - {
    "Should have the nested objects in the right place" in
      new TestResponse.Builder()
        .withEnum1(A.Enum1.B)
        .withEnum2(B.Enum2.D)
        .withObj(new C.Obj.Builder().withValue("foo").build())
        .build()

    "Should have nested classes as public static members" in {
      classOf[C.Obj].getModifiers & Modifier.PUBLIC mustBe Modifier.PUBLIC
      classOf[C.Obj].getModifiers & Modifier.STATIC mustBe Modifier.STATIC
    }

    "Should have nested enums as public non-static members that nonetheless reflect as static" in {
      classOf[A.Enum1].getModifiers & Modifier.PUBLIC mustBe Modifier.PUBLIC
      classOf[A.Enum1].getModifiers & Modifier.STATIC mustBe Modifier.STATIC
    }
  }
}
