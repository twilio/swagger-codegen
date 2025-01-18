package core.Jackson

import examples.client.dropwizard.definitions.{ Category, Pet }
import java.util.Optional
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class JacksonBuilderParamTest extends AnyFreeSpec with Matchers {
  "POJO builders should not accept nulls for required params" in
    assertThrows[NullPointerException] {
      new Pet.Builder(null: String)
        .build()
    }

  "POJO builders should accept nulls for the value variant for optional params" in {
    val pet = new Pet.Builder("fluffy")
      .withCategory(null: Category)
      .build()
    pet.getCategory shouldBe Optional.empty
  }

  "POJO builders should not accept nulls for the Optional<> variant for optional params" in
    assertThrows[NullPointerException] {
      new Pet.Builder("fluffy")
        .withCategory(null: Optional[Category])
        .build()
    }
}
