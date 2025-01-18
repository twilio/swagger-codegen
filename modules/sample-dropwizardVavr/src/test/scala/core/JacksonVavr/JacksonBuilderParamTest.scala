package core.JacksonVavr

import examples.client.dropwizardVavr.definitions.{ Category, Pet }
import io.vavr.control.{ Option => VavrOption }
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
    pet.getCategory shouldBe VavrOption.none
  }

  "POJO builders should not accept nulls for the Optional<> variant for optional params" in
    assertThrows[NullPointerException] {
      new Pet.Builder("fluffy")
        .withCategory(null: VavrOption[Category])
        .build()
    }
}
