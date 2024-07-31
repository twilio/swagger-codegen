package dev.guardrail.generators.spi

import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import dev.guardrail.Target
import dev.guardrail.terms.ProtocolTerms
import dev.guardrail.generators.java
import dev.guardrail.generators.java.JavaCollectionsGenerator
import dev.guardrail.generators.java.JavaGenerator
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.generators.java.JavaVavrCollectionsGenerator
import dev.guardrail.generators.java.asyncHttpClient.AsyncHttpClientClientGenerator
import dev.guardrail.generators.java.dropwizard.DropwizardGenerator
import dev.guardrail.generators.java.dropwizard.DropwizardServerGenerator
import dev.guardrail.generators.java.jackson.JacksonGenerator
import dev.guardrail.generators.java.springMvc.SpringMvcGenerator
import dev.guardrail.generators.java.springMvc.SpringMvcServerGenerator
import dev.guardrail.generators.scala
import dev.guardrail.generators.scala.ScalaCollectionsGenerator
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.scala.akkaHttp.AkkaHttpClientGenerator
import dev.guardrail.generators.scala.akkaHttp.AkkaHttpGenerator
import dev.guardrail.generators.scala.akkaHttp.AkkaHttpServerGenerator
import dev.guardrail.generators.scala.circe.CirceProtocolGenerator
import dev.guardrail.generators.scala.http4s.Http4sClientGenerator
import dev.guardrail.generators.scala.http4s.Http4sGenerator
import dev.guardrail.generators.scala.http4s.Http4sServerGenerator
import dev.guardrail.generators.scala.zioHttp.ZioHttpServerGenerator
import dev.guardrail.languages.LA

class SpiTest extends AnyFunSuite with Matchers {
  implicit class ModuleLoadResultSyntax[A](value: ModuleLoadResult[A]) {
    def valueOr(func: ModuleLoadResult[Nothing] => A): A = value match {
      case fail: ModuleLoadFailed => func(fail)
      case succ: ModuleLoadSuccess[A] => succ.result
      case conflict: ModuleLoadConflict => func(conflict)
    }
  }
  def testLanguageLoader[L <: LA, B](label: String, params: Set[String])(implicit tt: TypeTag[L], ct: ClassTag[B], pos: Position): Unit =
    LanguageLoader
      .load[L](params)
      .valueOr(err => fail(err.toString()))
      .shouldBe(a[B])

  test("LanguageLoader > ScalaLanguage") {
    testLanguageLoader[ScalaLanguage, ScalaGenerator]("ScalaGenerator", Set("scala-language"))
  }

  test("LanguageLoader > JavaLanguage") {
    testLanguageLoader[JavaLanguage, JavaGenerator]("JavaGenerator", Set("java-language"))
  }

  def testCollectionsGeneratorLoader[L <: LA, B](label: String, params: Set[String])(implicit tt: TypeTag[L], ct: ClassTag[B], pos: Position): Unit =
    CollectionsGeneratorLoader
      .load[L](params)
      .valueOr(err => fail(err.toString()))
      .shouldBe(a[B])

  test("CollectionsGeneratorLoader: Scala") {
    testCollectionsGeneratorLoader[ScalaLanguage, ScalaCollectionsGenerator]("ScalaCollectionsGenerator", Set("scala-stdlib"))
  }

  test("CollectionsGeneratorLoader: Java") {
    testCollectionsGeneratorLoader[JavaLanguage, JavaCollectionsGenerator]("JavaCollectionsGenerator", Set("java-stdlib"))
    testCollectionsGeneratorLoader[JavaLanguage, JavaVavrCollectionsGenerator]("JavaVavrCollectionsGenerator", Set("java-vavr"))
  }

  def testFrameworkLoader[L <: LA](label: String, params: Set[String])(implicit tt: TypeTag[L], pos: Position): Unit =
    FrameworkLoader
      .load[L](params)
      .valueOr(err => fail(err.toString()))

  def testModuleMapperLoader[L <: LA](name: String)(implicit tt: TypeTag[L], pos: Position): Unit = {
    val label = s"ModuleMapperLoader > $name"
    val params = ModuleMapperLoader.load[L](name).valueOr(err => fail(err.toString()))
    testFrameworkLoader[L](label, params)
  }

  test("FrameworkLoader: Scala") {
    testModuleMapperLoader[ScalaLanguage]("akka-http")
    testModuleMapperLoader[ScalaLanguage]("http4s")
    testModuleMapperLoader[ScalaLanguage]("dropwizard")

    testFrameworkLoader[ScalaLanguage]("akka-http", Set("akka-http", "circe"))
  }

  test("FrameworkLoader: Java") {
    testModuleMapperLoader[JavaLanguage]("dropwizard")
    testModuleMapperLoader[JavaLanguage]("spring-mvc")

    testFrameworkLoader[JavaLanguage]("dropwizard-vavr", Set("dropwizard", "jackson", "java-stdlib", "async-http-client"))
    testFrameworkLoader[JavaLanguage]("dropwizard-vavr", Set("dropwizard", "jackson", "java-vavr", "async-http-client"))
  }

  def testClientGeneratorLoader[L <: LA, B](label: String, params: Set[String])(implicit tt: TypeTag[L], ct: ClassTag[B], pos: Position): Unit =
    ClientGeneratorLoader
      .load[L](params)
      .valueOr(err => fail(err.toString()))
      .shouldBe(a[B])

  test("ClientGeneratorLoader: Scala") {
    testClientGeneratorLoader[ScalaLanguage, AkkaHttpClientGenerator]("akka-http", Set("akka-http", "circe"))
    testClientGeneratorLoader[ScalaLanguage, AkkaHttpClientGenerator]("akka-http", Set("akka-http", "jackson"))
    testClientGeneratorLoader[ScalaLanguage, Http4sClientGenerator]("http4s", Set("http4s"))
  }

  test("ClientGeneratorLoader: Java") {
    testClientGeneratorLoader[JavaLanguage, AsyncHttpClientClientGenerator]("async-http-client", Set("async-http-client", "java-stdlib"))
    testClientGeneratorLoader[JavaLanguage, AsyncHttpClientClientGenerator]("async-http-client", Set("async-http-client", "java-vavr"))
  }

  def testServerGeneratorLoader[L <: LA, B](label: String, params: Set[String])(implicit tt: TypeTag[L], ct: ClassTag[B], pos: Position): Unit =
    ServerGeneratorLoader
      .load[L](params)
      .valueOr(err => fail(err.toString()))
      .shouldBe(a[B])

  test("ServerGeneratorLoader: Scala") {
    testServerGeneratorLoader[ScalaLanguage, AkkaHttpServerGenerator]("akka-http", Set("akka-http", "circe"))
    testServerGeneratorLoader[ScalaLanguage, Http4sServerGenerator]("http4s", Set("http4s"))
    testServerGeneratorLoader[ScalaLanguage, ZioHttpServerGenerator]("zio-http", Set("zio-http"))
  }

  test("ServerGeneratorLoader: Java") {
    testServerGeneratorLoader[JavaLanguage, DropwizardServerGenerator]("dropwizard", Set("dropwizard", "java-stdlib"))
    testServerGeneratorLoader[JavaLanguage, SpringMvcServerGenerator]("spring-mvc", Set("spring-mvc", "java-stdlib"))
  }

  def testFrameworkGeneratorLoader[L <: LA, B](label: String, params: Set[String])(implicit tt: TypeTag[L], ct: ClassTag[B], pos: Position): Unit =
    FrameworkGeneratorLoader
      .load[L](params)
      .valueOr(err => fail(err.toString()))
      .shouldBe(a[B])

  test("FrameworkGeneratorLoader: Java") {
    testFrameworkGeneratorLoader[JavaLanguage, DropwizardGenerator]("dropwizard", Set("dropwizard", "java-stdlib"))
    testFrameworkGeneratorLoader[JavaLanguage, SpringMvcGenerator]("spring-mvc", Set("spring-mvc", "java-stdlib"))
  }

  test("FrameworkGeneratorLoader: Scala") {
    testFrameworkGeneratorLoader[ScalaLanguage, AkkaHttpGenerator]("akka-http", Set("akka-http", "circe", "scala-stdlib"))
    testFrameworkGeneratorLoader[ScalaLanguage, Http4sGenerator]("spring-mvc", Set("http4s", "scala-stdlib"))
  }

  def testProtocolGeneratorLoader[L <: LA, B](label: String, params: Set[String])(implicit tt: TypeTag[L], ct: ClassTag[B], pos: Position): Unit =
    ProtocolGeneratorLoader
      .load[L](params)
      .valueOr(err => fail(err.toString()))
      .shouldBe(a[B])

  test("ProtocolGeneratorLoader: Java") {
    testProtocolGeneratorLoader[JavaLanguage, JacksonGenerator]("dropwizard", Set("jackson", "java-stdlib"))
  }

  test("ProtocolGeneratorLoader: Scala") {
    testProtocolGeneratorLoader[ScalaLanguage, CirceProtocolGenerator]("circe", Set("circe"))
    testProtocolGeneratorLoader[ScalaLanguage, ProtocolTerms[ScalaLanguage, Target]]("jackson", Set("jackson"))
  }
}
