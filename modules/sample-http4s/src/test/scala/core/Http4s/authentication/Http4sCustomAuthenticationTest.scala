package core.Http4s.authentication

import _root_.authenticationCustom.client.{ http4s => cdefs }
import _root_.authenticationCustom.server.http4s.auth.AuthHandler
import _root_.authenticationCustom.server.http4s.auth.AuthResource
import _root_.authenticationCustom.server.http4s.auth.AuthResource._
import authenticationCustom.client.http4s.auth.AuthClient
import cats.data._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe.Json
import io.circe.Printer
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.http4s.syntax.StringSyntax
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Http4sCustomAuthenticationTest extends AnyFunSuite with Matchers with EitherValues with StringSyntax {

  test("provide context to handler") {
    type AuthContext = Either[Unit, Int]

    val authMiddleware = (_: NonEmptyList[NonEmptyMap[AuthResource.AuthSchemes, Set[String]]], _: Boolean, _: Request[IO]) => IO.pure(Right(42))

    val server: HttpRoutes[IO] = new AuthResource[IO, AuthContext](authMiddleware).routes(new AuthHandler[IO, AuthContext] {
      override def doBar(respond: DoBarResponse.type)(p1: String, body: String): IO[DoBarResponse] = ???

      override def doBaz(respond: DoBazResponse.type)(authContext: AuthContext, body: String): IO[DoBazResponse] = ???

      override def doFoo(respond: DoFooResponse.type)(authContext: AuthContext, body: String): IO[DoFooResponse] =
        authContext.fold(_ => IO(DoFooResponse.Ok("authentication failed")), ctx => IO(DoFooResponse.Ok(ctx.toString() + body)))
    })

    val client = Client.fromHttpApp(server.orNotFound)

    val retrieved =
      client
        .run(
          Request[IO](method = Method.POST, uri = Uri.unsafeFromString("/foo"))
            .withBodyStream(fs2.Stream.apply("\"-97-\"".getBytes(): _*))
            .withContentType(`Content-Type`(MediaType.application.json))
        )
        .use(_.bodyText.compile.string)
        .attempt
        .unsafeRunSync()
        .value

    retrieved shouldEqual "\"42-97-\""
  }

  test("process authentication error") {
    type AuthContext = Either[String, Int]

    val authMiddleware = (_: NonEmptyList[NonEmptyMap[AuthResource.AuthSchemes, Set[String]]], _: Boolean, _: Request[IO]) => IO.pure(Left("custom-failure"))

    val server: HttpRoutes[IO] = new AuthResource[IO, AuthContext](authMiddleware).routes(new AuthHandler[IO, AuthContext] {
      override def doBar(respond: DoBarResponse.type)(p1: String, body: String): IO[DoBarResponse] = ???

      override def doBaz(respond: DoBazResponse.type)(authContext: AuthContext, body: String): IO[DoBazResponse] = ???

      override def doFoo(respond: DoFooResponse.type)(authContext: AuthContext, body: String): IO[DoFooResponse] =
        authContext.fold(f => IO(DoFooResponse.Ok(f)), ctx => IO(DoFooResponse.Ok(ctx.toString() + body)))
    })

    val client = Client.fromHttpApp(server.orNotFound)

    val retrieved =
      client
        .run(
          Request[IO](method = Method.POST, uri = Uri.unsafeFromString("/foo"))
            .withBodyStream(fs2.Stream.apply("\"\"".getBytes(): _*))
            .withContentType(`Content-Type`(MediaType.application.json))
        )
        .use(_.bodyText.compile.string)
        .attempt
        .unsafeRunSync()
        .value

    retrieved shouldEqual "\"custom-failure\""
  }

  test("provide security requirements to authentication") {
    type AuthContext = Either[Unit, Int]
    import AuthResource.AuthSchemes

    val authMiddleware = (config: NonEmptyList[NonEmptyMap[AuthSchemes, Set[String]]], _: Boolean, _: Request[IO]) =>
      IO.pure {
        val c = config.toList.map(_.toSortedMap.toMap)

        if (c == List(
              Map(AuthSchemes.Jwt    -> Set("foo:read", "bar:write"), AuthSchemes.Basic -> Set("bar:basic")),
              Map(AuthSchemes.OAuth2 -> Set("oauth:scope"))
            ))
          Right(1)
        else
          Left(())
      }

    val server: HttpRoutes[IO] = new AuthResource[IO, AuthContext](authMiddleware).routes(new AuthHandler[IO, AuthContext] {
      override def doBar(respond: DoBarResponse.type)(p1: String, body: String): IO[DoBarResponse] = ???

      override def doBaz(respond: DoBazResponse.type)(authContext: AuthContext, body: String): IO[DoBazResponse] = ???

      override def doFoo(respond: DoFooResponse.type)(authContext: AuthContext, body: String): IO[DoFooResponse] =
        authContext.fold(_ => IO(DoFooResponse.Ok("test failed")), ctx => IO(DoFooResponse.Ok("test succeed")))
    })

    val client = Client.fromHttpApp(server.orNotFound)

    val retrieved =
      client
        .run(
          Request[IO](method = Method.POST, uri = Uri.unsafeFromString("/foo"))
            .withBodyStream(fs2.Stream.apply("\"\"".getBytes(): _*))
            .withContentType(`Content-Type`(MediaType.application.json))
        )
        .use(_.bodyText.compile.string)
        .attempt
        .unsafeRunSync()
        .value

    retrieved shouldEqual "\"test succeed\""
  }

  test("provide optional security requirement to the middleware") {
    type AuthContext = Either[Unit, Unit]
    import AuthResource.AuthSchemes

    val authMiddleware = (_: NonEmptyList[NonEmptyMap[AuthSchemes, Set[String]]], optional: Boolean, _: Request[IO]) =>
      IO.pure(
        if (optional) Right(())
        else Left(())
      )

    val server: HttpRoutes[IO] = new AuthResource[IO, AuthContext](authMiddleware).routes(new AuthHandler[IO, AuthContext] {
      override def doBar(respond: DoBarResponse.type)(p1: String, body: String): IO[DoBarResponse] = ???

      override def doBaz(respond: DoBazResponse.type)(authContext: AuthContext, body: String): IO[DoBazResponse] =
        authContext.fold(_ => IO(DoBazResponse.Ok("non optional")), ctx => IO(DoBazResponse.Ok("optional")))

      override def doFoo(respond: DoFooResponse.type)(authContext: AuthContext, body: String): IO[DoFooResponse] =
        authContext.fold(_ => IO(DoFooResponse.Ok("non optional")), ctx => IO(DoFooResponse.Ok("optional")))
    })

    val client = Client.fromHttpApp(server.orNotFound)

    def request(path: String) =
      client
        .run(
          Request[IO](method = Method.POST, uri = Uri.unsafeFromString(path))
            .withBodyStream(fs2.Stream.apply("\"\"".getBytes(): _*))
            .withContentType(`Content-Type`(MediaType.application.json))
        )
        .use(_.bodyText.compile.string)
        .attempt
        .unsafeRunSync()
        .value

    request("/foo") shouldEqual "\"non optional\""
    request("/baz") shouldEqual "\"optional\""
  }
}
