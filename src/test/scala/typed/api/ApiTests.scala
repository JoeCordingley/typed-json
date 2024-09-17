package typed.api

import utest.*
import org.http4s.HttpApp
import org.http4s.circe.jsonDecoder
import cats.effect.IO
import cats.syntax.all.*
import org.http4s.client.Client
import cats.effect.unsafe.implicits.global
import org.http4s
import org.http4s.implicits.*
import io.circe

object ApiTests extends TestSuite {
  type RootApi =
    Route[
      IO,
      Request[Method.Get, EmptyTuple],
      Response[Status.Ok, Json[String]]
    ]
  val rootApi: RootApi = { case Request(Method.Get, EmptyTuple) =>
    Response(status = Status.Ok, entity = Json("root")).pure[IO]
  }
  type PathApi =
    Route[
      IO,
      Request[Method.Get, "some" *: "path" *: EmptyTuple],
      Response[Status.Ok, Json[String]]
    ]
  val pathApi: PathApi = {
    case Request(Method.Get, "some" *: "path" *: EmptyTuple) =>
      Response(status = Status.Ok, entity = Json("path")).pure[IO]
  }
  type Api = (RootApi, PathApi)
  val api: Api = (rootApi, pathApi)

  val tests = Tests {
    def statusAndEntity(
        response: http4s.Response[IO]
    ): IO[(http4s.Status, circe.Json)] =
      response.as[circe.Json].map((response.status, _))
    test("get root") {
      val (status, entity) =
        Routes
          .fromApi(api)
          .apply(http4s.Request[IO]())
          .flatMap(statusAndEntity)
          .unsafeRunSync()
      assert(status == http4s.Status.Ok)
      assert(entity == circe.Json.fromString("root"))
    }
    test("get path") {
      val (status, entity) =
        Routes
          .fromApi(api)
          .apply(http4s.Request[IO](uri = uri"some/path"))
          .flatMap(statusAndEntity)
          .unsafeRunSync()
      assert(status == http4s.Status.Ok)
      assert(entity == circe.Json.fromString("path"))
    }
  }
}
