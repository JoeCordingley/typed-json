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
    EmptyTuple => (Method.Get, IO[Response[Status.Ok, "Ok", Json[String]]]) *:
      EmptyTuple

  val rootApi: RootApi = { case EmptyTuple =>
    (
      Method.Get,
      Response(status = Status.Ok, entity = Json("root")).pure[IO]
    ) *: EmptyTuple
  }

  type PathApi =
    "some" *: "path" *: EmptyTuple => (
        Method.Get,
        IO[Response[Status.Ok, "Ok", Json[String]]]
    ) *: EmptyTuple
  val pathApi: PathApi = { case "some" *: "path" *: EmptyTuple =>
    (
      Method.Get,
      Response(status = Status.Ok, entity = Json("path")).pure[IO]
    ) *: EmptyTuple
  }
//  type Api = (RootApi, PathApi)
//  val api: Api = (rootApi, pathApi)

  val tests = Tests {
    def statusAndEntity(
        response: http4s.Response[IO]
    ): IO[(http4s.Status, circe.Json)] =
      response.as[circe.Json].map((response.status, _))
    test("get root") {
      val (status, entity) =
        Routes
          .fromApi(rootApi *: EmptyTuple)
          .apply(http4s.Request[IO]())
          .flatMap(statusAndEntity)
          .unsafeRunSync()
      assert(status == http4s.Status.Ok)
      assert(entity == circe.Json.fromString("root"))
    }
    test("get path") {
      val (status, entity) =
        Routes
          .fromApi(pathApi *: EmptyTuple)
          .apply(http4s.Request[IO](uri = uri"some/path"))
          .flatMap(statusAndEntity)
          .unsafeRunSync()
      assert(status == http4s.Status.Ok)
      assert(entity == circe.Json.fromString("path"))
    }
  }
}
