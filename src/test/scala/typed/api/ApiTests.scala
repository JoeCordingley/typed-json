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
      (Method.Put, IO[Response[Status.Ok, "Ok", Json[String]]]) *: EmptyTuple

  val rootApi: RootApi = { case EmptyTuple =>
    (
      Method.Get -> Response(status = Status.Ok, entity = Json("get root"))
        .pure[IO],
      Method.Put -> Response(status = Status.Ok, entity = Json("put root"))
        .pure[IO]
    )
  }

  type PathApi =
    "some" *: "path" *: EmptyTuple => (
        Method.Get,
        IO[Response[Status.Ok, "Ok", Json[String]]]
    ) *: EmptyTuple
  val pathApi: PathApi = { case ("some", "path") =>
    (
      Method.Get,
      Response(status = Status.Ok, entity = Json("path")).pure[IO]
    ) *: EmptyTuple
  }
  type Api = (RootApi, PathApi)
  val api: Api = (rootApi, pathApi)

  type PathParamApi =
    (
        "path" *: PathParam["param"] *: EmptyTuple => (
            Method.Get,
            IO[Response[Status.Ok, "Ok", Json[String]]]
        ) *: EmptyTuple
    ) *: EmptyTuple

  val pathParamApi: PathParamApi = {
    case "path" *: PathParam(path) *: EmptyTuple =>
      (Method.Get, Response(status = Status.Ok, entity = Json(path)).pure[IO])
        *: EmptyTuple
  } *: EmptyTuple

  val tests = Tests {
    test("get root") {
      val t = for {
        response <- Routes
          .fromApi(api)
          .apply(http4s.Request[IO]())
        _ = assert(response.status == http4s.Status.Ok)
        entity <- response.as[circe.Json]
      } yield {
        entity ==> circe.Json.fromString("get root")
      }
      t.unsafeRunSync()
    }
    test("get path") {
      val t = for {
        response <- Routes
          .fromApi(api)
          .apply(http4s.Request[IO](uri = uri"some/path"))
        _ = assert(response.status == http4s.Status.Ok)
        entity <- response.as[circe.Json]
      } yield {
        entity ==> circe.Json.fromString("path")
      }
      t.unsafeRunSync()
    }
    test("put root") {
      val t = for {
        response <- Routes
          .fromApi(api)
          .apply(http4s.Request[IO](method = http4s.Method.PUT))
        _ = assert(response.status == http4s.Status.Ok)
        entity <- response.as[circe.Json]
      } yield {
        entity ==> circe.Json.fromString("put root")
      }
      t.unsafeRunSync()
    }
    test("path param") {
      val t = for {
        response <- Routes
          .fromApi(pathParamApi)
          .apply(
            http4s.Request[IO](method = http4s.Method.GET, uri = uri"path/test")
          )
        _ = assert(response.status == http4s.Status.Ok)
        entity <- response.as[circe.Json]
      } yield {
        entity ==> circe.Json.fromString("test")
      }
      t.unsafeRunSync()
    }
  }
}
