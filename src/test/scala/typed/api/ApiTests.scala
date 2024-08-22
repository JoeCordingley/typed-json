package typed.api

import utest.*
import org.http4s.HttpApp
import cats.effect.IO
import cats.syntax.all.*
import org.http4s.client.Client
import cats.effect.unsafe.implicits.global
import org.http4s

object ApiTests extends TestSuite {
  val tests = Tests {
    test("get root") {

      type Api = Route[IO, Method.Get, Path.Root, Status.Ok]

      val api: Api = { case Request(Method.Get, Path.Root) =>
        Status.Ok.pure[IO]
      }

      val status = Client
        .fromHttpApp(Routes.fromApi(api))
        .status(http4s.Request[IO]())
        .unsafeRunSync()
      assert(status == http4s.Status.Ok)
    }
  }
}
