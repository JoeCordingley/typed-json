package typed.api

import org.http4s.HttpApp
import org.http4s
import cats.data.Kleisli
import cats.data.OptionT
import cats.*
import cats.implicits.*
import io.circe.syntax.*

type Route[F[_], Request, Status] = Request => F[Status]

object Method:
  object Get
  type Get = Get.type

case class Request[Method, Path](method: Method, path: Path)

object Status:
  case object Ok
  type Ok = Ok.type

object Path:
  case object Root
  type Root = Root.type

object Routes:
  def fromApi[F[_]: Monad, Request: FromHttp4s, Response: ResponseOf](
      route: Route[F, Request, Response]
  ): HttpApp[F] = summon[FromHttp4s[Request]].apply
    .andThen(Kleisli(route).mapK(OptionT.liftK))
    .map(summon[ResponseOf[Response]].apply[F])
    .orNotFound

  def requestOf[F[_]: Monad, Method: FromHttp4s, Path: FromHttp4s](using
      m: FromHttp4s[Method],
      p: FromHttp4s[Path]
  ): Http4sKleisli[F, Request[Method, Path]] =
    (m.apply[F], p.apply[F]).mapN(Request(_, _))

type Http4sKleisli[F[_], A] =
  Kleisli[[X] =>> OptionT[F, X], http4s.Request[F], A]

trait FromHttp4s[A]:
  def apply[F[_]: Monad]: Http4sKleisli[F, A]
object FromHttp4s:
  given [Method, Path](using
      m: FromHttp4s[Method],
      p: FromHttp4s[Path]
  ): FromHttp4s[Request[Method, Path]] with
    def apply[F[_]: Monad]: Http4sKleisli[F, Request[Method, Path]] =
      (m.apply[F], p.apply[F]).mapN(Request(_, _))
  given FromHttp4s[Method.Get] with
    def apply[F[_]: Monad] =
      Kleisli {
        case r if r.method == http4s.Method.GET => OptionT.some(Method.Get)
        case _                                  => OptionT.none
      }
  given FromHttp4s[Path.Root] with
    def apply[F[_]: Monad] =
      Kleisli {
        case req if req.uri.path == http4s.Uri.Path.Root =>
          OptionT.some(Path.Root)
        case _ => OptionT.none
      }

trait ResponseOf[A]:
  def apply[F[_]](a: A): http4s.Response[F]

object ResponseOf:
  given ResponseOf[Status.Ok] with
    def apply[F[_]](value: Status.Ok): http4s.Response[F] =
      http4s.Response(status = http4s.Status.Ok)
