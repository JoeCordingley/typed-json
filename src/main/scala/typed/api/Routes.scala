package typed.api

import org.http4s.HttpApp
import org.http4s
import cats.data.Kleisli
import cats.data.OptionT
import cats.*
import cats.implicits.*

type Route[F[_], Method, Path, Status] = Request[Method, Path] => F[Status]

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
  def fromApi[F[
      _
  ]: Monad, Method: FromHttp4s, Path: FromHttp4s, Response: ResponseOf](
      route: Route[F, Method, Path, Response]
  ): HttpApp[F] = requestOf[F, Method, Path]
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
  def apply[F[_]]: Http4sKleisli[F, A]

trait ResponseOf[A]:
  def apply[F[_]](a: A): http4s.Response[F]
