package typed.api

import org.http4s.HttpApp
import org.http4s
import cats.data.Kleisli
import cats.data.OptionT
import cats.*
import cats.implicits.*
import io.circe.syntax.*
import io.circe.Encoder
import org.http4s.EntityEncoder
import org.http4s.circe.jsonEncoderOf

type Route[F[_], Request, Status] = Request => F[Status]

object Method:
  object Get
  type Get = Get.type

case class Request[Method, Path](method: Method, path: Path)
case class Response[Status, Entity](status: Status, entity: Entity)
case class Json[A](value: A)
object Json:
  given jsonEntityEncoder[F[_], A: Encoder]: EntityEncoder[F, Json[A]] =
    jsonEncoderOf[A].contramap[Json[A]](_.value)

object Status:
  case object Ok
  type Ok = Ok.type

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
  given FromHttp4s[EmptyTuple] with
    def apply[F[_]: Monad]: Http4sKleisli[F, EmptyTuple] =
      Kleisli {
        case req if req.uri.path == http4s.Uri.Path.Root =>
          OptionT.some(EmptyTuple)
        case _ => OptionT.none
      }
  given [A <: String: ValueOf, T <: Tuple: FromHttp4s]: FromHttp4s[A *: T] with
    def apply[F[_]: Monad] =
      val a: A = summon[ValueOf[A]].value
      Kleisli { r =>
        r.uri.path.segments.toList match {
          case x :: xs if x == http4s.Uri.Path.Segment(a) =>
            summon[FromHttp4s[T]]
              .apply(r.withPathInfo(http4s.Uri.Path(xs.toVector)))
              .map(a *: _)
          case _ => OptionT.none
        }
      }

trait ResponseOf[A]:
  def apply[F[_]](a: A): http4s.Response[F]

object ResponseOf:
  given [Status: StatusOf, Entity: EntityOf]
      : ResponseOf[Response[Status, Entity]] with
    def apply[F[_]](value: Response[Status, Entity]): http4s.Response[F] =
      value match {
        case Response(status, entity) =>
          http4s.Response(
            status = summon[StatusOf[Status]].apply(status),
            entity = summon[EntityOf[Entity]].apply(entity)
          )
      }

trait StatusOf[A]:
  def apply(a: A): http4s.Status

object StatusOf:
  given StatusOf[Status.Ok] with
    def apply(value: Status.Ok): http4s.Status = http4s.Status.Ok

trait EntityOf[A]:
  def apply[F[_]](a: A): http4s.Entity[F]
object EntityOf:
  given [A: Encoder]: EntityOf[Json[A]] with
    def apply[F[_]](value: Json[A]): http4s.Entity[F] =
      http4s.EntityEncoder[F, Json[A]].toEntity(value)
