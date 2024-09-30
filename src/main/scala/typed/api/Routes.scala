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

type Route[F[_], Request, Response] = Request => F[Response]

object Method:
  object Get
  type Get = Get.type
  object Put
  type Put = Put.type

case class Request[Method, Path](method: Method, path: Path)
case class Response[Status, Description, Entity](status: Status, entity: Entity)
case class Json[A](value: A)
case object Empty
type Empty = Empty.type

object Json:
  given jsonEntityEncoder[F[_], A: Encoder]: EntityEncoder[F, Json[A]] =
    jsonEncoderOf[A].contramap[Json[A]](_.value)

object Status:
  case object Ok
  type Ok = Ok.type

trait ToRoutes[F[_], A]:
  def apply(routes: A): http4s.HttpRoutes[F]

object ToRoutes:
  given [F[_]: Applicative]: ToRoutes[F, EmptyTuple] with
    def apply(routes: EmptyTuple): http4s.HttpRoutes[F] =
      http4s.HttpRoutes.empty

  given [F[_]: Monad, Path: PathOf, PathItems, T <: Tuple](using
      t: ToRoutes[F, T],
      p: PathItemsOf[F, PathItems]
  ): ToRoutes[F, (Path => PathItems) *: T] with
    def apply(
        routes: (Path => PathItems) *: T
    ): http4s.HttpRoutes[F] = routes match {
      case f *: xs =>
        Kleisli[[X] =>> OptionT[F, X], http4s.Request[F], http4s.Response[F]] {
          case request =>
            OptionT
              .fromOption(
                summon[PathOf[Path]]
                  .apply(request.uri.path)
              )
              .flatMap(path => p.apply(request.method, f(path)))
        } <+> t.apply(xs)
    }
trait PathOf[A]:
  def apply(uri: http4s.Uri.Path): Option[A]
object PathOf:
  given PathOf[EmptyTuple] = p =>
    if p.segments.isEmpty then Some(EmptyTuple) else None
  given [A <: String: ValueOf, T <: Tuple: PathOf]: PathOf[A *: T] =
    _.segments.toList match {
      case segment :: segments =>
        val a = summon[ValueOf[A]].value
        if http4s.Uri.Path.Segment(a) == segment then
          summon[PathOf[T]]
            .apply(http4s.Uri.Path(segments.toVector))
            .map(a *: _)
        else None
      case _ => None
    }
  given [A, T <: Tuple: PathOf]: PathOf[PathParam[A] *: T] =
    _.segments.toList match {
      case segment :: segments =>
        summon[PathOf[T]]
          .apply(http4s.Uri.Path(segments.toVector))
          .map(PathParam(segment.toString) *: _)
      case _ => None
    }
case class PathParam[Label](value: String)

trait PathItemsOf[F[_], A]:
  def apply(http4sMethod: http4s.Method, p: A): OptionT[F, http4s.Response[F]]

object PathItemsOf:
  given [F[_]: Applicative]: PathItemsOf[F, EmptyTuple] = { case _ =>
    OptionT.none
  }
  given [F[_]: Functor, Method: MatchesHttp4sMethod, Response, T <: Tuple](using
      ps: PathItemsOf[F, T],
      rs: ResponseOf[F, Response]
  ): PathItemsOf[F, (Method, F[Response]) *: T] with
    def apply(
        http4sMethod: http4s.Method,
        p: (Method, F[Response]) *: T
    ): OptionT[F, http4s.Response[F]] = p match {
      case (_, response) *: _
          if summon[MatchesHttp4sMethod[Method]].apply(http4sMethod) =>
        OptionT.liftF(response.map(rs.apply))
      case _ *: t => ps.apply(http4sMethod, t)
    }

trait MatchesHttp4sMethod[A]:
  def apply(method: http4s.Method): Boolean

object MatchesHttp4sMethod:
  given MatchesHttp4sMethod[Method.Get] = _ == http4s.Method.GET
  given MatchesHttp4sMethod[Method.Put] = _ == http4s.Method.PUT
object Routes:
  def fromApi[F[_]: Applicative, A](routes: A)(using t: ToRoutes[F, A]) =
    t.apply(routes).orNotFound

trait ResponseOf[F[_], A]:
  def apply(a: A): http4s.Response[F]

object ResponseOf:
  given [F[_], Status: StatusOf, Entity: EntityOf, Description]
      : ResponseOf[F, Response[Status, Description, Entity]] with
    def apply(
        value: Response[Status, Description, Entity]
    ): http4s.Response[F] =
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
