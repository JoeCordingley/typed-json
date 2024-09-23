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

//  given [F[_]: Monad, Request: FromHttp4s, Response: ResponseOf, T <: Tuple](
//      using t: ToRoutes[F, T]
//  ): ToRoutes[F, Route[F, Request, Response] *: T] with
//    def apply(
//        routes: Route[F, Request, Response] *: T
//    ): http4s.HttpRoutes[F] = routes match {
//      case f *: xs =>
//        summon[FromHttp4s[Request]]
//          .apply[F]
//          .andThen(Kleisli(f).mapK(OptionT.liftK))
//          .map(summon[ResponseOf[Response]].apply[F]) <+> t.apply(xs)
//    }

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

//  def requestOf[F[_]: Monad, Method: FromHttp4s, Path: FromHttp4s](using
//      m: FromHttp4s[Method],
//      p: FromHttp4s[Path]
//  ): Http4sKleisli[F, Request[Method, Path]] =
//    (m.apply[F], p.apply[F]).mapN(Request(_, _))

//type Http4sKleisli[F[_], A] =
//  Kleisli[[X] =>> OptionT[F, X], http4s.Request[F], A]
//
//trait FromHttp4s[A]:
//  def apply[F[_]: Monad]: Http4sKleisli[F, A]
//
//object FromHttp4s:
//  given [Method, Path](using
//      m: FromHttp4s[Method],
//      p: FromHttp4s[Path]
//  ): FromHttp4s[Request[Method, Path]] with
//    def apply[F[_]: Monad]: Http4sKleisli[F, Request[Method, Path]] =
//      (m.apply[F], p.apply[F]).mapN(Request(_, _))
//  given FromHttp4s[Method.Get] with
//    def apply[F[_]: Monad] =
//      Kleisli {
//        case r if r.method == http4s.Method.GET => OptionT.some(Method.Get)
//        case _                                  => OptionT.none
//      }
//  given FromHttp4s[EmptyTuple] with
//    def apply[F[_]: Monad]: Http4sKleisli[F, EmptyTuple] =
//      Kleisli {
//        case req if req.uri.path == http4s.Uri.Path.Root =>
//          OptionT.some(EmptyTuple)
//        case _ => OptionT.none
//      }
//  given [A <: String: ValueOf, T <: Tuple: FromHttp4s]: FromHttp4s[A *: T] with
//    def apply[F[_]: Monad] =
//      val a: A = summon[ValueOf[A]].value
//      Kleisli { r =>
//        r.uri.path.segments.toList match {
//          case x :: xs if x == http4s.Uri.Path.Segment(a) =>
//            summon[FromHttp4s[T]]
//              .apply(r.withPathInfo(http4s.Uri.Path(xs.toVector)))
//              .map(a *: _)
//          case _ => OptionT.none
//        }
//      }
//
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
