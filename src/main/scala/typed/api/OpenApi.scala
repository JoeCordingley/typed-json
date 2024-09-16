package typed.api

import typed.json.{JsonObject, MatchesPattern}
import scala.util.matching.Regex
import io.circe.KeyEncoder
import typed.api.OpenApiSchemaCodec.StatusCodePattern

type OpenApiSchemaCodec = JsonObject[
  (
      ("openapi", "3.1.0"),
      ("info", OpenApiSchemaCodec.Info),
      Option[
        (
            "paths",
            OpenApiSchemaCodec.Paths
        )
      ]
  )
]
object OpenApiSchemaCodec:
  type Paths = JsonObject[
    Map[PathPattern, OpenApiSchemaCodec.PathItem]
  ]
  type Info = JsonObject[
    (
        ("title", String),
        ("version", String)
    )
  ]

  type PathItem = JsonObject.Solo[
    Option[("get", Operation)]
  ]
  object PathItem:
    def get(operation: Operation): PathItem =
      JsonObject.Solo(Some("get" -> operation))
  type Operation = JsonObject.Solo[
    Option[("responses", Responses)]
  ]
  type Responses = JsonObject[Map[StatusCodePattern, Response]]
  case class StatusCodePattern(value: String)
  object StatusCodePattern:
    given KeyEncoder[StatusCodePattern] = _.value
    given MatchesPattern[StatusCodePattern] with
      def apply: Regex = "^[1-5](?:[0-9]{2}|XX)$".r
  type Response = JsonObject.Solo[
    ("description", String)
  ]
  def fromOpenApiSchema(info: Info, paths: Paths): OpenApiSchemaCodec =
    JsonObject(
      (
        "openapi" -> "3.1.0",
        "info" -> info,
        Some("paths" -> paths)
      )
    )
  def of[A](info: typed.api.Info)(using
      schema: OpenApiSchemaOf[A]
  ): OpenApiSchemaCodec =
    fromOpenApiSchema(infoCodec(info), pathsCodec(schema.apply))
  def pathsCodec(schema: OpenApiSchema): Paths = JsonObject(schema.paths.map {
    case (path, typed.api.PathItem(method, operation)) =>
      path -> pathItemCodec(method, operationCodec(operation))
  })
  def infoCodec: typed.api.Info => Info = {
    case typed.api.Info(title, version) =>
      JsonObject("title" -> title, "version" -> version)
  }
  def pathItemCodec: (Methods, Operation) => PathItem = {
    case (Methods.Get, operation) => PathItem.get(operation)
  }

  def operationCodec: typed.api.Operation => Operation = {
    case Operation(Statuses.Ok) =>
      JsonObject.Solo(
        Some(
          "responses" -> JsonObject(
            Map(
              StatusCodePattern("200") -> JsonObject.Solo("description" -> "OK")
            )
          )
        )
      )
  }

trait OpenApiSchemaOf[A]:
  def apply: OpenApiSchema

object OpenApiSchemaOf:
  given [F[_], Method: MethodOf, Path: PathPatternOf, Response: OperationOf]
      : OpenApiSchemaOf[Route[F, Request[Method, Path], Response]] with
    def apply: OpenApiSchema = OpenApiSchema(
      Map(
        summon[PathPatternOf[Path]].apply -> PathItem(
          summon[MethodOf[Method]].apply,
          summon[OperationOf[Response]].apply
        )
      )
    )

trait PathPatternOf[A]:
  def apply: PathPattern

object PathPatternOf:
  given PathPatternOf[EmptyTuple] with
    def apply: PathPattern = PathPattern(List.empty)
  given [A <: String: ValueOf, T <: Tuple: PathPatternOf]: PathPatternOf[A *: T]
  with
    def apply: PathPattern = PathPattern(
      summon[ValueOf[A]].value :: summon[PathPatternOf[T]].apply.segments
    )

trait MethodOf[A]:
  def apply: Methods

object MethodOf:
  given MethodOf[Method.Get] with
    def apply: Methods = Methods.Get

trait OperationOf[A]:
  def apply: Operation

object OperationOf:
  given OperationOf[Status.Ok] with
    def apply: Operation = Operation(Statuses.Ok)

case class PathPattern(segments: List[String])
object PathPattern:
  given KeyEncoder[PathPattern] = _.segments.mkString("/", "/", "")
  given MatchesPattern[PathPattern] with
    def apply: Regex = "^/".r

case class OpenApiSchema(paths: Map[PathPattern, PathItem])
case class Info(title: String, version: String)
case class PathItem(method: Methods, operation: Operation)
case class Operation(status: Statuses)
enum Statuses:
  case Ok
enum Methods:
  case Get
