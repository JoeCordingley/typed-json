package typed.api

import typed.json.{JsonObject, MatchesPattern, JsonSchemaCodec}
import scala.util.matching.Regex
import io.circe.KeyEncoder
import typed.api.OpenApiSchemaCodec.StatusCodePattern
import typed.json.{SchemaOf, JsonSchema}
import cats.syntax.all.*

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
    Map[PathPattern, PathItem]
  ]
  type Info = JsonObject[
    (
        ("title", String),
        ("version", String)
    )
  ]

  type PathItem = JsonObject[
    (Option[("get", Operation)], Option[("put", Operation)])
  ]
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

  def pathsCodec(schema: OpenApiSchema): Paths = JsonObject(schema.map {
    case (path, pathItem) =>
      path -> pathItemCodec(pathItem)
  })
  def pathItemCodec(m: typed.api.PathItem): PathItem = JsonObject(
    m.get.map(operation => "get" -> operationCodec(operation)),
    m.put.map(operation => "put" -> operationCodec(operation))
  )

  def infoCodec: typed.api.Info => Info = {
    case typed.api.Info(title, version) =>
      JsonObject("title" -> title, "version" -> version)
  }

  def operationCodec: typed.api.Operation => Operation = {
    case Operation(statusCodePattern, description, _) =>
      JsonObject.Solo(
        Some(
          "responses" -> JsonObject(
            Map(
              statusCodePattern -> JsonObject.Solo(
                "description" -> description
              )
            )
          )
        )
      )
  }

trait OpenApiSchemaOf[A]:
  def apply: OpenApiSchema

object OpenApiSchemaOf:
  given OpenApiSchemaOf[EmptyTuple] with
    def apply: OpenApiSchema = Map.empty
  given [
      Path: PathPatternOf,
      PathItem: PathItemOf,
      T <: Tuple: OpenApiSchemaOf
  ]: OpenApiSchemaOf[(Path => PathItem) *: T] with
    def apply: OpenApiSchema =
      summon[OpenApiSchemaOf[T]].apply + (
        summon[PathPatternOf[Path]].apply -> summon[PathItemOf[PathItem]].apply
      )

trait PathItemOf[A]:
  def apply: PathItem

object PathItemOf:
  given PathItemOf[EmptyTuple] with
    def apply: PathItem = PathItem.empty

  given [F[_], Method: MethodOf, O: OperationOf, T <: Tuple: PathItemOf]
      : PathItemOf[(Method, F[O]) *: T] with
    def apply: PathItem = {
      summon[
        MethodOf[Method]
      ].apply match {
        case Methods.Get => PathItem.setGet
        case Methods.Put => PathItem.setPut
      }
    }.apply(summon[OperationOf[O]].apply, summon[PathItemOf[T]].apply)

trait PathPatternOf[A]:
  def apply: PathPattern

object PathPatternOf:
  given PathPatternOf[EmptyTuple] with
    def apply: PathPattern = PathPattern(List.empty)
  given fixedPath[A <: String: ValueOf, T <: Tuple: PathPatternOf]
      : PathPatternOf[A *: T] with
    def apply: PathPattern = PathPattern(
      summon[ValueOf[A]].value :: summon[PathPatternOf[T]].apply.segments
    )
  given pathParam[A <: String: ValueOf, T <: Tuple: PathPatternOf]
      : PathPatternOf[PathParam[A] *: T] with
    def apply: PathPattern = PathPattern(
      summon[ValueOf[A]].value :: summon[PathPatternOf[T]].apply.segments
    )

trait MethodOf[A]:
  def apply: Methods

object MethodOf:
  given MethodOf[Method.Get] with
    def apply: Methods = Methods.Get
  given MethodOf[Method.Put] with
    def apply: Methods = Methods.Put

trait OperationOf[A]:
  def apply: Operation

object OperationOf:
  given [
      Status: StatusCodePatternOf,
      Description <: String: ValueOf,
      Content: ContentOf
  ]: OperationOf[Response[Status, Description, Content]] with
    def apply: Operation = Operation(
      summon[StatusCodePatternOf[Status]].apply,
      summon[ValueOf[Description]].value,
      summon[ContentOf[Content]].apply
    )

trait StatusCodePatternOf[A]:
  def apply: StatusCodePattern

object StatusCodePatternOf:
  given StatusCodePatternOf[Status.Ok] with
    def apply: StatusCodePattern = StatusCodePattern("200")

trait ContentOf[A]:
  def apply: Option[Content]

object ContentOf:
  given ContentOf[Empty] with
    def apply: Option[Content] = None
  given [A: SchemaOf]: ContentOf[Json[A]] with
    def apply: Option[Content] = Some(Content.Json(JsonSchemaCodec.of[A]))

enum Content:
  case Json(schema: JsonSchemaCodec)

case class PathPattern(segments: List[String])
object PathPattern:
  given KeyEncoder[PathPattern] = _.segments.mkString("/", "/", "")
  given MatchesPattern[PathPattern] with
    def apply: Regex = "^/".r

type OpenApiSchema = Map[PathPattern, PathItem]
case class PathItem(get: Option[Operation], put: Option[Operation])
object PathItem:
  def empty: PathItem = PathItem(None, None)
  def setGet(operation: Operation, pathItem: PathItem): PathItem =
    pathItem.copy(get = Some(operation))
  def setPut(operation: Operation, pathItem: PathItem): PathItem =
    pathItem.copy(put = Some(operation))
case class Info(title: String, version: String)
case class Operation(
    status: StatusCodePattern,
    description: String,
    content: Option[Content]
)

enum Methods:
  case Get
  case Put
