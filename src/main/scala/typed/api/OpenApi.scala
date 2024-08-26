package typed.api

import typed.json.{JsonObject, MatchesPattern}
import scala.util.matching.Regex
import io.circe.KeyEncoder

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
    Map[OpenApiSchemaCodec.PathPattern, OpenApiSchemaCodec.PathItem]
  ]
  type Info = JsonObject[
    (
        ("title", String),
        ("version", String)
    )
  ]
  case class PathPattern(value: String)
  object PathPattern:
    given KeyEncoder[PathPattern] = _.value
    given MatchesPattern[PathPattern] with
      def apply: Regex = "^/".r

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
      PathPattern(path) -> pathItemCodec(method, operationCodec(operation))
  })
  def infoCodec: typed.api.Info => Info = {
    case typed.api.Info(title, version) =>
      JsonObject("title" -> title, "version" -> version)
  }
  def pathItemCodec: (Methods, Operation) => PathItem = {
    case (Methods.Get, operation) => PathItem.get(operation)
  }

  def operationCodec: typed.api.Operation => Operation = ???

trait OpenApiSchemaOf[A]:
  def apply: OpenApiSchema

object OpenApiSchemaOf:
  given [F[_], Request, Response]: OpenApiSchemaOf[Route[F, Request, Response]]
  with
    def apply: OpenApiSchema = OpenApiSchema(Map.empty)

case class OpenApiSchema(paths: Map[String, PathItem])
case class Info(title: String, version: String)
case class PathItem(method: Methods, operation: Operation)
case class Operation()
enum Methods:
  case Get
