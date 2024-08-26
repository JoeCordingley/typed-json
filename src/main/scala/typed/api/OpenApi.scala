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
            JsonObject[
              Map[OpenApiSchemaCodec.PathPattern, OpenApiSchemaCodec.PathItem]
            ]
        )
      ]
  )
]
object OpenApiSchemaCodec:
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
  def fromOpenApiSchema: OpenApiSchema => OpenApiSchemaCodec = ???
  def of[A](info: typed.api.Info)(using
      schema: OpenApiSchemaOf[A]
  ): OpenApiSchemaCodec =
    fromOpenApiSchema(schema.apply)

trait OpenApiSchemaOf[A]:
  def apply: OpenApiSchema

object OpenApiSchemaOf:
  given [F[_], Request, Response]: OpenApiSchemaOf[Route[F, Request, Response]]
  with
    def apply: OpenApiSchema = OpenApiSchema()

case class OpenApiSchema()
case class Info(title: String, version: String)
