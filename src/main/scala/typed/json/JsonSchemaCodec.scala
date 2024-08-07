package typed.json

import io.circe
import io.circe.Encoder
import cats.data.{Kleisli, Reader}
import cats.syntax.all.*
import io.circe.Json

type JsonSchemaCodec = Fix[JsonSchemaCodec.Unfixed]

object JsonSchemaCodec:
  type CodecMap = JsonObject[Map[String, JsonSchemaCodec]]
  type Unfixed[A] = Either[
    Boolean,
    JsonObject[
      (
          Option[("type", Either[SchemaType, JsonArray[List[SchemaType]]])],
          Option[("properties", JsonObject[Map[String, A]])],
          Option[("required", JsonArray[List[String]])],
          Option[("items", A)],
          Option[("additionalProperties", A)],
          Option[("format", String)],
          Option[("minLength", Int)],
          Option[("maxLength", Int)],
          Option[("anyOf", JsonArray[List[A]])],
          Option[("const", circe.Json)],
          Option[("enum", JsonArray[List[circe.Json]])],
          Option[("$defs", JsonObject[Map[String, A]])],
          Option[("$ref", String)],
      )
    ]
  ]
  val `true`: JsonSchemaCodec = Fix(Left(true))
  def `object`(
      `$defs`: Option[CodecMap],
      `type`: Option[Either[SchemaType, JsonArray[List[SchemaType]]]] = None,
      properties: Option[CodecMap] = None,
      required: Option[JsonArray[List[String]]] = None,
      items: Option[JsonSchemaCodec] = None,
      additionalProperties: Option[JsonSchemaCodec] = None,
      format: Option[String] = None,
      minLength: Option[Int] = None,
      maxLength: Option[Int] = None,
      anyOf: Option[JsonArray[List[JsonSchemaCodec]]] = None,
      const: Option[circe.Json] = None,
      `enum`: Option[JsonArray[List[circe.Json]]] = None,
      `$ref`: Option[String] = None
  ): JsonSchemaCodec = Fix(
    Right(
      JsonObject(
        (
          `type`.map("type" -> _),
          properties.map("properties" -> _),
          required.map("required" -> _),
          items.map("items" -> _),
          additionalProperties.map(
            "additionalProperties" -> _
          ),
          format.map("format" -> _),
          minLength.map("minLength" -> _),
          maxLength.map("maxLength" -> _),
          anyOf.map("anyOf" -> _),
          const.map("const" -> _),
          `enum`.map("enum" -> _),
          `$defs`.map("$defs" -> _),
          `$ref`.map("$ref" -> _)
        )
      )
    )
  )

  def simplyTyped(
      s: SchemaType,
      removeType: Boolean,
      defs: Option[CodecMap]
  ): JsonSchemaCodec =
    if removeType then JsonSchemaCodec.`true`
    else
      JsonSchemaCodec.`object`(
        `$defs` = defs,
        `type` = Some(Left(s))
      )

  def createDefs(defs: Map[String, JsonSchema]): JsonObject[
    Map[String, JsonSchemaCodec]
  ] = JsonObject(defs.view.mapValues(fromSchema).toMap)

  def fromSingular(
      removeType: Boolean,
      maybeDefs: Option[CodecMap]
  ): JsonSchema.Singular => JsonSchemaCodec = {
    case JsonSchema.Singular.String(None, None, None) =>
      simplyTyped(SchemaType.String, removeType, maybeDefs)
    case JsonSchema.Singular.String(format, minLength, maxLength) =>
      JsonSchemaCodec.`object`(
        `$defs` = maybeDefs,
        `type` = if removeType then None else Some(Left(SchemaType.String)),
        format = format,
        minLength = minLength,
        maxLength = maxLength
      )
    case JsonSchema.Singular.True => JsonSchemaCodec.`true`
    case JsonSchema.Singular.Null =>
      simplyTyped(SchemaType.Null, removeType, maybeDefs)
    case JsonSchema.Singular.Integer =>
      simplyTyped(SchemaType.Integer, removeType, maybeDefs)
    case JsonSchema.Singular.Object(None, None, None) if removeType =>
      JsonSchemaCodec.`true`
    case JsonSchema.Singular.Object(
          maybeProperties,
          maybeRequired,
          maybeAdditionalProperties
        ) =>
      JsonSchemaCodec.`object`(
        `$defs` = maybeDefs,
        `type` = if removeType then None else Some(Left(SchemaType.Object)),
        properties = maybeProperties.map(properties =>
          JsonObject(properties.view.mapValues(fromSchema).toMap)
        ),
        required = maybeRequired.map(JsonArray(_)),
        additionalProperties = maybeAdditionalProperties.map(fromSchema)
      )
    case JsonSchema.Singular.Boolean =>
      simplyTyped(SchemaType.Boolean, removeType, maybeDefs)
    case JsonSchema.Singular.Number =>
      simplyTyped(SchemaType.Number, removeType, maybeDefs)
    case JsonSchema.Singular.Array(
          JsonSchema(List(JsonSchema.Singular.True))
        ) =>
      simplyTyped(SchemaType.Array, removeType, maybeDefs)
    case JsonSchema.Singular.Array(
          items
        ) =>
      JsonSchemaCodec.`object`(
        `$defs` = maybeDefs,
        `type` = Some(Left(SchemaType.Array)),
        items = Some(fromSchema(items))
      )
    case JsonSchema.Singular.Const(value) =>
      JsonSchemaCodec.`object`(
        `$defs` = maybeDefs,
        const = Some(value)
      )
    case JsonSchema.Singular.Ref(JsonSchema.Reference.Root(values)) =>
      JsonSchemaCodec.`object`(
        `$defs` = maybeDefs,
        `$ref` = Some(values.mkString("#/", "/", ""))
      )
  }

  def addToSetStrictly[A](s: Set[A], a: A) =
    if (s contains a) None else Some(s + a)

  val fromSchema: JsonSchema => JsonSchemaCodec = fromJsonSchemaWithDefs(
    None
  )

  def fromJsonSchemaWithDefs(
      defs: Option[CodecMap]
  ): JsonSchema => JsonSchemaCodec = {
    type MaybeEncode = Kleisli[Option, JsonSchema, JsonSchemaCodec]
    val single: MaybeEncode = Kleisli {
      case JsonSchema(List(schema)) =>
        Some(fromSingular(removeType = false, defs)(schema))
      case _ => None
    }
    val describedByTypeAlone: MaybeEncode = Kleisli {
      case JsonSchema(schemas) =>
        for {
          types <- schemas.traverse(JsonSchema.Singular.describedByTypeAlone)
          unique <- types.foldM(Set.empty)(addToSetStrictly)
        } yield JsonSchemaCodec.`object`(
          `$defs` = defs,
          `type` = Some(Right(JsonArray(types.toList)))
        )
    }

    val enumerable: MaybeEncode = Kleisli {
      _.anyOf
        .traverse {
          case JsonSchema.Singular.Const(value) => Some(value)
          case _                                => None
        }
        .map(values =>
          JsonSchemaCodec.`object`(
            `$defs` = defs,
            `enum` = Some(JsonArray(values.distinct))
          )
        )
    }

    val usingAnyOf: Reader[JsonSchema, JsonSchemaCodec] =
      Kleisli { case JsonSchema(schemas) =>
        val maybeSingularType = schemas
          .foldM(none[SchemaType]) { case (maybePrevious, singular) =>
            SchemaType.fromSingular(singular).collect {
              case t if maybePrevious.forall(_ == t) => Some(t)
            }
          }
          .flatten
        JsonSchemaCodec.`object`(
          `type` = maybeSingularType.map(Left(_)),
          anyOf = Some(
            JsonArray(
              schemas.map(
                fromSingular(
                  removeType = maybeSingularType.isDefined,
                  maybeDefs = None
                )
              )
            )
          ),
          `$defs` = defs
        )
      }
    ((single <+> describedByTypeAlone <+> enumerable).toReader, usingAnyOf)
      .mapN(_ getOrElse _)
      .run
  }

  def of[A: SchemaOf]: JsonSchemaCodec =
    val (Defs(defs), anyOf) = summon[SchemaOf[A]].apply.run
    JsonSchemaCodec.fromJsonSchemaWithDefs(
      if defs.isEmpty then None else Some(createDefs(defs))
    )(anyOf)

  given encoder(using
      e: => Encoder[Unfixed[Fix[Unfixed]]]
  ): Encoder[JsonSchemaCodec] = e.contramap(_.unfix)
