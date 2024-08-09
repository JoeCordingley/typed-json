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
          Option[("$schema", String)],
          Option[("type", Either[SchemaType, JsonArray[List[SchemaType]]])],
          Option[("properties", JsonObject[Map[String, A]])],
          Option[("required", JsonArray[List[String]])],
          Option[("prefixItems", JsonArray[List[A]])],
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
  val `false`: JsonSchemaCodec = Fix(Left(false))
  def `object`(
      `$schema`: Option[String],
      `$defs`: Option[CodecMap],
      `type`: Option[Either[SchemaType, JsonArray[List[SchemaType]]]] = None,
      properties: Option[CodecMap] = None,
      required: Option[JsonArray[List[String]]] = None,
      prefixItems: Option[JsonArray[List[JsonSchemaCodec]]] = None,
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
          `$schema`.map("$schema" -> _),
          `type`.map("type" -> _),
          properties.map("properties" -> _),
          required.map("required" -> _),
          prefixItems.map("prefixItems" -> _),
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

  def createDefs(defs: Map[String, JsonSchema]): JsonObject[
    Map[String, JsonSchemaCodec]
  ] = JsonObject(defs.view.mapValues(fromSchema).toMap)

  def fromSingular(
      metaSchema: Option[String],
      removeType: Boolean,
      maybeDefs: Option[CodecMap],
      singular: JsonSchema.Singular
  ): JsonSchemaCodec = {
    def simplyTyped(
        s: SchemaType
    ): JsonSchemaCodec =
      if removeType then JsonSchemaCodec.`true`
      else
        JsonSchemaCodec.`object`(
          `$schema` = metaSchema,
          `$defs` = maybeDefs,
          `type` = Some(Left(s))
        )
    singular match {
      case JsonSchema.Singular.String(None, None, None) =>
        simplyTyped(SchemaType.String)
      case JsonSchema.Singular.String(format, minLength, maxLength) =>
        JsonSchemaCodec.`object`(
          `$schema` = metaSchema,
          `$defs` = maybeDefs,
          `type` = if removeType then None else Some(Left(SchemaType.String)),
          format = format,
          minLength = minLength,
          maxLength = maxLength
        )
      case JsonSchema.Singular.True =>
        if (metaSchema.nonEmpty || maybeDefs.nonEmpty) then
          JsonSchemaCodec.`object`(`$schema` = metaSchema, `$defs` = maybeDefs)
        else JsonSchemaCodec.`true`
      case JsonSchema.Singular.Null =>
        simplyTyped(SchemaType.Null)
      case JsonSchema.Singular.Integer =>
        simplyTyped(SchemaType.Integer)
      case JsonSchema.Singular.Object(None, None, None) if removeType =>
        JsonSchemaCodec.`true`
      case JsonSchema.Singular.Object(
            maybeProperties,
            maybeRequired,
            maybeAdditionalProperties
          ) =>
        JsonSchemaCodec.`object`(
          `$schema` = metaSchema,
          `$defs` = maybeDefs,
          `type` = if removeType then None else Some(Left(SchemaType.Object)),
          properties = maybeProperties.map(properties =>
            JsonObject(properties.view.mapValues(fromSchema).toMap)
          ),
          required = maybeRequired.map(JsonArray(_)),
          additionalProperties = maybeAdditionalProperties.map(fromSchema)
        )
      case JsonSchema.Singular.Boolean =>
        simplyTyped(SchemaType.Boolean)
      case JsonSchema.Singular.Number =>
        simplyTyped(SchemaType.Number)
      case JsonSchema.Singular.ListArray(
            JsonSchema(List(JsonSchema.Singular.True))
          ) =>
        simplyTyped(SchemaType.Array)
      case JsonSchema.Singular.ListArray(
            items
          ) =>
        JsonSchemaCodec.`object`(
          `$schema` = metaSchema,
          `$defs` = maybeDefs,
          `type` = Some(Left(SchemaType.Array)),
          items = Some(fromSchema(items))
        )
      case JsonSchema.Singular.TupleArray(prefixItems) =>
        JsonSchemaCodec.`object`(
          `$schema` = metaSchema,
          `$defs` = maybeDefs,
          `type` = Some(Left(SchemaType.Array)),
          prefixItems = Some(JsonArray(prefixItems.map(fromSchema))),
          items = Some(JsonSchemaCodec.`false`)
        )
      case JsonSchema.Singular.Const(value) =>
        JsonSchemaCodec.`object`(
          `$schema` = metaSchema,
          `$defs` = maybeDefs,
          const = Some(value)
        )
      case JsonSchema.Singular.Ref(JsonSchema.Reference.Root(values)) =>
        JsonSchemaCodec.`object`(
          `$schema` = metaSchema,
          `$defs` = maybeDefs,
          `$ref` = Some(values.mkString("#/", "/", ""))
        )
    }
  }

  def addToSetStrictly[A](s: Set[A], a: A) =
    if (s contains a) None else Some(s + a)

  val fromSchema: JsonSchema => JsonSchemaCodec = fromJsonSchemaWithDefs(
    None,
    None
  )

  def fromJsonSchemaWithDefs(
      metaSchema: Option[String],
      defs: Option[CodecMap]
  ): JsonSchema => JsonSchemaCodec = {
    type MaybeEncode = Kleisli[Option, JsonSchema, JsonSchemaCodec]
    val single: MaybeEncode = Kleisli {
      case JsonSchema(List(schema)) =>
        Some(fromSingular(metaSchema, removeType = false, defs, schema))
      case _ => None
    }
    val describedByTypeAlone: MaybeEncode = Kleisli {
      case JsonSchema(schemas) =>
        for {
          types <- schemas.traverse(JsonSchema.Singular.describedByTypeAlone)
          unique <- types.foldM(Set.empty)(addToSetStrictly)
        } yield JsonSchemaCodec.`object`(
          `$schema` = metaSchema,
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
            `$schema` = metaSchema,
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
          `$schema` = metaSchema,
          `type` = maybeSingularType.map(Left(_)),
          anyOf = Some(
            JsonArray(
              schemas.map(
                fromSingular(
                  metaSchema = None,
                  removeType = maybeSingularType.isDefined,
                  maybeDefs = None,
                  _
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
  val metaSchema: String = "https://json-schema.org/draft/2020-12/schema"

  def of[A: SchemaOf]: JsonSchemaCodec =
    val (Defs(defs), anyOf) = summon[SchemaOf[A]].apply.run
    JsonSchemaCodec.fromJsonSchemaWithDefs(
      Some(metaSchema),
      if defs.isEmpty then None else Some(createDefs(defs))
    )(anyOf)

  given encoder(using
      e: => Encoder[Unfixed[Fix[Unfixed]]]
  ): Encoder[JsonSchemaCodec] = e.contramap(_.unfix)
