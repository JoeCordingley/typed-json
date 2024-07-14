package typed.json

import org.http4s.Uri
import cats.syntax.option
import io.circe.Encoder
import io.circe.syntax.*
import io.circe
import cats.syntax.all.*
import cats.data.{Kleisli, Reader}

type SchemaType = Either["string", Either["object", Either[
  "integer",
  Either["boolean", Either["null", Either["array", "number"]]]
]]]

object SchemaType:

  val String: SchemaType = Left("string")
  val Object: SchemaType = Right(Left("object"))
  val Integer: SchemaType = Right(Right(Left("integer")))
  val Boolean: SchemaType = Right(Right(Right(Left("boolean"))))
  val Null: SchemaType = Right(Right(Right(Right(Left("null")))))
  val Array: SchemaType = Right(Right(Right(Right(Right(Left("array"))))))
  val Number: SchemaType = Right(Right(Right(Right(Right(Right("number"))))))

  def fromSingular: JsonSchema.Singular => Option[SchemaType] = {
    case _: JsonSchema.Singular.String => Some(String)
    case _: JsonSchema.Singular.Object => Some(Object)
    case JsonSchema.Singular.Integer   => Some(Integer)
    case JsonSchema.Singular.Boolean   => Some(Boolean)
    case JsonSchema.Singular.Null      => Some(Null)
    case _: JsonSchema.Singular.Array  => Some(Array)
    case JsonSchema.Singular.Number    => Some(Number)
    case _                             => None
  }

type JsonSchemaCodec = Fix[JsonSchemaCodec.Unfixed]
case class JsonSchema(schemas: List[JsonSchema.Singular])

object JsonSchema:
  def or(left: JsonSchema, right: JsonSchema): JsonSchema = JsonSchema(
    left.schemas ++ right.schemas
  )

  enum Singular:
    case String(
        format: Option[java.lang.String],
        minLength: Option[Int],
        maxLength: Option[Int]
    )
    case Null
    case Integer
    case Object(
        properties: Option[Map[java.lang.String, JsonSchema]],
        required: Option[List[java.lang.String]],
        additionalProperties: Option[JsonSchema]
    )
    case Boolean
    case Number
    case Array(items: JsonSchema)
    case True
    case Const(value: circe.Json)

  object Singular:
    def isSimple: Singular => Boolean = {
      case String(format, minLength, maxLength) =>
        format.isEmpty && minLength.isEmpty && maxLength.isEmpty
      case Object(properties, required, additionalProperties) =>
        properties.isEmpty && required.isEmpty && additionalProperties.isEmpty
      case Array(schema) => schema == JsonSchema(List(JsonSchema.Singular.True))
      case _             => true
    }
    def describedByTypeAlone(singular: Singular): Option[SchemaType] =
      if isSimple(singular) then SchemaType.fromSingular(singular) else None

  def string(
      format: Option[String] = None,
      minLength: Option[Int] = None,
      maxLength: Option[Int] = None
  ): JsonSchema =
    JsonSchema.fromSingular(
      JsonSchema.Singular.String(format, minLength, maxLength)
    )
  val integer: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Integer)
  val boolean: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Boolean)
  val `null`: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Null)
  def `object`(
      properties: Option[Map[String, JsonSchema]] = None,
      required: Option[List[String]] = None,
      additionalProperties: Option[JsonSchema] = None
  ): JsonSchema =
    JsonSchema.fromSingular(
      JsonSchema.Singular.Object(
        properties,
        required,
        additionalProperties
      )
    )

  def const(value: circe.Json): JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Const(value))
  val `true`: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.True)
  val `number`: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Number)
  def array(items: JsonSchema): JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Array(items))

  def fromSingular(schema: JsonSchema.Singular): JsonSchema = JsonSchema(
    List(schema)
  )

object JsonSchemaCodec:
  val `true`: JsonSchemaCodec = Fix(Left(true))
  def `object`(
      `type`: Option[Either[SchemaType, JsonArray[SchemaType]]] = None,
      properties: Option[JsonObject[Map[String, JsonSchemaCodec]]] = None,
      required: Option[JsonArray[String]] = None,
      items: Option[JsonSchemaCodec] = None,
      additionalProperties: Option[JsonSchemaCodec] = None,
      format: Option[String] = None,
      minLength: Option[Int] = None,
      maxLength: Option[Int] = None,
      anyOf: Option[JsonArray[JsonSchemaCodec]] = None,
      const: Option[circe.Json] = None,
      `enum`: Option[JsonArray[circe.Json]] = None
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
          `enum`.map("enum" -> _)
        )
      )
    )
  )

  def simplyTyped(s: SchemaType, removeType: Boolean): JsonSchemaCodec =
    if removeType then JsonSchemaCodec.`true`
    else
      JsonSchemaCodec.`object`(
        `type` = Some(Left(s))
      )

  def fromSingular(
      removeType: Boolean
  ): JsonSchema.Singular => JsonSchemaCodec = {
    case JsonSchema.Singular.String(None, None, None) =>
      simplyTyped(SchemaType.String, removeType)
    case JsonSchema.Singular.String(format, minLength, maxLength) =>
      JsonSchemaCodec.`object`(
        `type` = if removeType then None else Some(Left(SchemaType.String)),
        format = format,
        minLength = minLength,
        maxLength = maxLength
      )
    case JsonSchema.Singular.True => JsonSchemaCodec.`true`
    case JsonSchema.Singular.Null => simplyTyped(SchemaType.Null, removeType)
    case JsonSchema.Singular.Integer =>
      simplyTyped(SchemaType.Integer, removeType)
    case JsonSchema.Singular.Object(None, None, None) if removeType =>
      JsonSchemaCodec.`true`
    case JsonSchema.Singular.Object(
          maybeProperties,
          maybeRequired,
          maybeAdditionalProperties
        ) =>
      JsonSchemaCodec.`object`(
        `type` = if removeType then None else Some(Left(SchemaType.Object)),
        properties = maybeProperties.map(properties =>
          JsonObject(properties.view.mapValues(fromJsonSchema).toMap)
        ),
        required = maybeRequired.map(JsonArray(_)),
        additionalProperties = maybeAdditionalProperties.map(fromJsonSchema)
      )
    case JsonSchema.Singular.Boolean =>
      simplyTyped(SchemaType.Boolean, removeType)
    case JsonSchema.Singular.Number =>
      simplyTyped(SchemaType.Number, removeType)
    case JsonSchema.Singular.Array(
          JsonSchema(List(JsonSchema.Singular.True))
        ) =>
      simplyTyped(SchemaType.Array, removeType)
    case JsonSchema.Singular.Array(
          items
        ) =>
      JsonSchemaCodec.`object`(
        `type` = Some(Left(SchemaType.Array)),
        items = Some(fromJsonSchema(items))
      )
    case JsonSchema.Singular.Const(value) =>
      JsonSchemaCodec.`object`(
        const = Some(value)
      )
  }

  def addToSetStrictly[A](s: Set[A], a: A) =
    if (s contains a) None else Some(s + a)

  type MaybeEncode = Kleisli[Option, List[JsonSchema.Singular], JsonSchemaCodec]

  def fromJsonSchema(schema: JsonSchema): JsonSchemaCodec = {
    val single: MaybeEncode = Kleisli {
      case List(JsonSchema.Singular.True) => Some(JsonSchemaCodec.`true`)
      case List(schema) => Some(fromSingular(removeType = false)(schema))
      case _            => None
    }
    val describedByTypeAlone: MaybeEncode = Kleisli { schemas =>
      for {
        types <- schemas.traverse(JsonSchema.Singular.describedByTypeAlone)
        unique <- types.foldM(Set.empty)(addToSetStrictly)
      } yield JsonSchemaCodec.`object`(`type` =
        Some(Right(JsonArray(types.toList)))
      )
    }

    val enumerable: MaybeEncode = Kleisli {
      _.traverse {
        case JsonSchema.Singular.Const(value) => Some(value)
        case _                                => None
      }.map(values =>
        JsonSchemaCodec.`object`(`enum` = Some(JsonArray(values.distinct)))
      )
    }

    val orElse: Reader[List[JsonSchema.Singular], JsonSchemaCodec] = Kleisli {
      schemas =>
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
                fromSingular(removeType = maybeSingularType.isDefined)
              )
            )
          )
        )
    }
    ((single <+> describedByTypeAlone <+> enumerable).toReader, orElse)
      .mapN(_ getOrElse _)
      .run(schema.schemas)
  }

  def of[A: SchemaOf]: JsonSchemaCodec =
    JsonSchemaCodec.fromJsonSchema(summon[SchemaOf[A]].apply)

  given encoder(using
      e: => Encoder[Unfixed[Fix[Unfixed]]]
  ): Encoder[JsonSchemaCodec] = e.contramap(_.unfix)
  type Unfixed[A] = Either[
    Boolean,
    JsonObject[
      (
          Option[("type", Either[SchemaType, JsonArray[SchemaType]])],
          Option[("properties", JsonObject[Map[String, A]])],
          Option[("required", JsonArray[String])],
          Option[("items", A)],
          Option[("additionalProperties", A)],
          Option[("format", String)],
          Option[("minLength", Int)],
          Option[("maxLength", Int)],
          Option[("anyOf", JsonArray[A])],
          Option[("const", circe.Json)],
          Option[("enum", JsonArray[circe.Json])],
      )
    ]
  ]

  def singular[A]: List[A] => Either[A, List[A]] = {
    case List(x) => Left(x)
    case xs      => Right(xs)
  }

trait SchemaOf[A]:
  def apply: JsonSchema

object SchemaOf:
  given objMap[A: SchemaOf]: SchemaOf[JsonObject[Map[String, A]]] with
    def apply: JsonSchema = JsonSchema.`object`(
      additionalProperties = Some(summon[SchemaOf[A]].apply)
    )
  given SchemaOf[String] with
    def apply: JsonSchema = JsonSchema.string()
  given SchemaOf[Int] with
    def apply: JsonSchema = JsonSchema.integer
  given SchemaOf[Boolean] with
    def apply: JsonSchema = JsonSchema.boolean
  given SchemaOf[JsonNull] with
    def apply: JsonSchema = JsonSchema.`null`
  given SchemaOf[circe.JsonObject] with
    def apply: JsonSchema =
      JsonSchema.`object`()
  given SchemaOf[circe.Json] with
    def apply: JsonSchema = JsonSchema.`true`
  given SchemaOf[Double] with
    def apply: JsonSchema = JsonSchema.number
  given SchemaOf[Email] with
    def apply: JsonSchema = JsonSchema.string(format = Some("email"))
  given [A: SchemaOf]: SchemaOf[JsonArray[A]] with
    def apply: JsonSchema = JsonSchema.array(summon[SchemaOf[A]].apply)
  given objWithProperties[A: PropertiesOf: RequiredOf]: SchemaOf[JsonObject[A]]
  with
    def apply: JsonSchema = JsonSchema.`object`(
      properties = Some(summon[PropertiesOf[A]].apply),
      required = Some(summon[RequiredOf[A]].apply)
    )
  given [A: SchemaOf, B: SchemaOf]: SchemaOf[Either[A, B]] with
    def apply: JsonSchema =
      JsonSchema.or(summon[SchemaOf[A]].apply, summon[SchemaOf[B]].apply)

  given [A: ValueOf: Encoder]: SchemaOf[A] with
    def apply: JsonSchema = JsonSchema.const(summon[ValueOf[A]].value.asJson)

trait PropertiesOf[A]:
  def apply: Map[String, JsonSchema]

object PropertiesOf:
  given opt[A: JsonFieldCodec, B: SchemaOf, C <: Tuple: PropertiesOf]
      : PropertiesOf[Option[(A, B)] *: C] with
    def apply: Map[String, JsonSchema] = nonOpt[A, B, C].apply
  given PropertiesOf[EmptyTuple] with
    def apply: Map[String, JsonSchema] = Map.empty
  given nonOpt[A: JsonFieldCodec, B: SchemaOf, C <: Tuple: PropertiesOf]
      : PropertiesOf[(A, B) *: C] with
    def apply: Map[String, JsonSchema] = summon[
      PropertiesOf[C]
    ].apply + (summon[JsonFieldCodec[A]].encode -> summon[SchemaOf[B]].apply)

trait RequiredOf[A]:
  def apply: List[String]

object RequiredOf:
  given RequiredOf[EmptyTuple] with
    def apply: List[String] = List.empty
  given nonOpt[A: JsonFieldCodec, B, C <: Tuple: RequiredOf]
      : RequiredOf[(A, B) *: C] with
    def apply: List[String] =
      summon[JsonFieldCodec[A]].encode :: summon[RequiredOf[C]].apply
  given opt[A, C <: Tuple: RequiredOf]: RequiredOf[Option[A] *: C] with
    def apply: List[String] = summon[RequiredOf[C]].apply
