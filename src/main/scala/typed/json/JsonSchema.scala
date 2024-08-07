package typed.json

import io.circe.Encoder
import io.circe.syntax.*
import io.circe
import cats.syntax.all.*
import cats.data.{Writer, StateT}
import cats.*

enum SchemaType:
  case String
  case Object
  case Integer
  case Boolean
  case Null
  case Array
  case Number

object SchemaType:
  given Encoder[SchemaType] =
    Encoder[String].contramap {
      case String  => "string"
      case Object  => "object"
      case Integer => "integer"
      case Boolean => "boolean"
      case Null    => "null"
      case Array   => "array"
      case Number  => "number"
    }

  given SchemaOf[SchemaType] with
    def apply: DefsWriter[JsonSchema] =
      JsonSchema(
        SchemaType.values.toList.map(s => JsonSchema.Singular.Const(s.asJson))
      ).pure[DefsWriter]

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

case class RecursiveRef[A, F[_]](fixed: Fix[F])

case class JsonSchema(anyOf: List[JsonSchema.Singular])

object JsonSchema:
  def apply(anyOf: Singular*): JsonSchema = JsonSchema(List(anyOf*))

  def string(
      format: Option[String] = None,
      minLength: Option[Int] = None,
      maxLength: Option[Int] = None
  ): JsonSchema = singular(
    Singular.String(format, minLength, maxLength)
  )
  val `null`: JsonSchema = singular(Singular.Null)
  val integer: JsonSchema = singular(Singular.Integer)
  def `object`(
      properties: Option[Map[String, JsonSchema]] = None,
      required: Option[List[String]] = None,
      additionalProperties: Option[JsonSchema] = None
  ): JsonSchema = singular(
    Singular.Object(properties, required, additionalProperties)
  )

  val boolean: JsonSchema = singular(Singular.Boolean)
  val number: JsonSchema = singular(Singular.Number)
  def array(items: JsonSchema): JsonSchema = singular(
    Singular.Array(items)
  )

  val `true`: JsonSchema =
    singular(Singular.True)
  def const(value: circe.Json): JsonSchema = singular(
    Singular.Const(value)
  )
  def `enum`(values: List[circe.Json]): JsonSchema =
    JsonSchema(values.map(Singular.Const(_)))

  def singular(singular: Singular): JsonSchema =
    JsonSchema(singular)

  enum Singular:
    case String(
        format: Option[java.lang.String] = None,
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None
    )
    case Null
    case Integer
    case Object(
        properties: Option[Map[java.lang.String, JsonSchema]] = None,
        required: Option[List[java.lang.String]] = None,
        additionalProperties: Option[JsonSchema] = None
    )
    case Boolean
    case Number
    case Array(items: JsonSchema)
    case True
    case Const(value: circe.Json)
    case Ref(identifier: Reference)

  enum Reference:
    case Root(path: List[String])

  object Singular:
    def isSimple: Singular => Boolean = {
      case String(format, minLength, maxLength) =>
        format.isEmpty && minLength.isEmpty && maxLength.isEmpty
      case Object(properties, required, additionalProperties) =>
        properties.isEmpty && required.isEmpty && additionalProperties.isEmpty
      case Array(JsonSchema(schemas)) =>
        schemas == List(JsonSchema.Singular.True)
      case _ => true
    }

    def describedByTypeAlone(singular: Singular): Option[SchemaType] =
      if isSimple(singular) then SchemaType.fromSingular(singular) else None

trait SchemaOf[A]:
  def apply: DefsWriter[JsonSchema]

object SchemaOf:
  type RootDefsReference[A]
  def instance[A](
      schema: JsonSchema,
      defs: Option[Map[String, JsonSchema]] = None
  ): SchemaOf[A] = new SchemaOf[A] {
    def apply: DefsWriter[JsonSchema] =
      Writer.apply(Defs(defs.getOrElse(Map.empty)), schema)
  }

  given objMap[A: SchemaOf]: SchemaOf[JsonObject[Map[String, A]]] with
    def apply: DefsWriter[JsonSchema] =
      summon[SchemaOf[A]].apply.map { anyOf =>
        JsonSchema(
          JsonSchema.Singular.Object(
            additionalProperties = Some(anyOf)
          )
        )
      }

  given SchemaOf[String] with
    def apply: DefsWriter[JsonSchema] =
      JsonSchema(JsonSchema.Singular.String())
        .pure[DefsWriter]

  given SchemaOf[Int] with
    def apply: DefsWriter[JsonSchema] =
      JsonSchema(JsonSchema.Singular.Integer)
        .pure[DefsWriter]

  given SchemaOf[Boolean] with
    def apply: DefsWriter[JsonSchema] =
      JsonSchema(JsonSchema.Singular.Boolean)
        .pure[DefsWriter]

  given SchemaOf[JsonNull] with
    def apply: DefsWriter[JsonSchema] =
      JsonSchema(JsonSchema.Singular.Null)
        .pure[DefsWriter]

  given SchemaOf[circe.JsonObject] with
    def apply: DefsWriter[JsonSchema] =
      JsonSchema(JsonSchema.Singular.Object())
        .pure[DefsWriter]

  given SchemaOf[circe.Json] with
    def apply: DefsWriter[JsonSchema] =
      JsonSchema(JsonSchema.Singular.True)
        .pure[DefsWriter]

  given SchemaOf[Double] with
    def apply: DefsWriter[JsonSchema] =
      JsonSchema(JsonSchema.Singular.Number)
        .pure[DefsWriter]

  given SchemaOf[Email] with
    def apply: DefsWriter[JsonSchema] =
      JsonSchema(JsonSchema.Singular.String(format = Some("email")))
        .pure[DefsWriter]

  given [A: SchemaOf]: SchemaOf[JsonArray[A]] with
    def apply: DefsWriter[JsonSchema] =
      summon[SchemaOf[A]].apply.map { anyOf =>
        JsonSchema(JsonSchema.Singular.Array(anyOf))
      }

  given [F[_], A <: String: ValueOf](using
      SchemaOf[F[RootDefsReference[A]]]
  ): SchemaOf[RecursiveRef[A, F]] with
    def apply: DefsWriter[JsonSchema] =
      summon[SchemaOf[Referenced[A, F[RootDefsReference[A]]]]].apply

  given [A <: String: ValueOf]: SchemaOf[RootDefsReference[A]] with
    def apply: DefsWriter[JsonSchema] = JsonSchema(
      JsonSchema.Singular.Ref(
        JsonSchema.Reference
          .Root(List("$defs", summon[ValueOf[A]].value))
      )
    ).pure[DefsWriter]

  given objWithProperties[A: PropertiesOf: RequiredOf]: SchemaOf[JsonObject[A]]
  with
    def apply: DefsWriter[JsonSchema] =
      summon[PropertiesOf[A]].apply.map { case Properties(properties) =>
        JsonSchema(
          JsonSchema.Singular.Object(
            properties = Some(properties),
            required = Some(summon[RequiredOf[A]].apply)
          )
        )
      }

  given [A: SchemaOf, B: SchemaOf]: SchemaOf[Either[A, B]] with
    def apply: DefsWriter[JsonSchema] =
      (
        summon[SchemaOf[A]].apply,
        summon[SchemaOf[B]].apply,
      ).mapN { case (JsonSchema(x), JsonSchema(y)) =>
        JsonSchema(x ++ y)
      }

  given [A: ValueOf: Encoder]: SchemaOf[A] with
    def apply: DefsWriter[JsonSchema] =
      JsonSchema(
        JsonSchema.Singular.Const(summon[ValueOf[A]].value.asJson)
      )
        .pure[DefsWriter]

trait PropertiesOf[A]:
  def apply: DefsWriter[Properties]

case class Properties(value: Map[String, JsonSchema])
case class Defs(value: Map[String, JsonSchema])

object Defs:
  def empty: Defs = Defs(Map.empty)
  given Monoid[Defs] with
    def combine(x: Defs, y: Defs): Defs = Defs(x.value ++ y.value)
    def empty: Defs = Defs(Map.empty)

type DefsWriter[A] = Writer[Defs, A]

object PropertiesOf:
  given opt[A: JsonFieldCodec, B: SchemaOf, C <: Tuple: PropertiesOf]
      : PropertiesOf[Option[(A, B)] *: C] with
    def apply: DefsWriter[Properties] = nonOpt[A, B, C].apply

  given PropertiesOf[EmptyTuple] with
    def apply: DefsWriter[Properties] = Properties(Map.empty).pure[DefsWriter]

  given nonOpt[A: JsonFieldCodec, B: SchemaOf, C <: Tuple: PropertiesOf]
      : PropertiesOf[(A, B) *: C] with
    def apply: DefsWriter[Properties] =
      (
        summon[PropertiesOf[C]].apply,
        summon[SchemaOf[B]].apply
      ).mapN { case (Properties(properties), anyOf) =>
        Properties(properties + (summon[JsonFieldCodec[A]].encode -> anyOf))
      }

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

case class Referenced[A, B](value: B)

object Referenced:
  given [A <: String: ValueOf, B: SchemaOf]: SchemaOf[Referenced[A, B]] with
    def apply: DefsWriter[JsonSchema] = for {
      thisDef <- summon[SchemaOf[B]].apply
      key = summon[ValueOf[A]].value
      thisValue <- Writer(
        Defs(
          Map(
            key -> thisDef
          )
        ),
        JsonSchema(
          JsonSchema.Singular.Ref(
            JsonSchema.Reference.Root(List("$defs", key))
          )
        )
      )
    } yield thisValue
