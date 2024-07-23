package typed.json

import io.circe.Encoder
import io.circe.syntax.*
import io.circe
import cats.syntax.all.*
import cats.data.Writer
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
    def apply: JsonSchema = JsonSchema(
      JsonSchema.AnyOf(
        SchemaType.values.toList.map(s => JsonSchema.Singular.Const(s.asJson))
      )
    )

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

case class JsonSchema(anyOf: JsonSchema.AnyOf, defs: Defs = Defs.empty) {
  def writer: WithDefs[JsonSchema.AnyOf] = Writer(defs, anyOf)
}

object JsonSchema:
  def of[A: SchemaOf]: JsonSchema = summon[SchemaOf[A]].apply

  def fromWriter: Writer[Defs, AnyOf] => JsonSchema = _.run match {
    case (defs, anyOf) => JsonSchema(anyOf, defs)
  }
  case class AnyOf(schemas: List[JsonSchema.Singular])

  object AnyOf:
    def apply(schemas: Singular*): AnyOf = AnyOf(List(schemas*))

  enum Singular:
    case String(
        format: Option[java.lang.String] = None,
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None
    )
    case Null
    case Integer
    case Object(
        properties: Option[Map[java.lang.String, AnyOf]] = None,
        required: Option[List[java.lang.String]] = None,
        additionalProperties: Option[AnyOf] = None
    )
    case Boolean
    case Number
    case Array(items: AnyOf)
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
      case Array(AnyOf(schemas)) => schemas == List(JsonSchema.Singular.True)
      case _                     => true
    }

    def describedByTypeAlone(singular: Singular): Option[SchemaType] =
      if isSimple(singular) then SchemaType.fromSingular(singular) else None

  val `true`: JsonSchema =
    singular(Singular.True)

  def singular(singular: Singular): JsonSchema =
    JsonSchema(JsonSchema.AnyOf(singular))

trait SchemaOf[A]:
  def apply: JsonSchema

object SchemaOf:
  given objMap[A: SchemaOf]: SchemaOf[JsonObject[Map[String, A]]] with
    def apply: JsonSchema = JsonSchema.of[A] match {
      case JsonSchema(anyOf, defs) =>
        JsonSchema(
          JsonSchema.AnyOf(
            JsonSchema.Singular.Object(
              additionalProperties = Some(anyOf)
            )
          ),
          defs
        )

    }

  given SchemaOf[String] with
    def apply: JsonSchema =
      JsonSchema.singular(JsonSchema.Singular.String())

  given SchemaOf[Int] with
    def apply: JsonSchema =
      JsonSchema.singular(JsonSchema.Singular.Integer)

  given SchemaOf[Boolean] with
    def apply: JsonSchema =
      JsonSchema.singular(JsonSchema.Singular.Boolean)

  given SchemaOf[JsonNull] with
    def apply: JsonSchema =
      JsonSchema.singular(JsonSchema.Singular.Null)

  given SchemaOf[circe.JsonObject] with
    def apply: JsonSchema = JsonSchema.singular(JsonSchema.Singular.Object())

  given SchemaOf[circe.Json] with
    def apply: JsonSchema = JsonSchema.`true`

  given SchemaOf[Double] with
    def apply: JsonSchema =
      JsonSchema.singular(JsonSchema.Singular.Number)

  given SchemaOf[Email] with
    def apply: JsonSchema =
      JsonSchema.singular(JsonSchema.Singular.String(format = Some("email")))

  given [A: SchemaOf]: SchemaOf[JsonArray[A]] with
    def apply: JsonSchema = JsonSchema.of[A] match {
      case JsonSchema(anyOf, defs) =>
        JsonSchema(JsonSchema.AnyOf(JsonSchema.Singular.Array(anyOf)), defs)
    }

  given objWithProperties[A: PropertiesOf: RequiredOf]: SchemaOf[JsonObject[A]]
  with
    def apply: JsonSchema = Properties.of[A] match {
      case Properties(properties, defs) =>
        JsonSchema(
          anyOf = JsonSchema.AnyOf(
            JsonSchema.Singular.Object(
              properties = Some(properties),
              required = Some(summon[RequiredOf[A]].apply)
            )
          ),
          defs = defs
        )
    }

  given [A: SchemaOf, B: SchemaOf]: SchemaOf[Either[A, B]] with
    def apply: JsonSchema =
      JsonSchema.fromWriter {
        (
          JsonSchema.of[A].writer,
          JsonSchema.of[B].writer
        ).mapN { case (JsonSchema.AnyOf(x), JsonSchema.AnyOf(y)) =>
          JsonSchema.AnyOf(x ++ y)
        }
      }

  given [A: ValueOf: Encoder]: SchemaOf[A] with
    def apply: JsonSchema =
      JsonSchema.singular(
        JsonSchema.Singular.Const(summon[ValueOf[A]].value.asJson)
      )

trait PropertiesOf[A]:
  def apply: Properties

case class Properties(value: Map[String, JsonSchema.AnyOf], defs: Defs) {
  def writer: WithDefs[Map[String, JsonSchema.AnyOf]] = Writer(defs, value)
}
object Properties:
  def of[A: PropertiesOf]: Properties = summon[PropertiesOf[A]].apply
  def fromWriter: WithDefs[Map[String, JsonSchema.AnyOf]] => Properties =
    _.run match {
      case (defs, props) => Properties(props, defs)
    }

case class Defs(value: Map[String, JsonSchema.AnyOf])

object Defs:
  def empty: Defs = Defs(Map.empty)
  given Monoid[Defs] with
    def combine(x: Defs, y: Defs): Defs = Defs(x.value ++ y.value)
    def empty: Defs = Defs(Map.empty)

type WithDefs[A] = Writer[Defs, A]

object WithDefs:
  def pure[A](a: A): WithDefs[A] = Writer.value(a)

object PropertiesOf:
  given opt[A: JsonFieldCodec, B: SchemaOf, C <: Tuple: PropertiesOf]
      : PropertiesOf[Option[(A, B)] *: C] with
    def apply: Properties = nonOpt[A, B, C].apply

  given PropertiesOf[EmptyTuple] with
    def apply: Properties = Properties(Map.empty, Defs(Map.empty))

  given nonOpt[A: JsonFieldCodec, B: SchemaOf, C <: Tuple: PropertiesOf]
      : PropertiesOf[(A, B) *: C] with
    def apply: Properties =
      Properties.fromWriter {
        (
          Properties.of[C].writer,
          JsonSchema.of[B].writer
        ).mapN { case (properties, anyOf) =>
          properties + (summon[JsonFieldCodec[A]].encode -> anyOf)
        }
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
    def apply: JsonSchema = JsonSchema.fromWriter {
      for {
        thisDef <- summon[SchemaOf[B]].apply.writer
        key = summon[ValueOf[A]].value
        _ <- Writer.tell(Defs(Map(key -> thisDef)))
      } yield JsonSchema.AnyOf(
        JsonSchema.Singular.Ref(
          JsonSchema.Reference.Root(List("$defs", key))
        )
      )

    }
