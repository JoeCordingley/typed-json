package typed.json

import io.circe.syntax.*
import io.circe.{Decoder, Json}
import io.circe.parser.parse
import typed.json.JsonSchemaCodec.given
import typed.json.SchemaType
import utest.*
import io.circe.literal.*
import io.circe
import cats.syntax.all.*
import io.circe.DecodingFailure
import io.circe.DecodingFailure.Reason
import scala.annotation.experimental

object JsonSchemaTests extends TestSuite {
  def addToSetStrictly[A](s: Set[A], a: A) =
    if (s contains a) None else Some(s + a)

  def toSet[A](l: List[A]): Option[Set[A]] =
    l.foldM(Set.empty)(addToSetStrictly)

  case class StrictSet[A](s: Set[A])
  object StrictSet:
    given [A: Decoder]: Decoder[StrictSet[A]] =
      Decoder[List[A]].emap(
        toSet(_).toRight(s"contains duplicates").map(StrictSet(_))
      )
  val maybeType = Decoder[Json].at("type").decodeJson
  val maybeAnyOf = Decoder[Json].at("anyOf").decodeJson
  def testFixed[A: SchemaOf](expectedSchema: Json) = {
    val schema = JsonSchemaCodec.of[A].asJson
    assert(schema == expectedSchema)
  }
  def testSimple[A: SchemaOf](typeName: String) = testFixed[A](parse(s"""{
          "type": "$typeName"
        }""").toOption.get)
  val tests = Tests {
    test("json") { testFixed[circe.Json](json"true") }
    test("string") { testSimple[String]("string") }
    test("email") {
      testFixed[Email](json"""{
        "type": "string",
        "format": "email"
      }""")
    }
    test("custom format string") {
      case class MyStringFormat(value: String)
      given SchemaOf[MyStringFormat] with
        def apply: JsonSchema =
          JsonSchema.string(minLength = Some(5), maxLength = Some(10))
      testFixed[MyStringFormat](json"""{
        "type": "string",
        "minLength": 5,
        "maxLength": 10
      }""")
    }
    test("null") { testSimple[JsonNull]("null") }
    test("integer") { testSimple[Int]("integer") }
    test("object") { testSimple[circe.JsonObject]("object") }
    test("boolean") { testSimple[Boolean]("boolean") }
    test("number") { testSimple[Double]("number") }
    test("object with properties") {
      testFixed[JsonObject[
        (("required-key", String), Option[("optional-key", Int)])
      ]](parse(s"""
        {
          "type": "object",
          "properties": {
            "required-key": {
              "type": "string"
            },
            "optional-key": {
              "type": "integer"
            }
          },
          "required": [
            "required-key"
          ]
        }
      """).toOption.get)
    }
    test("string or null") {
      val schema = JsonSchemaCodec
        .fromJsonSchema(summon[SchemaOf[Either[String, JsonNull]]].apply)
        .asJson
      val expectedSchema = (`type`: Json) => parse(s"""{
          "type": ${`type`}
        }""")
      assert(
        maybeType(schema).flatMap(_.as[StrictSet[String]]) == Right(
          StrictSet(Set("null", "string"))
        )
      )
      assert(Right(schema) == maybeType(schema).flatMap(expectedSchema))
    }

    test("object or null") {
      val schema = JsonSchemaCodec
        .of[
          Either[JsonObject.Solo[("key", String)], JsonNull]
        ]
        .asJson
      val expectedSchema = (anyOf: Json) => parse(s"""{
          "anyOf": $anyOf
        }""")
      val expectedFirstSchema = json"""{
        "type": "object",
        "properties": {
          "key": {
            "type": "string"
          }
        },
        "required": ["key"]
      }"""
      val expectedSecondSchema = json"""{
        "type": "null"
      }"""
      assert(
        maybeAnyOf(schema).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
      assert(Right(schema) == maybeAnyOf(schema).flatMap(expectedSchema))
    }
    test("object or object") {
      val schema = JsonSchemaCodec
        .of[
          Either[JsonObject.Solo[("first", String)], JsonObject.Solo[
            ("second", Int)
          ]]
        ]
        .asJson

      val expectedSchema = (anyOf: Json) => parse(s"""{
          "type": "object",
          "anyOf": $anyOf
        }""")
      val maybeAnyOf = Decoder[Json].at("anyOf").decodeJson
      val expectedFirstSchema = json"""{
        "properties": {
          "first": {
            "type": "string"
          }
        },
        "required": ["first"]
      }"""
      val expectedSecondSchema = json"""{
        "properties": {
          "second": {
            "type": "integer"
          }
        },
        "required": ["second"]
      }"""
      assert(
        maybeAnyOf(schema).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
      assert(Right(schema) == maybeAnyOf(schema).flatMap(expectedSchema))
    }
    test("object or object or null") {
      val schema = JsonSchemaCodec
        .of[Either[JsonObject.Solo[("first", String)], Either[JsonObject.Solo[
          ("second", Int)
        ], JsonNull]]]
        .asJson

      val expectedSchema = (anyOf: Json) => parse(s"""{
          "anyOf": $anyOf
        }""")

      val maybeAnyOf = Decoder[Json].at("anyOf").decodeJson
      val expectedFirstSchema = json"""{
        "type": "object",
        "properties": {
          "first": {
            "type": "string"
          }
        },
        "required": ["first"]
      }"""
      val expectedSecondSchema = json"""{
        "type": "object",
        "properties": {
          "second": {
            "type": "integer"
          }
        },
        "required": ["second"]
      }"""
      val expectedThirdSchema = json"""{
        "type": "null"
      }"""
      assert(
        maybeAnyOf(schema).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(
            Set(expectedFirstSchema, expectedSecondSchema, expectedThirdSchema)
          )
        )
      )
      assert(
        Right(schema) == maybeAnyOf(schema).flatMap(expectedSchema)
      )
    }
    test("object or formatted string") {
      val schema = JsonSchemaCodec
        .of[Either[JsonObject.Solo[("first", String)], Email]]
        .asJson
      val expectedSchema = (anyOf: Json) => parse(s"""{
        "anyOf": $anyOf
      }""")
      val expectedFirstSchema = json"""{
        "type": "object",
        "properties": {
          "first": {
            "type": "string"
          }
        },
        "required": ["first"]
      }"""
      val expectedSecondSchema = json"""{
        "type": "string",
        "format": "email"
      }"""
      assert(
        maybeAnyOf(schema).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
      assert(
        Right(schema) == maybeAnyOf(schema).flatMap(expectedSchema)
      )
    }
    test("object with nullable key") {
      val schema = JsonSchemaCodec
        .of[
          JsonObject.Solo[("key", Nullable[String])]
        ]
        .asJson
      val maybeKeyTypes =
        Decoder[Json].at("type").at("key").at("properties").decodeJson
      val expectedSchema = (`type`: Json) => parse(s"""{
          "type": "object",
          "properties": {
            "key": {
              "type": ${`type`}
            }
          },
          "required": ["key"]
        }""")
      assert(
        maybeKeyTypes(schema).flatMap(_.as[StrictSet[String]]) == Right(
          StrictSet(Set("null", "string"))
        )
      )
      assert(Right(schema) == maybeKeyTypes(schema).flatMap(expectedSchema))
    }
    test("object or object or object") {
      val schema =
        JsonSchemaCodec
          .of[Either[JsonObject.Solo[("first", String)], Either[
            JsonObject.Solo[("second", Int)],
            JsonObject.Solo[("third", Boolean)]
          ]]]
          .asJson

      val expectedSchema = (anyOf: Json) => parse(s"""{
          "type": "object",
          "anyOf": $anyOf
        }""")
      val expectedFirstSchema = json"""{
        "properties": {
          "first": {
            "type": "string"
          }
        },
        "required": ["first"]
      }"""
      val expectedSecondSchema = json"""{
        "properties": {
          "second": {
            "type": "integer"
          }
        },
        "required": ["second"]
      }"""
      val expectedThirdSchema = json"""{
        "properties": {
          "third": {
            "type": "boolean"
          }
        },
        "required": ["third"]
      }"""
      assert(
        maybeAnyOf(schema).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(
            Set(expectedFirstSchema, expectedSecondSchema, expectedThirdSchema)
          )
        )
      )
      assert(Right(schema) == maybeAnyOf(schema).flatMap(expectedSchema))
    }
    test("map object") {
      testFixed[JsonObject[Map[String, String]]](json"""{
        "type": "object",
        "additionalProperties": {
          "type": "string"
        }
      }""")
    }
    test("object or map object") {
      val schema = JsonSchemaCodec
        .of[
          Either[JsonObject.Solo[("key", String)], JsonObject[Map[String, Int]]]
        ]
        .asJson
      val expectedSchema = (anyOf: Json) => parse(s"""{
        "type": "object",
        "anyOf": $anyOf
      }""")
      val expectedFirstSchema = json"""{
        "properties": {
          "key": {
            "type": "string"
          }
        },
        "required": ["key"]
      }"""
      val expectedSecondSchema = json"""{
        "additionalProperties": {
          "type": "integer"
        }
      }"""
      assert(
        maybeAnyOf(schema).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
      assert(Right(schema) == maybeAnyOf(schema).flatMap(expectedSchema))
    }
    test("string or formatted string") {
      val schemaJson = JsonSchemaCodec.of[Either[String, Email]].asJson
      val expectedSchema = (anyOf: Json) => parse(s"""{
        "type": "string",
        "anyOf": $anyOf
      }""")
      val expectedFirstSchema = json"true"
      val expectedSecondSchema = json"""{
        "format": "email"
      }"""
      assert(
        maybeAnyOf(schemaJson).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
      assert(
        Right(schemaJson) == maybeAnyOf(schemaJson).flatMap(expectedSchema)
      )
    }
    test("email or minLength") {
      type MyStringFormat
      given SchemaOf[MyStringFormat] with
        def apply: JsonSchema =
          JsonSchema.string(
            minLength = Some(5)
          )
      val schemaJson = JsonSchemaCodec.of[Either[MyStringFormat, Email]].asJson
      val expectedSchema = (anyOf: Json) => parse(s"""{
        "type": "string",
        "anyOf": $anyOf
      }""")
      val expectedFirstSchema = json"""{
        "format": "email"
      }"""
      val expectedSecondSchema = json"""{
        "minLength": 5
      }"""
      assert(
        maybeAnyOf(schemaJson).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
      assert(
        Right(schemaJson) == maybeAnyOf(schemaJson).flatMap(expectedSchema)
      )
    }
    test("array of json") {
      testSimple[JsonArray[circe.Json]]("array")
    }
    test("array of string") {
      testFixed[JsonArray[String]](json"""{
        "type": "array",
        "items": {
          "type": "string"
        }
      }""")
    }
    test("const") {
      testFixed["value"](json"""{
        "const": "value"
      }""")
    }
    test("enum") {
      val schemaJson =
        JsonSchemaCodec.of[Either["first case", "second case"]].asJson
      val expectedSchema = (`enum`: Json) => parse(s"""{
        "enum": ${`enum`}
      }""")
      val maybeEnum = Decoder[Json].at("enum").decodeJson
      assert(
        maybeEnum(schemaJson).flatMap(_.as[StrictSet[String]]) == Right(
          StrictSet(
            Set(
              "first case",
              "second case"
            )
          )
        )
      )
      assert(
        Right(schemaJson) == maybeEnum(schemaJson).flatMap(expectedSchema)
      )
    }
//    test("schemaType") {
//      val schemaJson = JsonSchemaCodec.of[SchemaType].asJson
//      val expectedSchema = (`enum`: Json) => parse("""{
//        "enum": ${`enum`}
//      }""")
//      val maybeEnum = Decoder[Json].at("enum").decodeJson
//      assert(SchemaType.Object.asJson == json""""object"""")
//      assert(
//        maybeEnum(schemaJson).flatMap(_.as[StrictSet[String]]) == Right(
//          StrictSet(
//            Set(
//              "string",
//              "object",
//              "integer",
//              "boolean",
//              "null",
//              "array",
//              "number"
//            )
//          )
//        )
//      )
//
//    }
  }
}
