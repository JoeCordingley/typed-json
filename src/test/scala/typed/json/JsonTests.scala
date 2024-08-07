package typed.json

import utest.*
import io.circe.literal.*
import io.circe.Decoder
import io.circe.syntax.*

object JsonTests extends TestSuite {

  val tests = Tests {

    test("optional") {
      val json = json"""{"optionalField": "aString"}"""
      val decoded =
        Decoder[JsonObject[Option[("optionalField", String)] *: EmptyTuple]]
          .decodeJson(json)
      assert(
        decoded == Right(
          JsonObject(Some(("optionalField", "aString")) *: EmptyTuple)
        )
      )
    }
    test("email") {
      assert(
        Email("person@domain.com").asJson == io.circe.Json.fromString(
          "person@domain.com"
        )
      )
    }
    test("null") {
      assert(JsonNull.asJson == io.circe.Json.Null)
    }
    test("object with properties") {
      type JsonType = JsonObject[
        (
            ("required-key", String),
            Option[("optional-key-1", Int)],
            Option[("optional-key-2", String)],
            ("optional-key-3", Nullable[String])
        )
      ]
      val json: JsonType = JsonObject(
        (
          "required-key" -> "requiredValue",
          Some("optional-key-1" -> 42),
          None,
          "optional-key-3" -> Nullable.fromOption(None)
        )
      )
      assert(json.asJson == json"""
        {
          "required-key": "requiredValue",
          "optional-key-1": 42,
          "optional-key-3": null
        }
      """)
    }
    test("string or null") {
      type JsonType = Either[String, JsonNull]
      val stringValue: JsonType = Left("aString")
      val nullValue: JsonType = Right(JsonNull)
      assert(stringValue.asJson == io.circe.Json.fromString("aString"))
      assert(nullValue.asJson == json"""null""")
    }
    test("map object") {
      type JsonType = JsonObject[Map[String, io.circe.Json]]
      val jsonValue: JsonType =
        JsonObject(Map("key1" -> json"{}", "key2" -> json"null"))
      assert(jsonValue.asJson == json"""{
        "key1": {},
        "key2": null
      }""")

    }
    test("array") {
      type JsonType = JsonArray[List[Int]]
      val jsonValue: JsonType = JsonArray(List(1, 2, 3))
      assert(jsonValue.asJson == json"[1, 2, 3]")
    }
    test("const") {
      type JsonType = "onlyThisValue"
      val jsonValue: JsonType = "onlyThisValue"
      assert(jsonValue.asJson == io.circe.Json.fromString("onlyThisValue"))
    }
    test("ref") {
      type JsonType = Referenced["name", io.circe.Json]
      val jsonValue: JsonType = Referenced(io.circe.Json.Null)
      assert(jsonValue.asJson == io.circe.Json.Null)
    }
  }
}
