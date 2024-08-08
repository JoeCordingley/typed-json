package typed.json

import utest.*
import io.circe.literal.*
import io.circe.Decoder
import io.circe.syntax.*
import cats.Id

object JsonTests extends TestSuite {
  type Tree[A] = TreeF[Id, A]
  object Tree:
    def leaf[A](value: A): Tree[A] = TreeF.Leaf(value)
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      TreeF.Branch(left, right)
  enum TreeF[F[_], A]:
    case Leaf(value: A)
    case Branch(left: F[TreeF[F, A]], right: F[TreeF[F, A]])

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
      val email = Email("person@domain.com")
      val json = io.circe.Json.fromString("person@domain.com")
      test("encoding") {
        assert(
          email.asJson == json
        )
      }
      test("decoding") {
        assert(
          json.as[Email] == Right(email)
        )
      }
    }
    test("null") {
      test("encoding") {
        assert(JsonNull.asJson == io.circe.Json.Null)
      }
      test("decoding") {
        assert(io.circe.Json.Null.as[JsonNull] == Right(JsonNull))
      }
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
      val jsonObject: JsonType = JsonObject(
        (
          "required-key" -> "requiredValue",
          Some("optional-key-1" -> 42),
          None,
          "optional-key-3" -> Nullable.fromOption(None)
        )
      )
      val json = json"""
        {
          "required-key": "requiredValue",
          "optional-key-1": 42,
          "optional-key-3": null
        }
      """
      test("encoding") {
        assert(jsonObject.asJson == json)
      }
      test("decoding") {
        assert(json.as[JsonType] == Right(jsonObject))
      }
    }
    test("nested-object") {
      type JsonType = JsonObject.Solo[
        ("outer", JsonObject.Solo[("inner", String)])
      ]
      val jsonObject: JsonType = JsonObject.Solo(
        "outer" -> JsonObject.Solo(
          "inner" -> "value"
        )
      )
      val json = json"""
        {
          "outer": {
            "inner": "value"
          }
        }
      """
      test("encoding") {
        assert(jsonObject.asJson == json)
      }
      test("decoding") {
        assert(json.as[JsonType] == Right(jsonObject))
      }
    }
    test("string or null") {
      type JsonType = Either[String, JsonNull]
      val stringValue: JsonType = Left("aString")
      val stringJson: io.circe.Json = io.circe.Json.fromString("aString")
      val nullValue: JsonType = Right(JsonNull)
      val nullJson: io.circe.Json = json"""null"""
      test("encoding") {
        assert(stringValue.asJson == stringJson)
        assert(nullValue.asJson == nullJson)
      }
      test("decoding") {
        assert(stringJson.as[JsonType] == Right(stringValue))
        assert(nullJson.as[JsonType] == Right(nullValue))
      }
    }
    test("map object") {
      type JsonType = JsonObject[Map[String, io.circe.Json]]
      val objectValue: JsonType =
        JsonObject(Map("key1" -> json"{}", "key2" -> json"null"))
      val json: io.circe.Json = json"""{
        "key1": {},
        "key2": null
      }"""
      test("encoding") {
        assert(objectValue.asJson == json)
      }
      test("decoding") {
        assert(json.as[JsonType] == Right(objectValue))
      }
    }
    test("array") {
      type JsonType = JsonArray[List[Int]]
      val jsonValue: JsonType = JsonArray(List(1, 2, 3))
      val json: io.circe.Json = json"[1, 2, 3]"
      test("encoding") {
        assert(jsonValue.asJson == json)
      }
      test("decoding") {
        assert(json.as[JsonType] == Right(jsonValue))
      }
    }
    test("const") {
      type JsonType = "onlyThisValue"
      val jsonValue: JsonType = "onlyThisValue"
      val json: io.circe.Json = io.circe.Json.fromString("onlyThisValue")
      test("encoding") {
        assert(jsonValue.asJson == json)
      }
      test("decoding") {
        assert(json.as[JsonType] == Right(jsonValue))
      }
    }
    test("ref") {
      type JsonType = Referenced["name", io.circe.Json]
      val jsonValue: JsonType = Referenced(io.circe.Json.Null)
      val json: io.circe.Json = io.circe.Json.Null
      test("encoding") {
        assert(jsonValue.asJson == json)
      }
      test("decoding") {
        assert(json.as[JsonType] == Right(jsonValue))
      }
    }
    test("recursive-ref") {
      type Unfixed[A, B] = Either[A, JsonObject[(("left", B), ("right", B))]]
      type JsonType[A] = Fix[[B] =>> Unfixed[A, B]]
      def toJson[A]: Tree[A] => JsonType[A] = {
        case TreeF.Leaf(a) => Fix(Left(a))
        case TreeF.Branch(left, right) =>
          Fix(
            Right(
              JsonObject(("left" -> toJson(left), "right" -> toJson(right)))
            )
          )
      }
      val tree: Tree[Int] =
        Tree.branch(Tree.branch(Tree.leaf(1), Tree.leaf(2)), Tree.leaf(3))
      val jsonValue: JsonType[Int] = toJson(tree)
      val json = json"""
       {
        "left": {
          "left" : 1,
          "right": 2
        },
        "right": 3
       }
       """
      test("encoding") {
        assert(jsonValue.asJson == json)
      }
      test("decoding") {
        assert(json.as[JsonType[Int]] == Right(jsonValue))
      }
    }
    test("recursive-ref-with-opt") {
      type Unfixed[A, B] =
        Either[A, JsonObject[(Option[("left", B)], Option[("right", B)])]]
      type JsonType[A] = Fix[[B] =>> Unfixed[A, B]]
      def toJson[A]: TreeF[Option, A] => JsonType[A] = {
        case TreeF.Leaf(a) => Fix(Left(a))
        case TreeF.Branch(left, right) =>
          Fix(
            Right(
              JsonObject(
                (left.map("left" -> toJson(_)), right.map("right" -> toJson(_)))
              )
            )
          )
      }
      val tree: TreeF[Option, Int] = TreeF.Branch(
        Some(TreeF.Branch(Some(TreeF.Leaf(1)), None)),
        Some(TreeF.Leaf(3))
      )
      val jsonValue: JsonType[Int] = toJson(tree)
      val json = json"""
       {
        "left": {
          "left" : 1
        },
        "right": 3
       }
       """
      test("encoding") {
        assert(jsonValue.asJson == json)
      }
      test("decoding") {
        assert(json.as[JsonType[Int]] == Right(jsonValue))
      }
    }
    test("tuple-arrays") {
      type JsonType = JsonArray[(String, Int)]
      val jsonValue = JsonArray(("firstElement", 42))
      val json = json"""
      ["firstElement", 42]
    """
      test("encoding") {
        assert(jsonValue.asJson == json)
      }
      test("decoding") {
        assert(json.as[JsonType] == Right(jsonValue))
      }
    }

  }
}
