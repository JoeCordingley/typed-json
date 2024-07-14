package typed.json

import utest.*
import io.circe.literal.*
import io.circe.Decoder

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

  }
}
