package typed.api

import utest.*
import io.circe.literal.*
import cats.Id
import io.circe.syntax.*

object OpenApiTests extends TestSuite {

  val info = Info(title = "aTitle", version = "1")
  val tests = Tests {
    test("get root") {
      type Api = Route[Id, Request[Method.Get, Path.Root], Status.Ok]
      val expectedSchema = json"""
        {
          "openapi": "3.1.0",
          "info": {
            "title": "aTitle",
            "version": "1"
          },
          "paths": {
            "/": {
              "get": {
                "responses": {
                  "200": {
                    "description": "OK"
                  }
                }
              }
            }
          }
        }
      """
      val actual = OpenApiSchemaCodec.of[Api](info).asJson
      assert(actual == expectedSchema)

      assert(true == true)
    }
  }

}
