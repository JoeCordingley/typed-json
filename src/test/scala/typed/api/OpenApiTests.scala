package typed.api

import utest.*
import io.circe.literal.*
import cats.Id
import io.circe.syntax.*
import cats.effect.IO
import typed.api.ApiTests.*

object OpenApiTests extends TestSuite {

  val info = Info(title = "aTitle", version = "1")
  type RootApi =
    EmptyTuple => (Method.Get, IO[Response[Status.Ok, "Ok", Json[String]]]) *:
      EmptyTuple
  type PathApi =
    "some" *: "path" *: EmptyTuple => (
        Method.Get,
        IO[Response[Status.Ok, "OK", Json[String]]]
    ) *: EmptyTuple
  type RootOrPathApi = (RootApi, PathApi)
  val tests = Tests {
    test("get root") {
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
                    "description": "Ok"
                  }
                }
              }
            }
          }
        }
      """
      val actual = OpenApiSchemaCodec.of[RootApi *: EmptyTuple](info).asJson
      assert(actual == expectedSchema)
    }
    test("get path") {
      val expectedSchema = json"""
        {
          "openapi": "3.1.0",
          "info": {
            "title": "aTitle",
            "version": "1"
          },
          "paths": {
            "/some/path": {
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
      val actual = OpenApiSchemaCodec.of[PathApi *: EmptyTuple](info).asJson
      assert(actual == expectedSchema)
    }
    test("two routes") {
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
                    "description": "Ok"
                  }
                }
              }
            },
            "/some/path": {
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
      val actual = OpenApiSchemaCodec.of[RootOrPathApi](info).asJson
      assert(actual == expectedSchema)
    }
    test("get or put") {

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
                    "description": "Ok"
                  }
                }
              },
              "put": {
                "responses": {
                  "200": {
                    "description": "Ok"
                  }
                }
              }
            }
          }
        }
      """
      val actual =
        OpenApiSchemaCodec.of[GetOrPutApi *: EmptyTuple](info).asJson
      assert(actual == expectedSchema)
    }
    test("path parameter") {

      val expectedSchema = json"""
        {
          "openapi": "3.1.0",
          "info": {
            "title": "aTitle",
            "version": "1"
          },
          "paths": {
            "/path/{param}": {
              "parameters": [
                {
                  "name": "param",
                  "in": "path",
                  "required": true,
                  "schema": "string"
                }
              ],
              "get": {
                "responses": {
                  "200": {
                    "description": "Ok"
                  }
                }
              }
            }
          }
        }
      """
      val actual =
        OpenApiSchemaCodec.of[PathParamApi](info).asJson
      assert(actual == expectedSchema)
    }
  }

}
