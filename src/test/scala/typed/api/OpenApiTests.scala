package typed.api

import utest.*
import io.circe.literal.*
import cats.Id
import io.circe.syntax.*
import cats.effect.IO

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
//  type PathApi =
//    Route[
//      Id,
//      Request[Method.Get, "some" *: "path" *: EmptyTuple],
//      Response[Status.Ok, "OK", Empty]
//    ]
//
//  type GetOrPut = (RootApi, PathApi)
//    Route[
//      Id,
//      EmptyTuple,
//      (
//          (Method.Get, Response[Status.Ok, "Ok", Empty]),
//          (Method.Put, Response[Status.Ok, "Ok", Empty])
//      )
//    ]
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

      type RootOrPutApi =
        EmptyTuple => (
            (Method.Get, IO[Response[Status.Ok, "Ok", Json[String]]]),
            (Method.Put, IO[Response[Status.Ok, "Ok", Json[String]]])
        )

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
              },
            }
          }
        }
      """
      val actual = OpenApiSchemaCodec.of[RootOrPutApi](info).asJson
      assert(actual == expectedSchema)
    }
  }

}
