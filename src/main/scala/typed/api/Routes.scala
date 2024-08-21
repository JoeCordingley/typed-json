package typed.api

import org.http4s.HttpApp

type Route[F[_], Method, Path, Status] = (Method, Path) => F[Status]

object Method:
  object Get
  type Get = Get.type

object Status:
  case object Ok
  type Ok = Ok.type

object Path:
  case object Root
  type Root = Root.type

object Routes:
  def fromApi[F[_], Method, Path, Status](
      route: Route[F, Method, Path, Status]
  ): HttpApp[F] = ???
