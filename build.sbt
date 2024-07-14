lazy val `root` = (project in file("."))
  .settings(
    organization := "com.joecordingley",
    scalaVersion := "3.3.1",
    name := "typed-json",
    version := "0.1.0",
    libraryDependencies ++= List(
      "org.http4s" %% "http4s-core" % "1.0.0-M38",
      "com.lihaoyi" %% "utest" % "0.8.1" % Test,
      "org.typelevel" %% "cats-core" % "2.12.0",
      "io.circe" %% "circe-core" % "0.14.9",
      "io.circe" %% "circe-parser" % "0.14.9",
      "io.circe" %% "circe-literal" % "0.14.9",
      "org.scalacheck" %% "scalacheck" % "1.17.0" % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-source:future",
  "-Xfatal-warnings"
)
