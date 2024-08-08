import xerial.sbt.Sonatype._

ThisBuild / sonatypeCredentialHost := sonatypeCentralHost
ThisBuild / version := "0.1.0"

lazy val `root` = (project in file("."))
  .settings(
    organization := "com.joecordingley",
    scalaVersion := "3.4.2",
    name := "typed-json",
    libraryDependencies ++= List(
      "org.http4s" %% "http4s-core" % "1.0.0-M38",
      "com.lihaoyi" %% "utest" % "0.8.1" % Test,
      "org.typelevel" %% "cats-core" % "2.12.0",
      "io.circe" %% "circe-core" % "0.14.9",
      "io.circe" %% "circe-parser" % "0.14.9",
      "io.circe" %% "circe-literal" % "0.14.9",
      "org.scalacheck" %% "scalacheck" % "1.17.0" % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    publishTo := sonatypePublishToBundle.value,
    sonatypeProjectHosting := Some(
      GitHubHosting("JoeCordingley", "typed-json", "joewcordingley@gmail.com")
    ),
    sonatypeProfileName := "com.joecordingley",
    publishMavenStyle := true,
    licenses := Seq(
      "APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")
    )
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
