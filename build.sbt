name := "choice-lunch-place"

version := "0.1"

scalaVersion := "2.12.7"

enablePlugins(JavaAppPackaging)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test

libraryDependencies += "com.bot4s" %% "telegram-core" % "4.0.0-RC2"
libraryDependencies += "com.bot4s" %% "telegram-akka" % "4.0.0-RC2"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.2"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.6.2"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.18"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.18"
libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.1.5"

val circeVersion = "0.10.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
