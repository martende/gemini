organization := "com.github.martende"

name := "gemini"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.4"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.6",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.6" % "test",
  "com.typesafe.play" %% "play-json" % "2.3.0",
  "org.scalatest"     %% "scalatest" % "2.2.1"   % "test"
)


