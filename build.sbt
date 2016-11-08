scalaVersion := "2.11.8"

name := "fun-scalaz"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.4",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.4",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test"
)
