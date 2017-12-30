name := "cats-study"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.4"

scalacOptions += "-Ypartial-unification"

fork := true

libraryDependencies := Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.typelevel" %% "cats-core" % "1.0.0"
)
