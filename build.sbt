name := "bootcamp-tasks"

version := "0.1"

scalaVersion := "2.13.4"

scalacOptions ++= Seq(
  "-Ymacro-annotations"
)

val catsVersion = "2.6.0"
val catsEffectVersion = "2.5.0"
val circeVersion = "0.13.0"
val scalajVersion = "2.4.2"
val scalatestVersion = "3.2.2"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalaj" %% "scalaj-http" % scalajVersion % Test,
  "org.scalatest" %% "scalatest" % scalatestVersion % Test,
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)
