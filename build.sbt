organization := "pl.caltha"
name := "metrics-model"
version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.8"
val shapelessVersion = "2.3.2"
val circeVersion = "0.4.1"
val scalaTestVersion = "2.2.6"

libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % shapelessVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
