name := "hanabi"

version in ThisBuild := "1.0"

scalaVersion in ThisBuild := "2.11.8"


lazy val hanabi = project in file(".")
lazy val `hanabi-web` = project.enablePlugins(ScalaJSPlugin).dependsOn(hanabi)