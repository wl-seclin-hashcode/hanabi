name := "hanabi"

version in ThisBuild := "1.0"

scalaVersion in ThisBuild := "2.11.8"

lazy val hanabi = (project in file("."))
  .enablePlugins(ScalaJSPlugin)

lazy val `hanabi-web` = project
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(hanabi)
  .settings(
    libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.1"
  )