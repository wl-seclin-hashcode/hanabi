name := "hanabi"

version in ThisBuild := "1.0"

scalaVersion in ThisBuild := "2.12.1"

lazy val hanabi = (project in file("."))
  .enablePlugins(ScalaJSPlugin)

lazy val `hanabi-web` = project
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(hanabi)
  .settings(
    libraryDependencies ++= Seq(
      "be.doeraene" %%% "scalajs-jquery" % "0.9.1"
    )
  )