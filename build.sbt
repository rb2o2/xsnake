import Dependencies.*

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "ru.pangaia",
      scalaVersion := "3.3.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    libraryDependencies += scalaTest % Test
  )
