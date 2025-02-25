import Dependencies.*

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "ru.pangaia",
      scalaVersion := "3.4.3",
      version      := "0.1.0-SNAPSHOT",
      assembly / mainClass := Some("ru.pangaia.xsnake.GameFrame"),
      assembly / assemblyJarName := "xsnake.jar"
    )),
    name := "Hello",
    libraryDependencies += scalaTest % Test
  )
resolvers in Global ++= Seq(
  "Sbt plugins"                   at "https://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/",
  "Maven Central Server"          at "https://repo1.maven.org/maven2/",
  "TypeSafe Repository Releases"  at "https://repo.typesafe.com/typesafe/releases/",
  "TypeSafe Repository Snapshots" at "https://repo.typesafe.com/typesafe/snapshots/"
)
