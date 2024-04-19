ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

lazy val root = (project in file("."))
    .settings(
        name := "rv",
        libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
    )
