ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

fork := true

lazy val root = (project in file("."))
    .settings(
        name := "rv",
        libraryDependencies ++=
            Seq(
                "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
                // This pulls in the kernel and std modules automatically.
                "org.typelevel" %% "cats-effect" % "3.5.3",
                // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
                "org.typelevel" %% "cats-effect-kernel" % "3.5.3",
                // standard "effect" library (Queues, Console, Random etc.)
                "org.typelevel" %% "cats-effect-std" % "3.5.3"
            )
    )
