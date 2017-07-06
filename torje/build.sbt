import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "Fle",
      scalaVersion := "2.12.1",
      version      := "1.0"
    )),
    name := "Torje",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
      "com.typesafe.akka" %% "akka-actor" % "2.5.0"


    )
  )
