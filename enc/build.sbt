import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "Fle",
      scalaVersion := "2.12.1",
      version      := "1.3"
    )),
    name := "enc",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
      "com.typesafe.akka" %% "akka-actor" % "2.5.3",
      "org.scala-lang.modules" %% "scala-xml" % "1.0.6"/*
      "org.scala-lang.modules" %% "scala-xml" % "1.0.5"*/
    ) /*,
    libraryDependencies += "com.ximpleware" % "vtd-xml" % "2.12"*/
  )
