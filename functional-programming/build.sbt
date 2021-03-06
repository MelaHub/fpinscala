import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.functionalprogramming",
      scalaVersion := "2.12.4"
    )),
    name := "FunctionalProgramming",
    libraryDependencies += scalaTest % Test
  )
