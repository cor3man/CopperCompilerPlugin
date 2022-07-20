ThisBuild / version := "0.1.0"

//ThisBuild / scalaVersion := "2.13.8"
ThisBuild / scalaVersion := "2.12.10"
//ThisBuild / scalaVersion := "2.11.6"

lazy val commonSettings = Seq.empty[Setting[_]]

lazy val root = (project in file("."))
  //.settings(name := "CopperCompilerPlugin")
  .settings(name := "CCPlugin")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value
  ))

