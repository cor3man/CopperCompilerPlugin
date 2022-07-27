ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "2.12.10"

lazy val commonSettings = Seq.empty[Setting[_]]

lazy val root = (project in file("."))
  .settings(name := "CCPlugin")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value
    //"net.liftweb" %% "lift-json" % "3.5.0"
  ), exportJars := true)

