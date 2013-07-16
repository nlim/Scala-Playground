name := "scala_playground"

version := "0.0.1"

scalaVersion := "2.10.1"

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)
