name := "scala_playground"

version := "0.0.1"

scalaVersion := "2.10.1"

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.2"

libraryDependencies += "org.scalaz" %% "scalaz-iteratee" % "7.0.2"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.0.2"
