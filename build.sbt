name := "scala_playground"

version := "0.0.1"

scalaVersion := "2.10.3"

parallelExecution in Test := false

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.2"

libraryDependencies += "org.scalaz" %% "scalaz-iteratee" % "7.0.2"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.0.2"
