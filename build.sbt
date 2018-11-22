import Dependencies._

lazy val commonSettings = Seq(
  organization := "net.logan",
  scalaVersion := "2.12.6"
)


resolvers ++= Seq( "maven.org" at "http://repo2.maven.org/maven2",
  "conjars.org" at "http://conjars.org/repo")


lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "znake",
    publishArtifact := false,
    version := "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalaz" %% "scalaz-zio" % "0.3.1",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
    libraryDependencies += "jline" % "jline" % "2.14.6"
  )
