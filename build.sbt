import Dependencies._

lazy val commonSettings = Seq(
  organization := "net.logan",
  scalaVersion := "2.12.6"
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "znake",
    publishArtifact := false,
    version := "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalaz" %% "scalaz-zio" % "0.3.1",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
  )
