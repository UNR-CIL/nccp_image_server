name := "NCCP Image REST Server"

version := "0.0.1"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  cache,
  "commons-net" % "commons-net" % "3.3"
)

play.Project.playScalaSettings
