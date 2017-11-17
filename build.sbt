name := "play-formless"
organization := "com.iterable"
version := "0.1-SNAPSHOT"
scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play" % "2.4.8",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "eu.timepit" %% "refined" % "0.8.4"
)

scalacOptions ++= Seq(
  "-feature",
  "-language:reflectiveCalls"
)
