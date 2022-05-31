name := "bottom-up-arithmetic"

version := "0.1"

scalaVersion := "2.13.8"

idePackagePrefix := Some("org.bottomup.arithmetic")

libraryDependencies += "org.json4s" %% "json4s-native" % "4.0.5"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.8.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"