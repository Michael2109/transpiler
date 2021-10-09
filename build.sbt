name := "transpiler-sbt"

version := "0.1"

scalaVersion := "2.12.10"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

// https://mvnrepository.com/artifact/com.lihaoyi/fastparse
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.3"


// https://mvnrepository.com/artifact/org.slf4j/slf4j-log4j12
libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.32" % Test

// https://mvnrepository.com/artifact/org.webjars.bower/js-beautify
libraryDependencies += "org.webjars.bower" % "js-beautify" % "1.14.0"
