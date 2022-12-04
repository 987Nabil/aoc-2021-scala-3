name := "aoc-2021"

version := "0.1"

scalaVersion := "3.2.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

lazy val aoc2022 = Project("aoc-2022", file("aoc-2022")).settings(scalaVersion := "3.2.1")
