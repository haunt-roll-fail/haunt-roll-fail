enablePlugins(ScalaJSPlugin)

name := "Scala.js DOM Reduced"

version := "2.8.0-SNAPSHOT"

scalaVersion := "2.13.15"

scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-Wunused:imports,patvars,locals,implicits",
)

organization  := "org.scala-js"

moduleName := "scalajs-dom"

Compile / unmanagedSourceDirectories += (Compile / sourceDirectory).value / "scala-new-collections"

bspEnabled := false
