name := "HRF Good Game"

version := "17.0"

scalaVersion := "2.13.15"

scalacOptions += "-Wconf:cat=deprecation:s"

scalacOptions += "-unchecked"

libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.5.0"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.8.0"

libraryDependencies += "ch.megard" %% "akka-http-cors" % "1.2.0"

libraryDependencies += "com.typesafe.slick" %% "slick" % "3.5.2"
libraryDependencies += "com.typesafe.slick" %% "slick-hikaricp" % "3.5.2"

libraryDependencies += "org.hsqldb" % "hsqldb" % "2.7.4"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.16"

run / fork := true

Global / cancelable := true

trapExit := false

bspEnabled := false

assembly / assemblyMergeStrategy := {
    case x if x.endsWith("module-info.class") => MergeStrategy.discard
    case x => (assembly / assemblyMergeStrategy).value(x)
}
