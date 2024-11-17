enablePlugins(ScalaJSPlugin)

Compile / mainClass := Some("hrf.HRF")

unmanagedSources / excludeFilter := "reflect-jvm.scala" || "log-jvm.scala" || "host-jvm.scala" || "grey-jvm.scala" || "timeline-jvm.scala" || "host.scala" || "convert-images.scala"

scalaJSUseMainModuleInitializer := true

scalaJSLinkerConfig ~= { _.withOptimizer(false) }

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0-SNAPSHOT"
