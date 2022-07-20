enablePlugins(ScalaJSPlugin)

mainClass in Compile := Some("hrf.HRF")

excludeFilter in unmanagedSources := "reflect-jvm.scala" || "log-jvm.scala" || "host.scala"

scalaJSUseMainModuleInitializer := true

scalaJSLinkerConfig ~= { _.withOptimizer(false) }

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.2.0"

import SbtSound._

sound.play(compile in Compile, Sounds.Purr, Sounds.Basso) 

sound.play(fastLinkJS in Compile, Sounds.Hero, Sounds.Basso) 

sound.play(fullLinkJS in Compile, Sounds.Hero, Sounds.Sosumi)

