name := "HRF"

version := "0.8.29"

scalaVersion := "2.13.8"

scalacOptions := Seq(
    "-unchecked", 
    "-deprecation", 
    "-feature", 
    "-language:postfixOps", 
    "-Wconf:msg=then is a reserved:s,msg=procedure syntax:s,msg=a number and a:s,msg=implicitConversions visible:s,msg=multiarg infix syntax looks like a tuple:s,msg=wildcard is not allowed and will error under:s",
)

Compile / unmanagedSourceDirectories += baseDirectory.value / "root"

Compile / unmanagedSourceDirectories += baseDirectory.value / "cthw"

Compile / unmanagedSourceDirectories += baseDirectory.value / "dwam"

Compile / unmanagedSourceDirectories += baseDirectory.value / "coup"

Compile / unmanagedSourceDirectories += baseDirectory.value / "suok"

Compile / unmanagedSourceDirectories += baseDirectory.value / "sehi"

Compile / unmanagedSourceDirectories += baseDirectory.value / "yarg"

Compile / unmanagedSourceDirectories += baseDirectory.value / "bsg"

maxErrors := 5

libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.0"

libraryDependencies += "org.scala-lang.modules" %%% "scala-collection-contrib" % "0.2.2"

sourceGenerators in Compile += Def.task {
    val file = (sourceManaged in Compile).value / "info.scala"
    IO.write(file, """package hrf { object Info { val name = "%s"; val version = "%s"; val time = %d } }""".stripMargin.format(name.value, version.value, System.currentTimeMillis % (24 * 60 * 60 * 1000)))
    Seq(file)
}
