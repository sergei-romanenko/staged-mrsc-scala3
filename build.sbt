scalaVersion := "3.7.2"

name := "staged-mrsc-scala3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

// scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-source:3.0-migration", "-explain")
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-source:3.0-migration")

testOptions in Test += Tests.Argument("-oD")

logBuffered in Test := false

parallelExecution in Test := false
