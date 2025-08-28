scalaVersion := "3.7.2"

name := "staged-mrsc-scala3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

Test / testOptions += Tests.Argument("-oD")

Test / logBuffered := false

Test / parallelExecution := false
