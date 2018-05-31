scalaVersion := "2.12.6"

name := "staged-mrsc-scala"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

testOptions in Test += Tests.Argument("-oD")

logBuffered in Test := false

parallelExecution in Test := false
