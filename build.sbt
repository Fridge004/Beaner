// set the name of the project
name := "Beaner"

version := "0.1.7"

organization := "org.tearne"

libraryDependencies ++= Seq(
	"junit" % "junit" % "4.8" % "test->default",
	"org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test->default",
	"org.mockito" % "mockito-all" % "1.8.5" %"test->default"
)

scalaVersion := "2.9.0-1"
