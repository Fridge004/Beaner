import sbt._
import de.element34.sbteclipsify._

class BeanerProject(info: ProjectInfo) extends DefaultProject(info) with Eclipsify{
  lazy val hi = task { println("Hello World"); None }

  lazy val junit = "junit" % "junit" % "test"
  lazy val scalatest = "org.scalatest" % "scalatest" % "test"
  lazy val mockito = "org.mockito" % "mockito-all" % "test"
  
}
