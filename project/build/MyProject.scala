import sbt._
import de.element34.sbteclipsify._

class BeanerProject(info: ProjectInfo) extends DefaultProject(info) with IdeaProject with Eclipsify{
  lazy val hi = task { println("Hello World"); None }
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)


  lazy val junit = "junit" % "junit" % "4.5" % "test->default"
  lazy val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  lazy val mockito = "org.mockito" % "mockito-all" % "1.8.5" %"test"
}
