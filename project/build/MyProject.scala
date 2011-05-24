import sbt._
import de.element34.sbteclipsify._

class MyProject(info: ProjectInfo) extends DefaultProject(info)
                                    with IdeaProject
                                    with Eclipsify{

  lazy val hi = task { println("Hello World"); None }
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)


  lazy val justTest = testTask(testFrameworks, testClasspath, testCompileConditional.analysis, testOptions)

  lazy val justTestOnly = testQuickMethod(testCompileConditional.analysis, testOptions)(
    o => testTask(testFrameworks, testClasspath, testCompileConditional.analysis, o)
  )


  lazy val junit = "junit" % "junit" % "4.5" % "test->default"
  lazy val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "test"
  lazy val mockito = "org.mockito" % "mockito-all" % "1.8.5" %"test"
}
