import sbt._

class MyPlugins(info: ProjectInfo) extends PluginDefinition(info){
  lazy val eclipsify = "de.element34" % "sbt-eclipsify" % "0.6.0"
  lazy val sbtIdeaRepo = "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
  lazy val sbtIdea = "com.github.mpeltonen" % "sbt-idea-plugin" % "0.2.0"
}
