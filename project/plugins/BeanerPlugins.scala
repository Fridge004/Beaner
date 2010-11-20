import sbt._

class BeanerPlugins(info: ProjectInfo) extends PluginDefinition(info){
  lazy val eclipsify = "de.element34" % "sbt-eclipsify" % "0.6.0"
}
