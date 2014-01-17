import sbt._
import Keys._

object NlpwebBuild extends Build {
  val nlptoolsGroupId = "org.allenai.nlptools"
  val nlptoolsVersion = "2.5.0-SNAPSHOT"

  lazy val root = Project(id = "nlpweb",
                          base = file("."),
                          settings = Project.defaultSettings)
}
