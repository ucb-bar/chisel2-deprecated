import sbt._
import Keys._

object BuildSettings
{
  val buildOrganization = "edu.berkeley.cs"
  val buildVersion = "1.1"
  val buildScalaVersion = "2.9.2"

  def apply(projectdir: String) = {
    Defaults.defaultSettings ++ Seq (
        organization := buildOrganization,
        version      := buildVersion,
        scalaVersion := buildScalaVersion,
        scalaSource in Compile := Path.absolute(file(projectdir + "/src"))
      )
  }
}

object ChiselBuild extends Build
{
  lazy val chisel = Project("chisel", file("chisel"), settings = BuildSettings(".."))
}
