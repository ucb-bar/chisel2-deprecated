import sbt._
import Keys._

object BuildSettings
{
  val buildOrganization = "edu.berkeley.cs"
  val buildVersion = "1.1"
  val buildScalaVersion = "2.8.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion
  )
}

object ChiselBuild extends Build
{
  import BuildSettings._

  lazy val chisel = Project("chisel", file("chisel"), settings = buildSettings)
  lazy val test = Project("test", file("test"), settings = buildSettings) dependsOn(chisel)
}
