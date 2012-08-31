import sbt._
import Keys._

object BuildSettings
{
  val buildOrganization = "edu.berkeley.cs"
  val buildVersion = "1.0"
  val buildScalaVersion = "2.9.2"

  def apply(projectdir: String) = {
    Defaults.defaultSettings ++ Seq (
        organization := buildOrganization,
        version      := buildVersion,
        scalaVersion := buildScalaVersion,
        scalaSource in Compile := Path.absolute(file(projectdir + "/src")),
        resourceDirectory in Compile := Path.absolute(file(projectdir + "/src/main/resources/")),
        publishMavenStyle := true,
        publishArtifact in Test := false,
        pomIncludeRepository := { x => false }, 
        pomExtra := (
          <url>http://chisel.eecs.berkeley.edu/</url>
          <licenses>
            <license>
              <name>BSD-style</name>
              <url>http://www.opensource.org/licenses/bsd-license.php</url>
              <distribution>repo</distribution>
            </license>
          </licenses>
          <scm>
            <url>https://github.com/ucb-bar/chisel.git</url>
            <connection>scm:git:github.com/ucb-bar/chisel.git</connection>
          </scm>
          <developers>
            <developer>
              <id>jackbackrack</id>
              <name>Jonathan Bachrach</name>
              <url>http://people.csail.mit.edu/jrb/</url>
            </developer>
            <developer>
              <id>huytbvo</id>
              <name>Huy Vo</name>
            </developer>
          </developers>
        ),
        publishTo <<= version { v: String =>
          val nexus = "https://oss.sonatype.org/"
          if (v.trim.endsWith("SNAPSHOT")) 
            Some("snapshots" at nexus + "content/repositories/snapshots")
          else                             
            Some("releases" at nexus + "service/local/staging/deploy/maven2")
        }
      )
  }
}

object ChiselBuild extends Build
{
  lazy val chisel = Project("chisel", file("chisel"), settings = BuildSettings(".."))
}
