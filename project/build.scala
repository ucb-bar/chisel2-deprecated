import sbt._
import Keys._

object BuildSettings extends Build {

  val commonSettings = Defaults.defaultSettings ++ Seq (
    organization := "edu.berkeley.cs",
    // version := "2.2.26",
    version := "2.3-SNAPSHOT",
    scalaVersion := "2.10.4",
    crossScalaVersions := Seq("2.10.4", "2.11.5")
  )
  val commonArtifactSettings = commonSettings ++ Seq (
    //sourceDirectory := new File("@srcTop@"),
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
    },

    resolvers ++= Seq(
      "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
    ),

    /* Bumping "com.novocode" % "junit-interface" % "0.11", causes DelayTest testSeqReadBundle to fail
     *  in subtly disturbing ways on Linux (but not on Mac):
     *  - some fields in the generated .h file are re-named,
     *  - an additional field is added
     *  - the generated .cpp file has additional differences:
     *    - different temps in clock_lo
     *    - missing assignments
     *    - change of assignment order
     *    - use of "Tx" vs. "Tx.values"
     */
    libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),

    // Execute tests in the current project serially.
    // Tests from other projects may still run concurrently.
    parallelExecution in Test := false,
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:reflectiveCalls", "-language:implicitConversions", "-language:existentials")
  ) ++ org.scalastyle.sbt.ScalastylePlugin.Settings

  lazy val core = (project in file("core")).
    settings(commonArtifactSettings: _*).
    settings(
      name := "chisel"
    )
  lazy val library = (project in file("library")).dependsOn(core % "compile->compile;test->test").
    settings(commonArtifactSettings: _*).
    settings(
      name := "chisel_library"
    )

  lazy val root = (project in file(".")).
    aggregate(core, library).
    settings(commonSettings: _*).
    settings(
      publishArtifact := false,
      publish := {},
      publishLocal := {}
//      publishSigned := {}
//      publishLocalSigned := {}
    )

  Keys.`package` := {
    (Keys.`package` in (core, Compile)).value
    (Keys.`package` in (library, Compile)).value
  }

//  test in library := (test in library).dependsOn(test in core).value
    parallelExecution in test in ThisBuild := false

//  lazy val root = Project("chisel", file("."), settings=buildSettings)
}

