import sbt._
import Keys._
import sbtunidoc.Plugin._
import sbtunidoc.Plugin.UnidocKeys._

object BuildSettings extends Build {

  val commonSettings = Defaults.defaultSettings ++ Seq (
    organization := "edu.berkeley.cs",
    // version := "2.2.26",
    version := "2.3-SNAPSHOT",
    scalaVersion := "2.10.4",
    crossScalaVersions := Seq("2.10.4", "2.11.5"),

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
//    parallelExecution in Test := false,
    // Since we share test-output directories across subprojects,
    // we need to ensure they run serially.
    // The library project dependsOn attempts to do this without success.
    // Or, we could have the tests use different test-output directories,
    // but this appears to be non-trivial as well.
    parallelExecution in Global := false,
    // unify scaladoc across multiple projects.
    autoAPIMappings := true,

    scalacOptions ++= Seq("-deprecation", "-feature", "-language:reflectiveCalls", "-language:implicitConversions", "-language:existentials")
  ) ++ org.scalastyle.sbt.ScalastylePlugin.Settings

  // Common settings for all subprojects (where the real work happens).
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

  lazy val customUnidocSettings = unidocSettings ++ Seq (
    doc in Compile := (doc in ScalaUnidoc).value,
    target in unidoc in ScalaUnidoc := crossTarget.value / "api"
  )

  lazy val core = (project in file("core")).
    settings(commonArtifactSettings: _*).
    settings(
      name := "chisel",
      testOptions in Test += Tests.Argument("-DtestOutputDir=" + target + "/test-outputs")
    )
  lazy val library = (project in file("library")).dependsOn(core % "compile->compile;test->test").
    settings(commonArtifactSettings: _*).
    settings(
      name := "chisel_library",
      testOptions in Test += Tests.Argument("-DtestOutputDir=" + target + "/test-outputs")
    )

  lazy val root = (project in file(".")).dependsOn(core, library)
    .settings(commonSettings: _*)
    .settings(customUnidocSettings: _*)
    .settings(
      name := "ChiselRoot",
      scalacOptions in (ScalaUnidoc, unidoc) += "-Ymacro-no-expand",
      // Don't publish anything in the root project.
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      com.typesafe.sbt.pgp.PgpKeys.publishSigned := {},
      com.typesafe.sbt.pgp.PgpKeys.publishLocalSigned := {},
      // Don't run tests in the root project.
//      test := {},
      aggregate in doc := false
    )
    .aggregate(core, library)

//    test in library := (test in library).dependsOn(test in core).value
}

