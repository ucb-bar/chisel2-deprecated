organization := "edu.berkeley.cs"

version := "1.1-SNAPSHOT"

name := "chisel"

scalaVersion := "2.10.2"

//sourceDirectory := new File("@srcTop@")

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

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
)

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

org.scalastyle.sbt.ScalastylePlugin.Settings

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M1" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

seq(ScctPlugin.instrumentSettings : _*)

// Execute tests in the current project serially.
// Tests from other projects may still run concurrently.
parallelExecution in Test := false

parallelExecution in ScctTest := false

scalacOptions ++= Seq("-deprecation", "-feature", "-language:reflectiveCalls", "-language:implicitConversions", "-language:existentials")
