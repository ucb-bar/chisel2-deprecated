def versionToArray(v: String): Array[String] = v.split('.')

lazy val chiselBuildSettings = Seq (
    organization := "edu.berkeley.cs",
    // version := "2.2.36",
    version := "2.2.36",
    name := "Chisel",
    scalaVersion := "2.11.7",
    crossScalaVersions := Seq("2.10.6", "2.11.7"),
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
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases")
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
    // scalatest and scalacheck ordinarily are needed only for testing,
    //  but since ChiselSpec is in main for clients of chisel and their tests,
    //  these are now required for the main build.
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),

    // Execute tests in the current project serially.
    // Tests from other projects may still run concurrently.
    parallelExecution in Test := false,
    scalacOptions ++= Seq("-deprecation", "-feature", "-language:reflectiveCalls", "-language:implicitConversions", "-language:existentials"),
    javacOptions ++= Seq("-target", "1.7"),
    scalacOptions in (Compile, doc) <++= (baseDirectory in LocalProject("chisel"), version) map { (bd, v) =>
      Seq("-diagrams", "-diagrams-max-classes", "25", "-sourcepath", bd.getAbsolutePath, "-doc-source-url", "https://github.com/ucb-bar/chisel/tree/master/€{FILE_PATH}.scala")
    }
 )

lazy val chisel = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    // We should really be using name.value, but currently, the package is "Chisel" (uppercase first letter)
    buildInfoPackage := /* name.value */ "Chisel",
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoKeys := Seq[BuildInfoKey](buildInfoPackage, version, scalaVersion, sbtVersion),
    // Move the managed source directory where git won't complain about it,
    //  and where we can easily package its files as part of the source jar artifact.
    //  We'd like to use versionToArray(), slice(), and mkString() to convert an explicit
    //  Scala version (like 2.10.6), into the leftmost two components (2.10),
    //  but this seems to run afoul of assumptions sbt makes about the inclusion
    //  of Scala-version-specfic code (we get
    //    BuildInfo is already defined as case class BuildInfo
    //  so use the full version spec.
    //sourceManaged in Compile <<= (sourceDirectory in Compile, scalaVersion){ (s,v) => s / ("scala-" + versionToArray(v).slice(0,2).mkString(".") + "/src_managed") },
    sourceManaged in Compile <<= (sourceDirectory in Compile, scalaVersion){ (s,v) => s / ("scala-" + v + "/src_managed") },
    // Add the generated sources to the packagedSrc artifact since they are excluded by default.
    mappings in (Compile, packageSrc) += { ((sourceManaged in Compile).value / "sbt-buildinfo" / "BuildInfo.scala") -> "BuildInfo.scala" }
  ).
  settings(chiselBuildSettings: _*)
