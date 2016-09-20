resolvers += Resolver.url("scalasbt", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases")) (Resolver.ivyStylePatterns)

resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.5.0")

/* Prevent
    SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder"
    SLF4J: Defaulting to no-operation (NOP) logger implementation
   due to the sbt-git plugin
 */
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.7"

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.5")
