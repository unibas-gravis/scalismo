
resolvers += Resolver.url("bintray-sbt-plugin-releases", url("http://dl.bintray.com/banno/oss"))(Resolver.ivyStylePatterns)
 
addSbtPlugin("com.banno" % "sbt-license-plugin" % "0.1.4")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.3.0")




addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.6.0")


