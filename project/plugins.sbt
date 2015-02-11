resolvers += Resolver.url("bintray-sbt-plugin-releases", url("http://dl.bintray.com/banno/oss"))(Resolver.ivyStylePatterns)
 
addSbtPlugin("com.banno" % "sbt-license-plugin" % "0.1.4")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.3.0")




