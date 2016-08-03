
name := """freekvs"""

version := "0.1-SNAPSHOT"

lazy val root = project in file(".")

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature")

updateOptions := updateOptions.value.withCachedResolution(true)


sources in (Compile,doc) := Seq.empty
publishArtifact in (Compile, packageDoc) := false


libraryDependencies ++= Seq(
  "org.slf4j" % "jul-to-slf4j" % "1.7.12",
  "org.scalaz" %% "scalaz-core" % "7.1.5",
  "com.github.etaty" %% "rediscala" % "1.6.0"
)

licenses += ("Unlicense", url("http://unlicense.org/"))

credentials += Credentials(Path.userHome / ".ivy2" / ".atrepo")

resolvers ++= Seq[Resolver](
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
  "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/",
  Resolver.bintrayRepo("atrepo", "atrepo"),
  Resolver.url("atrepo", url("http://dl.bintray.com/atrepo/custom-plugins"))(Resolver.ivyStylePatterns)
)

