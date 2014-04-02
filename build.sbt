scalaVersion := "2.10.3"

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "Twitter Repo" at "http://maven.twttr.com/"
)

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.0.6",
    "com.twitter" %% "finagle-http" % "6.2.0",
    "commons-io" % "commons-io" % "2.4",
    "io.argonaut" %% "argonaut" % "6.0.3"
)