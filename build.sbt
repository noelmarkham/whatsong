scalaVersion := "2.10.3"

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "spray repo" at "http://repo.spray.io"
)

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.0.6",
    "commons-io" % "commons-io" % "2.4",
    "io.argonaut" %% "argonaut" % "6.0.3",
    "io.spray" % "spray-client" % "1.3.1",
    "com.typesafe.akka" %% "akka-actor" % "2.3.2"// % "provided"
)
