import sbtassembly.Plugin._

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.0.6",
    "commons-io" % "commons-io" % "2.4",
    "io.argonaut" %% "argonaut" % "6.0.4",
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.1",
    "org.eclipse.jetty" % "jetty-server" % "9.2.1.v20140609"
)

initialCommands in console :=
  """
    |import scalaz._
    |import Scalaz._
    |
    |import scala.concurrent.Future
    |import scala.concurrent.Await
    |import scala.concurrent.duration._
  """.stripMargin

assemblySettings
