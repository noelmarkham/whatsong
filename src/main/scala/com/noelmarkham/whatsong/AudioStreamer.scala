package com.noelmarkham.whatsong

import java.util.concurrent.atomic.{AtomicReference, AtomicInteger}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}

import org.eclipse.jetty.server.{Request, Server}
import org.eclipse.jetty.server.handler.AbstractHandler

import scalaz._
import Scalaz._
import argonaut._
import Argonaut._
import EitherT._
import Feeds._
import Implicits._
import scala.annotation.tailrec
import java.util.{Properties, Date}
import java.io.{FileInputStream, File}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit

trait AudioStreamerConfig {
  def url: String
  def sampleTimeSeconds: Int = 20
  def apiKey: String
  def echoprintExecutable: String
  def enmfpExecutable: String
}

object AudioStreamer {

  type EitherTString[M[+_], A] = EitherT[M, String, A]

  def getSong(config: AudioStreamerConfig): Future[String \/ (Option[Song], Option[Song])] = {
    import config._
    (for {
      playlists <- eitherT[Future, String, String](getPlaylist(url))
      streamUrl <- eitherT[Future, String, String](Future.successful(getPlaylistFeed(playlists)))
      stream <- streamData(streamUrl).liftM[EitherTString]
      outputFile <- writeStreamData(stream, sampleTimeSeconds * 18500).liftM[EitherTString]
      songOpts <- both(outputFile, apiKey, echoprintExecutable, enmfpExecutable)
      _ = outputFile.delete()
    } yield songOpts).run
  }

  private[this] def both(streamData: File, apiKey: String, echoprintExecutable: String, enmfpExecutable: String): EitherT[Future, String, (Option[Song], Option[Song])] = {
    val echoprintSong = fingerprint(echoprintExecutable, "4.12", streamData, apiKey)
    val enmfpSong = fingerprint(enmfpExecutable, "3.15", streamData, apiKey)

    (echoprintSong |@| enmfpSong){(_, _)}
  }

  private[this] def fingerprint(executable: String, apiVersion: String, streamData: File, apiKey: String): EitherT[Future, String, Option[Song]] = {
    for {
      fingerprintJson <- eitherT[Future, String , Json](getFingerprintJson(executable, streamData))
      fingerprint <- eitherT[Future, String, String](Future.successful(getFingerprint(fingerprintJson)))
      song <- eitherT[Future, String, Option[Song]](requestSong(fingerprint, apiVersion, apiKey))
    } yield song
  }

  def runContinually(config: AudioStreamerConfig, output: String => Unit): Unit = {

    @tailrec
    def matches(prevSong: Option[Song], prevMatches: (Option[Song], Option[Song]), errorCount: Int = 10): Unit = {

      val possibleSongs = try {
        println(s"${new Date()}: Polling")
        Await.result(getSong(config), Duration(10, TimeUnit.MINUTES))
      }
      catch {
        case e: Exception => {
          println(s"${new Date()}: Received exception: $e")
          (None, None).right
        }
      }

      println(s"${new Date()}: Got $possibleSongs")

      possibleSongs match {
        case -\/(errorString) => {
          println(s"  Error: $errorString")
          if(errorCount < 1) {
            println(s"Too many errors, exiting")
            sys.exit(-1)
          } else {
            matches(prevSong, prevMatches, errorCount -1)
          }
        }
        case \/-((s1, s2)) => {
          val (p1, p2) = prevMatches

          // If there is distinctly two or more matches from last 4 fingerprints, assume a match
          val goodMatches = List(p1, p2, s1, s2).foldMap[Map[Song, Int]]{o =>
            o.foldMap(s => Map(s -> 1))
          }.filter{case (_, v) => v > 1}.map{case (k, _) => k}.toList

          (goodMatches.length, goodMatches.headOption) match {
            case (1, o @ Some(s)) if prevSong =/= o =>
              println(s"${new Date()}: Found match: $s")
              output(s"${new Date()}: ${s.describe}")
              matches(o, (s1, s2))
            case _ => matches(prevSong, (s1, s2))
          }
        }
      }
    }

    matches(None, (None, None))
  }

  def runConsole(config: AudioStreamerConfig): Unit = {
    runContinually(config, println)
  }

  def runServer(config: AudioStreamerConfig): Unit = {
    val server = new Server(8080)

    val currentSong = new AtomicReference[Option[String]](None)

    def updateCurrentSong(songDescription: String) = {
      currentSong.set(songDescription.some)
    }

    server.setHandler(new AbstractHandler {
      override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
        response.setStatus(200)
        val output = currentSong.get().getOrElse("No matches")

        baseRequest.setHandled(true)
        response.getWriter.println(output)
      }
    })

    server.start()

    import scala.concurrent.ExecutionContext.Implicits.global
    Future(runContinually(config, updateCurrentSong))
    server.join()
  }
}

object Main extends App {

  val properties = new Properties()
  properties.load(new FileInputStream("config.properties"))

  val configOption =
   (Option(properties.getProperty("whatsong.url")) |@|
    Option(properties.getProperty("whatsong.apiKey")) |@|
    Option(properties.getProperty("whatsong.echoprintExecutable")) |@|
    Option(properties.getProperty("whatsong.enmfpExecutable"))) { (curl, capiKey, cechoprintExecutable, cenmfpExecutable) =>

    new AudioStreamerConfig {
      val url: String = curl
      val apiKey: String = capiKey
      val echoprintExecutable: String = cechoprintExecutable
      val enmfpExecutable: String = cenmfpExecutable
    }
  }

  configOption.cata({ (config: AudioStreamerConfig) =>
    AudioStreamer.runServer(config)
  }, {
    System.err.println("Provide an appropriate config.properties file")
//    System.exit(-1)
  })
}
