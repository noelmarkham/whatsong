package com.noelmarkham.whatsong

import scalaz._
import Scalaz._
import argonaut._
import Argonaut._
import EitherT._
import Feeds._
import Implicits._
import scala.annotation.tailrec
import java.util.Date
import java.io.File
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

  def runContinually(config: AudioStreamerConfig): Unit = {

    @tailrec
    def matches(prevSong: Option[Song], prevMatches: (Option[Song], Option[Song]), errorCount: Int = 10): Unit = {

      val possibleSongs = try {
        Await.result(getSong(config), Duration(10, TimeUnit.MINUTES))
      }
      catch {
        case _: Exception => (None, None).right
      }

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
            case (1, o @ Some(s)) if prevSong =/= o => {
              println(s"${new Date()}: ${s.describe}")
              matches(o, (s1, s2))
            }
            case _ => matches(prevSong, (s1, s2))
          }
        }
      }
    }

    matches(None, (None, None))
  }
}

