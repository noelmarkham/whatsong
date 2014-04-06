package com.noelmarkham.whatsong

import scalaz._
import Scalaz._
import OptionT._
import Feeds._
import com.twitter.util.{Await, Future}
import Implicits.{futureInstance, equalSongInstance}
import scala.annotation.tailrec
import java.util.Date
import java.io.File

object Wiring {

  def getSong(hostAndPort: String, path: String, sampleTimeSeconds: Int, apiKey: String): Future[Option[(Option[Song], Option[Song])]] = {
    (for {
      streamUrl <- optionT[Future](getPlaylist(hostAndPort, path).map(getPlaylistFeed))
      stream <- streamData(streamUrl).liftM[OptionT]
      outputFile <- writeStreamData(stream, sampleTimeSeconds * 18500).liftM[OptionT]
      songOpts <- both(outputFile, apiKey).liftM[OptionT]
      _ = outputFile.delete()
    } yield songOpts).run
  }

  private[this] def both(streamData: File, apiKey: String): Future[(Option[Song], Option[Song])] = {
    val echoprintFO = fingerprint("/usr/local/bin/echoprint-codegen", "4.12", streamData, apiKey)
    val enmfpFO = fingerprint("/Users/noel.markham/Downloads/enmfp/ENMFP_codegen/codegen.Darwin", "3.15", streamData, apiKey)

    (echoprintFO |@| enmfpFO){(_, _)}
  }

  private[this] def fingerprint(executable: String, apiVersion: String, streamData: File, apiKey: String): Future[Option[Song]] = {
    (for {
      fingerprintJson <- optionT[Future](getFingerprintJson(executable, streamData))
      fingerprint <- optionT[Future](Future.value(getFingerprint(fingerprintJson)))
      song <- optionT[Future](requestSong(fingerprint, apiVersion, apiKey))
    } yield song).run
  }

  def runContinually(hostAndPort: String, path: String, sampleTimeSeconds: Int, apiKey: String): Unit = {

    @tailrec
    def matches(prevSong: Option[Song], prevMatches: (Option[Song], Option[Song])): Unit = {

      val possibleSongs = Await.result(getSong(hostAndPort, path, sampleTimeSeconds, apiKey))

      possibleSongs match {
        case None => matches(prevSong, prevMatches)
        case Some((s1, s2)) => {
          val (p1, p2) = prevMatches
          val goodMatches = List(p1, p2, s1, s2).foldMap[Map[Song, Int]]{o =>
            o.foldMap(s => Map(s -> 1))
          }.filter{case (_, v) => v > 1}.map{case (k, _) => k}.toList

          (goodMatches.length, goodMatches.headOption) match {
            case (1, o @ Some(s)) if prevSong === o => matches(prevSong, (s1, s2))
            case (1, o @ Some(s)) => {
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

