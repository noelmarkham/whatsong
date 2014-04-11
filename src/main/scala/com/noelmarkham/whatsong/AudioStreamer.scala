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

trait AudioStreamerConfig {
  def hostAndPort: String
  def path: String
  def sampleTimeSeconds: Int = 20
  def apiKey: String
  def echoprintExecutable: String
  def enmfpExecutable: String
}

object AudioStreamer {

  def getSong(config: AudioStreamerConfig): Future[Option[(Option[Song], Option[Song])]] = {
    import config._
    (for {
      streamUrl <- optionT[Future](getPlaylist(hostAndPort, path).map(getPlaylistFeed))
      stream <- streamData(streamUrl).liftM[OptionT]
      outputFile <- writeStreamData(stream, sampleTimeSeconds * 18500).liftM[OptionT]
      songOpts <- both(outputFile, apiKey, echoprintExecutable, enmfpExecutable).liftM[OptionT]
      _ = outputFile.delete()
    } yield songOpts).run
  }

  private[this] def both(streamData: File, apiKey: String, echoprintExecutable: String, enmfpExecutable: String): Future[(Option[Song], Option[Song])] = {
    val echoprintFO = fingerprint(echoprintExecutable, "4.12", streamData, apiKey)
    val enmfpFO = fingerprint(enmfpExecutable, "3.15", streamData, apiKey)

    (echoprintFO |@| enmfpFO){(_, _)}
  }

  private[this] def fingerprint(executable: String, apiVersion: String, streamData: File, apiKey: String): Future[Option[Song]] = {
    (for {
      fingerprintJson <- optionT[Future](getFingerprintJson(executable, streamData))
      fingerprint <- optionT[Future](Future.value(getFingerprint(fingerprintJson)))
      song <- optionT[Future](requestSong(fingerprint, apiVersion, apiKey))
    } yield song).run
  }

  def runContinually(config: AudioStreamerConfig): Unit = {

    @tailrec
    def matches(prevSong: Option[Song], prevMatches: (Option[Song], Option[Song])): Unit = {

      val possibleSongs = Await.result(getSong(config))

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

