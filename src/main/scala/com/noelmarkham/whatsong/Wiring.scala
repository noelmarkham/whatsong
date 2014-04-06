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
    def run(prevSong: Option[Song]): Unit = {
      val songs = Await.result(getSong(hostAndPort, path, sampleTimeSeconds, apiKey))

      (prevSong, songs) match {
        case (p, None) => run(p)
        case (p, Some((None, None))) => run(p)
        case (p, Some((o1, o2))) if o1 =/= o2 => run(p)
        case (p, Some((o1 @ Some(s1), o2 @ Some(s2)))) if s1 === s2 && (p === o1) => run(p)
        case (p, Some((o1 @ Some(s1), o2 @ Some(s2)))) if s1 === s2 && (p =/= o1) => {
          println(s"${new Date()}: ${s2.describe}")
          run(o2)
        }
      }
    }
    run(None)
  }
}

