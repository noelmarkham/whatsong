package com.noelmarkham.whatsong

import scalaz._
import Scalaz._
import OptionT._
import Feeds._
import com.twitter.util.{Await, Future}
import TwitterFuture.futureInstance
import scala.annotation.tailrec
import java.util.Date

object Wiring {

  def getSong(hostAndPort: String, path: String, sampleTimeSeconds: Int, apiKey: String): Future[Option[String]] = {
    (for {
      streamUrl <- optionT[Future](getPlaylist(hostAndPort, path).map(getPlaylistFeed))
      stream <- streamData(streamUrl).liftM[OptionT]
      outputFile <- writeStreamData(stream, sampleTimeSeconds * 18500).liftM[OptionT]
      fingerprintJson <- optionT[Future](getFingerprintJson(outputFile))
      fingerprint <- optionT[Future](Future.value(getFingerprint(fingerprintJson)))
      song <- optionT[Future](requestSong(fingerprint, apiKey))
    } yield song.describe).run
  }

  def go(hostAndPort: String, path: String, sampleTimeSeconds: Int, apiKey: String): Future[String] = {
    optionT[Future](getSong(hostAndPort, path, sampleTimeSeconds, apiKey)).getOrElse("Unknown Song")
  }

  def runContinually(hostAndPort: String, path: String, sampleTimeSeconds: Int, apiKey: String): Unit = {
    @tailrec
    def run(prevSong: Option[String]): Unit = {
      val song = Await.result(getSong(hostAndPort, path, sampleTimeSeconds, apiKey))

      (prevSong, song) match {
        case (_, None) => ()
        case (p, o @ Some(s)) if p =/= o => println(s"${new Date()}: $s")
        case _ => ()
      }
      run(song)
    }
    run(None)
  }
}

