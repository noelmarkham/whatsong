package com.noelmarkham.whatsong

import scalaz._
import Scalaz._
import OptionT._
import Feeds._
import com.twitter.util.Future
import TwitterFuture.futureInstance

object Wiring {

  def go(hostAndPort: String, path: String, apiKey: String) = {
    (for {
      streamUrl <- optionT[Future](getPlaylist(hostAndPort, path).map(getPlaylistFeed))
      stream <- streamData(streamUrl).liftM[OptionT]
      outputFile <- writeStreamData(stream, 370000).liftM[OptionT]
      fingerprintJson <- optionT[Future](getFingerprintJson(outputFile))
      fingerprint <- optionT[Future](Future.value(getFingerprint(fingerprintJson)))
      song <- optionT[Future](requestSong(fingerprint, apiKey))
    } yield song.describe).getOrElse("Unknown Song")
  }

}
