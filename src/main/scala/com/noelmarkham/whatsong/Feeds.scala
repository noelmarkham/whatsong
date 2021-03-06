package com.noelmarkham.whatsong

import scalaz._
import Scalaz._
import java.io._
import org.apache.commons.io.IOUtils
import argonaut._
import Argonaut._
import java.net.URL
import scala.concurrent.Future
import dispatch._
import Defaults._
import Implicits._

case class Song(id: String, title: String, artist: String) {
  def describe = s"$title by $artist"
}

object Feeds {

  def getPlaylist(endpoint: String): Future[String \/ String] = {
    Http(url(endpoint) OK as.String).fold({ throwable =>
      s"Unable to retrieve playlist: $throwable".left
    }, { data =>
      data.right
    })
  }

  def getPlaylistFeed(playlistData: String): String \/ String = {
    playlistData
      .lines
      .filter(_.startsWith("File1="))
      .map(s => s.drop("File1=".length))
      .find(_ => true)
      .toRightDisjunction(s"Unable to find stream URL from playlist response: [$playlistData]")
  }

  def streamData(streamUrl: String): Future[InputStream] = {
    Future {
      val url = new URL(streamUrl)
      val conn = url.openConnection()
      conn.getInputStream
    }
  }

  def writeStreamData(stream: InputStream, size: Int): Future[File] = {
    Future {
      val file = java.io.File.createTempFile("stream", "")
      file.deleteOnExit
      val fos = new FileOutputStream(file)

      IOUtils.copyLarge(stream, fos, 0, size)
      fos.close
      stream.close
      file
    }
  }

  def getFingerprintJson(fingerprintApplication: String, streamData: File): Future[String \/ Json] = {
    Future {
      import scala.sys.process._
      val filename = streamData.getAbsolutePath
      val process = Process(s"$fingerprintApplication $filename")

      val output = process.!!
      Parse.parse(output)
    }
  }

  def getFingerprint(json: Json): String \/ String = {
    Lenses.codeLens
      .get(json)
      .toRightDisjunction(s"Cannot extract code from fingerprint application output: $json")
  }

  def requestSong(code: String, apiVersion: String, apiKey: String): Future[String \/ Option[Song]] = {

    val requestParameters = Map(
      "api_key" -> apiKey,
      "version" -> apiVersion,
      "code"    -> code
    ).toList.map{
      case (k, v) => s"$k=$v"
    }.mkString("&")

    val requestString = s"http://developer.echonest.com:80/api/v4/song/identify?$requestParameters"

    import Lenses._

    Http(url(requestString) OK as.String).fold({ throwable =>
      s"Exception requesting song: [$throwable]".left
    }, { data =>
      Parse.parse(data).map { json => (idLens.get(json) |@| titleLens.get(json) |@| artistLens.get(json)) { Song.apply } }
    })
  }
}

object Lenses {
  val codeLens = jArrayPL >=> jsonArrayPL(0) >=> jObjectPL >=> jsonObjectPL("code") >=> jStringPL

  private val songResponsePL = jObjectPL >=> jsonObjectPL("response") >=> jObjectPL >=> jsonObjectPL("songs") >=> jArrayPL >=> jsonArrayPL(0) >=> jObjectPL

  val idLens = songResponsePL >=> jsonObjectPL("id") >=> jStringPL
  val artistLens = songResponsePL >=> jsonObjectPL("artist_name") >=> jStringPL
  val titleLens = songResponsePL >=> jsonObjectPL("title") >=> jStringPL
}