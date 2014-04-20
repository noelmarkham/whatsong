package com.noelmarkham.whatsong

import scalaz._
import Scalaz._
import java.io._
import org.apache.commons.io.IOUtils
import argonaut._
import Argonaut._
import java.net.URL
import akka.io.IO
import akka.pattern.ask
import spray.can.Http
import spray.http._
import spray.client.pipelining._
import scala.concurrent.Future
import Implicits._

case class Song(id: String, title: String, artist: String) {
  def describe = s"$title by $artist"
}

object Feeds {

  def getPlaylist(url: String): Future[String \/ String] = {

    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
    val response = pipeline(Get(url))

    response.map{ _.entity.toOption.map { _.asString }.toRightDisjunction("Unable to retrieve playlist")}
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
    val codeLens = jArrayPL >=> jsonArrayPL(0) >=> jObjectPL >=> jsonObjectPL("code") >=> jStringPL
    codeLens
      .get(json)
      .toRightDisjunction(s"Cannot extract code from fingerprint application output: [$json]")
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

    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

    val response: Future[HttpResponse] = pipeline(Get(requestString))

    response.map { httpResponse =>
      val idLens = jObjectPL >=> jsonObjectPL("response") >=> jObjectPL >=> jsonObjectPL("songs") >=> jArrayPL >=> jsonArrayPL(0) >=> jObjectPL >=> jsonObjectPL("id") >=> jStringPL
      val artistLens = jObjectPL >=> jsonObjectPL("response") >=> jObjectPL >=> jsonObjectPL("songs") >=> jArrayPL >=> jsonArrayPL(0) >=> jObjectPL >=> jsonObjectPL("artist_name") >=> jStringPL
      val titleLens = jObjectPL >=> jsonObjectPL("response") >=> jObjectPL >=> jsonObjectPL("songs") >=> jArrayPL >=> jsonArrayPL(0) >=> jObjectPL >=> jsonObjectPL("title") >=> jStringPL

      for {
        entity <- httpResponse.entity.toOption.toRightDisjunction("Unable to parse response")
        json <- Parse.parse(entity.asString)
        song <- ((idLens.get(json) |@| titleLens.get(json) |@| artistLens.get(json)) { Song(_, _, _) }).right
      } yield song
    }.recover {
      case t => "Exception requesting song: [$t]".left
    }
  }
}
