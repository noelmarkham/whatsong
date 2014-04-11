package com.noelmarkham.whatsong

import scalaz._
import Scalaz._
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.RequestBuilder
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._
import java.io._
import org.apache.commons.io.IOUtils
import argonaut._
import Argonaut._
import java.nio.charset.Charset
import java.net.URL

case class Song(id: String, title: String, artist: String) {
  def describe = s"$title by $artist"
}

object Feeds {

  def getPlaylist(host: String, path: String): Future[String] = {
    val client: Service[HttpRequest, HttpResponse] = Http.newService(host)
    val request = RequestBuilder().url(s"http://$host$path").buildGet
    client(request).map {httpResponse =>
      httpResponse.getContent.toString(Charset.forName("UTF-8"))
    }
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

  def requestSong(code: String, apiVersion: String, apiKey: String): Future[Option[Song]] = {
    val client: Service[HttpRequest, HttpResponse] = Http.newService("developer.echonest.com:80")

    val requestParameters = Map(
      "api_key" -> apiKey,
      "version" -> apiVersion,
      "code" -> code
    ).toList.map{
      case (k, v) => s"$k=$v"
    }.mkString("&")

    val requestString = s"http://developer.echonest.com:80/api/v4/song/identify?$requestParameters"
    val request = RequestBuilder().url(requestString).buildGet

    client(request).map{ httpResponse =>
      val responseString = httpResponse.getContent.toString(Charset.forName("UTF-8"))
      val possibleJson = Parse.parseOption(responseString)
      possibleJson.flatMap { json =>
        val idLens = jObjectPL >=> jsonObjectPL("response") >=> jObjectPL >=> jsonObjectPL("songs") >=> jArrayPL >=> jsonArrayPL(0) >=> jObjectPL >=> jsonObjectPL("id") >=> jStringPL
        val artistLens = jObjectPL >=> jsonObjectPL("response") >=> jObjectPL >=> jsonObjectPL("songs") >=> jArrayPL >=> jsonArrayPL(0) >=> jObjectPL >=> jsonObjectPL("artist_name") >=> jStringPL
        val titleLens = jObjectPL >=> jsonObjectPL("response") >=> jObjectPL >=> jsonObjectPL("songs") >=> jArrayPL >=> jsonArrayPL(0) >=> jObjectPL >=> jsonObjectPL("title") >=> jStringPL

        (idLens.get(json) |@| titleLens.get(json) |@| artistLens.get(json)) { Song(_, _, _) }
      }
    }
  }
}
