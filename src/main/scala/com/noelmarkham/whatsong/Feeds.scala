package com.noelmarkham.whatsong

import scalaz._
import Scalaz._
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.RequestBuilder
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.buffer._
import java.io._
import org.apache.commons.io.IOUtils
import argonaut._
import Argonaut._
import java.nio.charset.Charset
import java.net.URL

case class Song(title: String, artist: String) {
  def describe = s"$title by $artist"
}

object Feeds {

  def getPlaylist(host: String, path: String): Future[String] = {
    val client: Service[HttpRequest, HttpResponse] = Http.newService(host)

    val request = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, path)

    client(request).map {httpResponse =>
      httpResponse.getContent.toString(Charset.forName("UTF-8"))
    }
  }

  def getPlaylistFeed(playlistData: String): Option[String] = {
    playlistData.lines.filter(_.startsWith("File1=")).map(s => s.drop("File1=".length)).find(_ => true)
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

  def getFingerprintJson(streamData: File): Future[Option[Json]] = {
    Future {
      import scala.sys.process._
      val filename = streamData.getAbsolutePath
      val process = Process(s"/usr/local/bin/echoprint-codegen $filename")

      val output = process.!!
      streamData.delete()
      Parse.parseOption(output)
    }
  }

  def getFingerprint(json: Json): Option[String] = {
    val codeLens = jArrayPL >=> jsonArrayPL(0) >=> jObjectPL >=> jsonObjectPL("code") >=> jStringPL
    codeLens.get(json)
  }

  def requestSong(code: String, apiKey: String): Future[Option[Song]] = {
    val client: Service[HttpRequest, HttpResponse] = Http.newService("developer.echonest.com:80")

    val requestParameters = Map(
      "api_key" -> apiKey,
      "version" -> "4.12",
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
        val artistLens = jObjectPL >=> jsonObjectPL("response") >=> jObjectPL >=> jsonObjectPL("songs") >=> jArrayPL >=> jsonArrayPL(0) >=> jObjectPL >=> jsonObjectPL("artist_name") >=> jStringPL
        val titleLens = jObjectPL >=> jsonObjectPL("response") >=> jObjectPL >=> jsonObjectPL("songs") >=> jArrayPL >=> jsonArrayPL(0) >=> jObjectPL >=> jsonObjectPL("title") >=> jStringPL

        (titleLens.get(json) |@| artistLens.get(json)){Song(_, _)}
      }
    }
  }
}
