package com.noelmarkham.whatsong

import scalaz._
import Scalaz._
import scala.concurrent.Future

object Implicits {

  implicit val futureInstance: Monad[Future] = new Monad[Future] {
    import scala.concurrent.ExecutionContext.Implicits.global
    def point[A](a: => A): Future[A] = Future.successful(a)
    def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
  }

  implicit val equalSongInstance: Equal[Song] = new Equal[Song] {
    def equal(s1: Song, s2: Song) = (s1.id === s2.id) || (s1.artist.toLowerCase === s2.artist.toLowerCase && s1.title.toLowerCase === s2.title.toLowerCase)
  }
}
