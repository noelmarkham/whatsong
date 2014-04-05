package com.noelmarkham.whatsong

import scalaz._
import Scalaz._
import com.twitter.util.Future

object Implicits {

  private[this] case class MonadFuture() extends Monad[Future] {
    def point[A](a: => A): Future[A] = Future(a)
    def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
  }

  implicit val futureInstance: Monad[Future] = new MonadFuture()

  private[this] case class EqualSong() extends Equal[Song] {
    def equal(s1: Song, s2: Song) = (s1.id === s2.id) || (s1.artist.toLowerCase == s2.artist.toLowerCase && s1.title.toLowerCase == s2.title.toLowerCase)
  }

  implicit val equalSongInstance: Equal[Song] = new EqualSong()
}
