package com.noelmarkham.whatsong

import scalaz._
import Scalaz._
import com.twitter.util.Future

object Implicits {

  private[this] case class F() extends Monad[Future] {
    def point[A](a: => A): Future[A] = Future(a)
    def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
  }

  implicit val futureInstance: Monad[Future] = new F()
}
