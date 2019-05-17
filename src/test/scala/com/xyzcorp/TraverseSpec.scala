/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
 * IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

package com.xyzcorp

import org.scalatest.{FunSpec, Matchers}


class TraverseSpec extends FunSpec with Matchers {

  describe(
    """Traverse and Sequence in the Scala Standard Library""".stripMargin) {
    it("is used in the Scala Standard Library, like in the case of Future") {
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent._
      import scala.concurrent.duration._

      val hostnames = List("alpha.example.com",
        "beta.example.com",
        "gamma.demo.com")

      def getUptime(hostname: String): Future[Int] = Future(
        hostname.length * 60)

      info(
        "The following is less desireable, since this returns a Future[List[Int]]")

      val allUptimes: Future[List[Int]] = hostnames
        .foldLeft(Future(List.empty[Int])) {
          (accum, host) =>
            val uptime = getUptime(host)
            for {
              accum <- accum
              uptime <- uptime
            } yield accum :+ uptime
        }

      Await.result(allUptimes, 1.second) should be(List(1020, 960, 840))

      info(
        """Instead, we can then use traverse to wrap all individual futures
          |  as one, and then perhaps block that single Future or just process
          |  it asynchronously.""".stripMargin)

      val allUptimesTraversed: Future[List[Int]] =
        Future.traverse(hostnames)(getUptime)
      Await.result(allUptimesTraversed, 1.second) should
        be(List(1020, 960, 840))
    }

    it(
      """is defined somewhat like the following in the standard
        |  library, in the Future. It abstracts away all the pain
        |  of doing this by hand.""".stripMargin) {
      import scala.concurrent.Future
      import scala.concurrent.ExecutionContext.Implicits.global
      def traverse[A, B](values: List[A])
                        (func: A => Future[B]): Future[List[B]] =
        values.foldLeft(Future(List.empty[B])) { (accum, host) =>
          val item: Future[B] = func(host)
          for {
            a <- accum
            i <- item
          } yield a :+ i
        }
    }

    it(
      """also has a cousin called Sequence that assumes we’re
        |  starting with a List[Future[B]] and
        |  don’t need to provide an identity function.""".stripMargin) {

      import scala.concurrent.Future
      import scala.concurrent.ExecutionContext.Implicits.global

      def traverse[A, B](values: List[A])
                        (func: A => Future[B]): Future[List[B]] =
        values.foldLeft(Future(List.empty[B])) { (accum, host) =>
          val item: Future[B] = func(host)
          for {
            a <- accum
            i <- item
          } yield a :+ i
        }

      info("NOTE: Identity function comes from Scala Predef")

      def sequence[B](futures: List[Future[B]]): Future[List[B]] =
        traverse(futures)(identity)
    }
  }

  describe(
    """Cat's Traverse generalizes to work with any Applicative, for example
      |  Future, Option, and Validated, to provide
      |  a more convenient,
      |  more lawful, pattern for iteration""".stripMargin) {

    it("""can be used with an Option""") {

    }
  }
}
