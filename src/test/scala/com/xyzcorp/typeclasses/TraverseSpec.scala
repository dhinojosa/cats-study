/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
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
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import cats.*
import cats.implicits.*
import org.scalatest.*
import matchers.should.*
import funspec.AnyFunSpec

class TraverseSpec extends AnyFunSpec with Matchers:

  describe("""Traverse and Sequence in the Scala Standard Library""".stripMargin) {
    it("is used in the Scala Standard Library, like in the case of Future") {
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.*
      import scala.concurrent.duration.*

      val hostnames = List("alpha.example.com", "beta.example.com", "gamma.demo.com")

      def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

      info("The following is less desirable, since this returns a Future[List[Int]]")

      val allUptimes: Future[List[Int]] = hostnames
        .foldLeft(Future(List.empty[Int])) { (accum, host) =>
          val uptime = getUptime(host)
          for
            accum <- accum
            uptime <- uptime
          yield accum :+ uptime
        }

      Await.result(allUptimes, 1.second) should be(List(1020, 960, 840))

      info("""Instead, we can then use traverse to wrap all individual futures
             |  as one, and then perhaps block that single Future or just process
             |  it asynchronously.""".stripMargin)

      val allUptimesTraversed: Future[List[Int]] =
        Future.traverse(hostnames)(getUptime)
      Await.result(allUptimesTraversed, 1.second) should
        be(List(1020, 960, 840))
    }

    it("""is defined somewhat like the following in the standard
         |  library, in the Future. It abstracts away all the pain
         |  of doing this by hand.""".stripMargin) {
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.Future
      def traverse[A, B](values: List[A])(func: A => Future[B]): Future[List[B]] =
        values.foldLeft(Future(List.empty[B])) { (accum, host) =>
          val item: Future[B] = func(host)
          for
            a <- accum
            i <- item
          yield a :+ i
        }
    }

    it("""also has a cousin called Sequence that assumes we’re
         |  starting with a List[Future[B]] and
         |  don’t need to provide an identity function.""".stripMargin) {

      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.Future

      def traverse[A, B](values: List[A])(func: A => Future[B]): Future[List[B]] =
        values.foldLeft(Future(List.empty[B])) { (accum, host) =>
          val item: Future[B] = func(host)
          for
            a <- accum
            i <- item
          yield a :+ i
        }

      info("NOTE: Identity function comes from Scala Predef")

      def sequence[B](futures: List[Future[B]]): Future[List[B]] =
        traverse(futures)(identity)
    }
  }
  describe("""Cat's Sequence generalizes to work with any Applicative,
             |  for example Future, Option, and Validated, to provide
             |  a more convenient, more lawful, pattern for iteration
             |  The pattern that sequence generally holds is:
             |  (fa: F[G[A]]): G[F[B]]. Therefore, it must start with a nested
             |  Applicative.""".stripMargin) {
    it("""can be used to perform a sequence. Sequence takes a F[G[A]]
         |  and converts it to G[F[B]].
         |""".stripMargin) {
      import cats.*
      import cats.implicits.*
      val original: Option[List[Int]] = List(1, 2, 3).some
      val result: Seq[Option[Int]] = Traverse[Option].sequence(original)
      result should be(List(1.some, 2.some, 3.some))
    }
  }

  describe("""Cat's Traverse generalizes to work with any Applicative,
             |  for example Future, Option, and Validated, to provide
             |  a more convenient, more lawful, pattern for iteration
             |  The pattern that traverse generally holds is:
             |  (fa: F[A])(f: A => G[B]): G[F[B]]. Notice it doesn't
             |  necessarily have to start with a nested Applicative
             |  """.stripMargin) {

    it("""can be take a List[A] and a function A => G[B] that converts
         |  where in this case A is an Int and G is an Option and B
         |  is a transformation of A. The end result is G[F[B]], and given
         |  that G is an Option, F is a List, and B is a transformation.
         |""".stripMargin) {
      import cats.*
      import cats.implicits.*
      val actual = Traverse[List].traverse(List(1, 2, 3))(_.some)
      val expected = Some(List(1, 2, 3))
      actual should be(expected)
    }

    it("""can transform the A to something different, so given
         |  List[A] and function A => G[B] where B is transformed from
         |  A. The End Result is G[F[B]]. In this case F is List, G is
         |  Option, A is Int, and B is String. The result type is
         |  List[Option[String]]
         |""".stripMargin) {
      import cats.*
      import cats.implicits.*
      val actual = Traverse[List]
        .traverse(List(1, 2, 3))(_.toHexString.some)
      val expected = Some(List("1", "2", "3"))
      actual should be(expected)
    }
  }

  describe("""The difference between a sequence and a traverse""") {
    it("""doesn't require an nested Applicative for a traverse,
         |  but it does require a function""".stripMargin) {
      import cats.*
      import cats.implicits.*
      val list = List(1, 2, 3, 4)
      val actual = Traverse[List].traverse(list)(_.some)
      val expected = Some(List(1, 2, 3, 4))
      actual should be(expected)
    }
    it("""does require a nested Applicative for a sequence
         |  but no function""".stripMargin) {
      import cats.*
      import cats.implicits.*
      val list = List(1.some, 2.some, 3.some, 4.some)
      val actual = Traverse[List].sequence(list)
      val expected = Option(List(1, 2, 3, 4))
      actual should be(expected)
    }
  }

  describe("""Operations available""") {
    it("""can be used with compose which composes two
         |  Traverse types. In the following example,
         |  we are composing a List with an Option""".stripMargin) {
      import cats.*
      import cats.implicits.*
      val original = List(List(1, 2).some, List(4, 5).some)
      val result = Traverse[List]
        .compose[Option]
        .sequence(original)
      val expected = List(
        List(Some(1), Some(4)),
        List(Some(1), Some(5)),
        List(Some(2), Some(4)),
        List(Some(2), Some(5))
      )
      result should be(expected)
    }

    it("""has a flatTraverse which flattens the inner
         |  result, and turns it inside out.""".stripMargin) {
      def parseInt(s: String): Option[Int] =
        Either.catchOnly[NumberFormatException](s.toInt).toOption
      val optionOfAList = Option(List("1", "two", "3"))
      val result = optionOfAList.flatTraverse(_.map(parseInt))
      result should be(List(Some(1), None, Some(3)))
    }

    it("""has a traverseWithIndex which also does a map
         |  like computation while turning it inside out. This performs the
         |  traversal in a single pass but requires that effect G is monadic.
         |  An applicative traversal can be performed in two passes using
         |  zipWithIndex followed by traverse.""".stripMargin) {

      val result = Traverse[List]
        .traverseWithIndexM(List(1, 2, 3, 4))((a, i) => Option(s"${i}. $a"))
      result should be(Some(List("0. 1", "1. 2", "2. 3", "3. 4")))
    }
  }
