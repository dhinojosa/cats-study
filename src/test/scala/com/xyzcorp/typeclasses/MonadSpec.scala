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
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import cats._
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

import scala.language.{postfixOps, reflectiveCalls}

class MonadSpec extends FunSpec with Matchers {
  describe("Monad") {
    it("is defined by the following typeclass") {
      trait Monad[F[_]] {
        def pure[A](value: A): F[A]
        def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
      }
    }

    it("is defined for an option") {
      import cats.Monad
      import cats.instances.option._ // for Monad
      val opt1 = Monad[Option].pure(3) //Some(3)
      val maybeInt: Option[Int] = Monad[Option]
        .flatMap(opt1)(x => Monad[Option].pure(2 * x))
      maybeInt should be(Some(6))
    }

    it("is also defined for a list") {
      import cats.Monad
      import cats.instances.list._ // for Monad
      val list1 = Monad[List].pure(3)
      val result = Monad[List].flatMap(list1)(x => List(-x, x, x + 1))
      result should be(List(-3, 3, 4))
    }

    it("is also defined for a future") {
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent._
      import scala.concurrent.duration._

      val fm: Future[Int] = Monad[Future]
        .flatMap(Future.successful(5))(x => Future.successful(x * 3))

      Await.ready(fm, 1 second).map(x => x should be(15))
    }

    it("""has a pure that can be added implicitly to any type
         |  as long as it matches""".stripMargin) {
      import cats.instances.list._
      import cats.instances.option._
      import cats.syntax.applicative._ // for pure

      1.pure[Option] should be(Some(1))
      10.pure[List] should be(List(10))
    }

    it("""can be used in a for comprehension, since flatmaps and
         |  maps can convert""".stripMargin) {
      import scala.language.higherKinds

      def sumSquare[F[_]](a: F[Int], b: F[Int])(implicit mon: Monad[F]): F[Int] =
        for {
          x <- a
          y <- b
        } yield x * x + y * y

      sumSquare(List(1, 2, 3, 4), List(5, 6, 7, 8)) should be
      List(26, 37, 50, 65, 29, 40, 53, 68, 34, 45, 58, 73, 41, 52, 65, 80)

      sumSquare(Option.apply(1), Option.apply(3)) should be
      Option(10)
    }
  }

  describe("The laws of a Monad") {
    it("""must adhere to the Left Identity Rule: Calling pure and transforming the
         |  result with func is the same as calling func""".stripMargin) {
      val func = (x: Int) => (x + 12).pure[List]
      val a = 2
      a.pure[List].flatMap(func) should be(func(a))
    }

    it("""must adhere to the Right Identity Rule: passing pure to flatMap
         |  is the same as doing nothing""".stripMargin) {
      val m = 2.pure[List]
      m.flatMap(_.pure[List]) should be(m)
    }

    it("""must adhere to the Associativity Rule: flatMapping over two func􏰀ons
         |  f and g is the same as flatMapping over
         |  f and then flatMapping over g""".stripMargin) {

      val m = 2.pure[List]
      m.flatMap(_.pure[List]) should be(m)
    }

    it("""can be used in a method signature to ensure that a
         |  higher-kinded type has flatMap""".stripMargin) {
      def process[M[_]: Monad, A](x: M[A], y: M[A])(implicit monoid: Monoid[A]): M[A] = {
        for {
          i <- x
          j <- y
        } yield (monoid.combine(i, j))
      }

      val maybeInt = process(Option(1), (Option(2)))
      maybeInt
    }

    it("has another method") {
      pending
    }
  }

  describe("iterateUntil") {
    it("iterates until it meets a certain condition") {
      pending
    }

    it("can be brought into a method, through an implicit") {
      pending

    }
  }

  describe("iterateUntilM") {
    it("iterates until it meets a certain condition with a monad") {
      pending
      val result = Monad[List].iterateUntilM(0)(n => List(n + 1))(_ < 10)
      result should be(List(1, 2, 3))
    }
    it("can be brought into a method, through an implicit with a monad") {
      pending
      def testIterate[F[_]: Monad, A: Eq](f: F[A], a: A) = {
        f.iterateUntil(a.eqv)
      }

      testIterate(List(1, 2, 3, 4, 5), 4) should be(List(1, 2, 3))
    }
  }
}
