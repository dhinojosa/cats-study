/*
 * Copyright 2021 Daniel Hinojosa
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
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses
import cats._
import cats.data.Ior.Both
import cats.implicits._
import cats.data._
import org.scalatest._
import matchers.should._
import funspec.AnyFunSpec

class BiFunctorSpec extends AnyFunSpec with Matchers {

  describe("""A type class of types which give rise to two independent,
             |  covariant functors. The important is bimap""".stripMargin) {
    it("""has bimap with applies a map to both sides of a F[A,B],
         |  def bimap[A, B, C, D](fab: F[A, B])
         |    (f: A => C, g: B => D): F[C, D]""".stripMargin) {
      val either: Either[Throwable, Int] = Right(40)
      val result = Bifunctor[Either].bimap(either)(t => t.getMessage, i => s"${i}!")
      result should be(Right("40!"))
    }

    it("""has bimap with applies a map to both sides of a F[A,B],
         |  def bimap[A, B, C, D](fab: F[A, B])
         |    (f: A => C, g: B => D): F[C, D].
         |    This will use the Left""".stripMargin) {
      val either: Either[Throwable, Int] =
        Left(new Throwable("Unable to complete"))
      val result = Bifunctor[Either]
        .bimap(either)(t => t.getMessage, i => s"${i}!")
      result should be(Left("Unable to complete"))
    }

    it("""has a right functor which returns a right
         |  functor for the F[A,B]""".stripMargin) {
      val either: Either[Throwable, Int] = Left(new Throwable("Unable to complete"))
      val leftFunctor = Bifunctor[Either].leftFunctor[Int]
      val message = leftFunctor.fmap(either)(_.getMessage)
      message should be(Left("Unable to complete"))
    }

    it("""can also compose with other BiFunctors, given the following,
         |  the end result of these types would be """.stripMargin) {
      val bifunctorCompose = Bifunctor[Either].compose[Ior]
      val both: Ior[String, Int] = Both("Hello", 10)
      val eitherRight: Either[Ior[String, Int], Ior[String, Int]] = Right(both)
      val result: Either[Ior[String, Int], Ior[String, Int]] =
        bifunctorCompose.bimap(eitherRight)(s => s"$s!", i => i * 100)
      result should be(Right(Both("Hello!", 1000)))
    }

    it("""has a widen on both left and right to use a higher type""") {
      trait Human {
        def firstName: String
        def lastName: String
      }
      abstract class American extends Human
      case class NewMexican(firstName: String, lastName: String) extends American

      val iorBoth = Ior.both(NewMexican("Cesar", "Lazario"), 40)
      val result: Ior[American, Int] = Bifunctor[Ior].leftWiden(iorBoth)
      result shouldBe an[Ior[American, Int]]
    }
  }
}
