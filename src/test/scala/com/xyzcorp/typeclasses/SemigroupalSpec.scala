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
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import org.scalatest.{FunSpec, Matchers}

class SemigroupalSpec extends FunSpec with Matchers {
  describe("Semigroupal combines contexts") {
    it(
      """combines two containers F[A] and F[B] and brings together using
        |  F[(A,B)] which uses a tuple and is defined with the
        |  following type class.""".stripMargin) {

      trait Semigroupal[F[_]] {
        def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
      }
    }
    it("""uses the product method to combine the containers, here is an Option""") {
      import cats._
      import cats.implicits._

      val opt1 = Option(3)
      val opt2 = Option("Player")
      val result = Semigroupal[Option].product(opt1, opt2)
      result should be(Option(3 -> "Player"))
    }

    it("""uses the product method to combine the containers, here is an List""") {
      import cats._
      import cats.implicits._

      val xs1 = List(3, 2, 1)
      val xs2 = List('a', 'b')
      val result = Semigroupal[List].product(xs1, xs2)
      result should be(List((3,'a'), (3,'b'), (2,'a'), (2,'b'), (1,'a'), (1,'b')))
    }

    it("""will render None, for option, if any combination contains
        |  a None""".stripMargin) {
      import cats._
      import cats.implicits._

      val opt1 = Option(3)
      val opt2 = Option.empty[String]
      val result = Semigroupal[Option].product(opt1, opt2)
      result should be(None)
    }
    it(
      """will render an empty List, for List, if any combination contains
        |  an empty list """.stripMargin) {
      import cats._
      import cats.implicits._

      val opt1 = List(1,2,3,4,5)
      val opt2 = List.empty[String]
      val result = Semigroupal[List].product(opt1, opt2)
      result should be(List())
    }
    it("""creating our own Semigroupal for Either""".stripMargin) {
      import cats._
      type RightOnly[A] = Either[String, A]

      implicit val ro: Semigroupal[RightOnly] = new Semigroupal[RightOnly] {
        override def product[A, B](fa: RightOnly[A],
                                   fb: RightOnly[B]): RightOnly[(A, B)] = {
          for {a <- fa
               b <- fb} yield a -> b
        }
      }
      val opt1:Either[String, Int] = Right(40)
      val opt2:Either[String, Float] = Right(10.0f)
      val result = Semigroupal[RightOnly].product(opt1, opt2)
      result should be(Right(40 -> 10.0f))
    }
    it(
      """creating our own Semigroupal for
        |  Either but using an implicit conversion""".stripMargin) {
//      import cats._
//
//      type RightOnly[A] = Either[String, A]
//      val opt1:Either[String, Int] = Right(40)
//      val opt2:Either[String, Float] = Right(10.0f)
//
//      val result = Semigroupal[RightOnly].product(opt1, opt2)
//      result should be(Right(40 -> 10.0f))
    }

  }
}
