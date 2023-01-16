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
import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*

class SemigroupalSpec extends AnyFunSpec with Matchers:

  trait MySemigroupal[F[_]]:
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  describe("Semigroupal combines contexts") {
    it("""combines two containers F[A] and F[B] and brings together using
         |  F[(A,B)] which uses a tuple and is defined with the
         |  following type class.""".stripMargin) {
      val nonEmptyListInts = NonEmptyList.of(40, 10, 22, 44)
      val nonEmptyListChars = NonEmptyList.of('a', 'b', 'c', 'd')
      val result = Semigroupal[NonEmptyList]
        .product(nonEmptyListInts, nonEmptyListChars)
      result should be(
        NonEmptyList.of((40, 'a'),
                        (40, 'b'),
                        (40, 'c'),
                        (40, 'd'),
                        (10, 'a'),
                        (10, 'b'),
                        (10, 'c'),
                        (10, 'd'),
                        (22, 'a'),
                        (22, 'b'),
                        (22, 'c'),
                        (22, 'd'),
                        (44, 'a'),
                        (44, 'b'),
                        (44, 'c'),
                        (44, 'd')
        )
      )
    }

    it("""uses the product method to combine the containers, here is an Option""") {
      val opt1 = Option(3)
      val opt2 = Option("Player")
      val result = Semigroupal[Option].product(opt1, opt2)
      result should be(Option(3 -> "Player"))
    }

    it("""uses the product method to combine the containers, here is an List""") {
      val xs1 = List(3, 2, 1)
      val xs2 = List('a', 'b')
      val result = Semigroupal[List].product(xs1, xs2)
      result should be(List((3, 'a'), (3, 'b'), (2, 'a'), (2, 'b'), (1, 'a'), (1, 'b')))
    }

    it("""will render None, for option, if any combination contains
         |  a None""".stripMargin) {
      val opt1 = Option(3)
      val opt2 = Option.empty[String]
      val result = Semigroupal[Option].product(opt1, opt2)
      result should be(None)
    }

    it("""will render an empty List, for List, if any combination contains
         |  an empty list """.stripMargin) {
      val opt1 = List(1, 2, 3, 4, 5)
      val opt2 = List.empty[String]
      val result = Semigroupal[List].product(opt1, opt2)
      result should be(List())
    }

    it("""will render something interesting for Valid?""".stripMargin) {
      val valid1 = Valid("Nice")
      val valid2 = Valid("Great")
      val result = Semigroupal[[A] =>> Validated[String, A]].product(valid1, valid2)
      result should be(Valid("Nice", "Great"))
    }

    it("""will render something interesting for any Invalid?""".stripMargin) {
      val valid1 = Valid("Nice")
      val valid2 = Invalid("Do not like")
      val result = Semigroupal[[A] =>> Validated[String, A]]
        .product(valid1, valid2)
      result should be(Invalid("Do not like"))
    }

    it("""will render something interesting for any Invalid That is Layered?""".stripMargin) {
      val valid1 = Valid("Nice")
      val valid2 = Invalid(List("Do not like"))
      val valid3 = Invalid(List("Do not like at all"))
      val semigroupal = Semigroupal[[A] =>> Validated[List[String], A]]
      val result = semigroupal.product(valid3, semigroupal.product(valid1, valid2))
      result should be (Invalid(List("Do not like at all", "Do not like")))
    }

    it("""creating our own Semigroupal for Either""".stripMargin) {
      type RightOnly = [A] =>> Either[String, A]

      given Semigroupal[RightOnly] with
        def product[A, B](fa: RightOnly[A], fb: RightOnly[B]): RightOnly[(A, B)] =
          for
            a <- fa
            b <- fb
          yield a -> b

      val opt1: Either[String, Int] = Right(40)
      val opt2: Either[String, Float] = Right(10.0f)
      val result = Semigroupal[RightOnly].product(opt1, opt2)
      result should be(Right(40 -> 10.0f))
    }
  }
