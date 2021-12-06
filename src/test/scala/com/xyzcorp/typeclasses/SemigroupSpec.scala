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
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON INFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import cats.*
import cats.implicits.*
import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*

class SemigroupSpec extends AnyFunSpec with Matchers:

  describe("Semigroup combines items") {
    it("""uses the product method to combine the containers, here is an Option""") {
      val result = Semigroup[String].combine("All", " Right")
      result should be("All Right")
    }

    it("""can be brought into a method, to ensure that it applies to a signature""") {
      def fact[F[_]: Foldable, A](xs: F[A])(using monoid: Monoid[A]): Option[A] =
        xs.reduceLeftOption(monoid.combine)

      given Monoid[Int] with
        override def empty: Int = 0
        override def combine(x: Int, y: Int): Int = x * y

      fact(List(1, 2, 3, 4)) should be(Some(24))
    }
  }
