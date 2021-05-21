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

import org.scalatest._
import matchers.should._
import funspec.AnyFunSpec
import cats._
import cats.implicits._

class SemigroupSpec extends AnyFunSpec with Matchers {
  describe("Semigroup combines contexts") {
    it("""combines two containers F[A] and F[B] and brings together using
         |  F[(A,B)] which uses a tuple and is defined with the
         |  following type class.""".stripMargin) {

      trait Semigroupal[F[_]] {
        def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
      }
    }

    it("""uses the product method to combine the containers, here is an Option""") {
      val result = Semigroup[String].combine("All", " Right")
      result should be ("All Right")
    }

    it("""can be brought into a method, to ensure that it applies to a signature""") {
      def fact[F[_]:Foldable, A](xs:F[A])(implicit monoid:Monoid[A]):Option[A] = {
        xs.reduceLeftOption{case (x1, x2) => monoid.combine(x1, x2)}
      }
      implicit val mi: Monoid[Int] = new Monoid[Int] {
          override def empty: Int = 0
          override def combine(x: Int, y: Int): Int = x * y
      }
      fact(List(1,2,3,4)) should be (Some(10))
    }
  }
}
