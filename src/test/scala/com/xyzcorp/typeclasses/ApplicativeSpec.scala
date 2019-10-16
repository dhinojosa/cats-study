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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import cats._
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

import scala.language.postfixOps

class ApplicativeSpec extends FunSpec with Matchers {
  describe("Applicative") {
    it(
        """extends Functor with ap, where the first argument is F[A=>B]
          | and the second is F[A]. It is distinctly different than a Functor
          | in that we can wrap a function in a context""".stripMargin)
    {
      val result = Applicative[List].ap(List((x: Int) => x + 1))(List(1, 2, 3))
      result should be(List(2, 3, 4))
    }
    it("extends Functor with pure that creates the Applicative") {
      val intApplicative: List[Int => Int] = Applicative[List]
        .pure((x: Int) => x + 1)
      val result = intApplicative.ap(List(1, 2, 3))
      result should be(List(2, 3, 4))
    }
    it("introduces a <*> operation that is the same as ap") {
      val result = Applicative[Option].<*>(Some[Int => Int](4 *))(Some(3))
      result should be(Some(12))
    }
    it("can be a variable that is extracted and used as an applicative") {
      val ao = Applicative[Option]
      val result = ao.<*>(Some[Int => Int](4 *))(Some(3))
      result should be(Some(12))
    }
    it("can also be imported so as to just include the operator") {
      val ao = Applicative[Option]
      import ao.<*>
      val result = <*>(Option((x: Int) => 4 * x))(Some(3))
      result should be(Some(12))
    }
    it("can also be applied in as an infix operator") {
      val ao = Applicative[Option]
      val result = Option((x: Int) => 4 * x) <*> Some(3)
      result should be(Some(12))
    }
    it(
      """can do <*> with whatever kind of F[_].
        | The resulting list has every possible combination
        | of applying a function from the left list to a value in the right one.
        |""".stripMargin) {
      val axs = Applicative[List]
      val result = List[Int => String](_.toString, _.toHexString) <*>
        List(1, 20, 30, 40)
      result should be(List("1", "20", "30", "40", "1", "14", "1e", "28"))
    }
    it("""can do also with Function[A,B] where we can apply both sides""") {
      val axs = Applicative[List]
      val functions = List[(Int, Int) => Int](_ + _, _ * _)
      val result = functions.ap2(List(1, 2, 3), List(4, 5, 6))
      result should
        be(List(5, 4, 6, 5, 7, 6, 6, 8, 7, 10, 8, 12, 7, 12, 8, 15, 9, 18))
    }
  }
}
