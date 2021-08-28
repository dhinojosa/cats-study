/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the
 * Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to
 * do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice
 * shall be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
 * ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR
 * A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
 * EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
 * OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes

import cats.data._
import cats._
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

class CokleisliSpec extends FunSpec with Matchers {
  describe("Cokleisli Data Type") {

    it("is a wrapper around the function F[B] => A") {
      final case class MyCoKleisli[F[_], A, B](run: F[B] => A)
    }

    it("""When a cokleisli runs it produces the function.""") {
      val cokleisli = Cokleisli.apply[List, Int, String](_.mkString(","))
      val function = cokleisli.run
      function(List(1, 2, 3, 4)) should be("1,2,3,4")
    }

    it("""can perform a map which transforms the end result""") {
      val cokleisli = Cokleisli.apply[List, Int, String](_.mkString(","))
      val function = cokleisli.map(s => s.find(_ == 'r')).run
      function(List(1, 2, 3, 4)) should be(None)
    }

    it(
        """can perform a compose, in this compose we are performing:
          |
          |  a: (List[Int] => String)
          |  b: (List[String] => Int)
          |  where the compose is interestingly decomposes from the front
          |
          |  (List[String]
          |""".stripMargin) {

      val cokleisli = Cokleisli[List, Int, String]{xs =>
          println("A:" + xs)
          xs.mkString(",")
      }
      val function = cokleisli.compose(Cokleisli[List, String, Int]{xs =>
          println("B:" + xs)
          xs.mkString("").length
      })
      val result = function.run
      result(List("One", "Two", "Three")) should be("11,8,5")
    }

    it("""has a first method, that is used to get the first element""") {
        val cokleisli = Cokleisli[NonEmptyList, Int, String](_.mkString_(","))
        val f = cokleisli.first[Long].run
        val result = f(NonEmptyList.of((1,2), (5, 4), (6, 9)))
        result should be ("1,5,6" -> 2)
    }

      it("""has a second method, that is used to get the second element""") {
          val cokleisli = Cokleisli[NonEmptyList, Int, String](_.mkString_(","))
          val f = cokleisli.second[Long].run
          val result = f(NonEmptyList.of((1,2), (5, 4), (6, 9)))
          result should be (1 -> "2,4,9")
      }
  }
}
