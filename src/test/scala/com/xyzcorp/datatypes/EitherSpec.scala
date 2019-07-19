/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes

import org.scalatest.{FunSpec, Matchers}

class EitherSpec extends FunSpec with Matchers {

  describe("Either") {
    it(
      """is already a type that encapsulates whether
        |  something fails of succeeds""".stripMargin) {

      def div(numerator:Int, denominator:Int):Either[String, Int] = {
        if (denominator == 0) Left("Div 0")
        else Right(numerator/denominator)
      }

      div(3, 1) should be (Right(3))
      div(3, 0) should be (Left("Div 0"))
    }
    it ("""can be shortened using Cats smart constructors and aids in
        |  type inference bugs""".stripMargin) {

      import cats.syntax.either._

      val a: Either[String, Int] = 3.asRight[String]
      val b: Either[String, Int] = 4.asRight[String]

      val result = for {
        x <- a
        y <- b
      } yield x*x + y*y

      result should be (Right(25))
    }
    it ("""contains a utility called biMap which will map for left,
        |  and right""".stripMargin) {
      import cats.syntax.either._
      val r = 6.asRight[String].bimap(s => s + "!", i => i * 2)
      r should be (Right(12))

      val s = "Hello".asLeft[Int].bimap(s => s + "!", i => i * 2)
      s should be (Left("Hello!"))
    }
  }
}
