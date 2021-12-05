/*
 * Copyright 2020 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
 * OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

package com.xyzcorp.typeclasses
import cats.arrow.Arrow
import cats.data.{Cokleisli, NonEmptyList}
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

class ArrowSpec extends FunSpec with Matchers {
  describe("""Arrows is a typeclass that supports
             |  Kleisli and Cokleisli and FancyFunction""".stripMargin) {
    it("has...") {
      val intToIntArrow = Arrow[Function].lift((x: Int) => x * 2)
    }

    it("has a way to lift") {
      type CokleisliNonEmptyList[A, B] = Cokleisli[NonEmptyList, A, B]
      val value1 = Arrow[CokleisliNonEmptyList]
        .lift[Int, String](i => i.toBinaryString)
      val str: String = value1.run(NonEmptyList.of(120, 200, 300))
      str should be("1111000")
    }
  }
}
