/*
 * Copyright 2018 Daniel Hinojosa
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
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
 * IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

package com.xyzcorp

import cats._
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

class FoldableSpec extends FunSpec with Matchers {

  describe("Foldable abstracts the familiar foldLeft and foldRight operations")
  {
    it(
      """folds elements like fold in the standard library,
        |  but uses a type class for it.
        |  Here is a fold with a list. It also uses a Monoid
        |  to combine the elements""".stripMargin) {
      val result = Foldable[List].fold(List("a", "b", "c"))
      result should be("abc")
    }

    it(
      """folds elements like fold in the standard library,
        |  but uses a type class for it.
        |  Here is a fold with a list. It also uses a Monoid
        |  to combine the elements, here it uses the Monoid[Int]
        |  which is of course is addition""".stripMargin) {
      val result = Foldable[List].fold(List(1, 2, 3))
      result should be(6)
    }

    it(
      """can be used to fold left using a show method (not related to the
        |  show method to
        |  print out what happens in a fold""".stripMargin) {
      def show[A](list: List[A]): String =
        list.foldLeft("nil")((accum, item) => s"($item then $accum)")

      show(List(1, 2, 3, 4)) should
        be("(4 then (3 then (2 then (1 then nil))))")
    }

    it(
      """can be used to fold right using a show method (not related to the
        |  show method to
        |  print out what happens in a fold""".stripMargin) {

      def show[A](list: List[A]): String =
        list.foldRight("nil")((item, accum) => s"($item then $accum)")

      show(List(1, 2, 3, 4)) should
        be("(1 then (2 then (3 then (4 then nil))))")
    }

    it("""can be used with a list as an accumulator""".stripMargin) {
      val result = Foldable[List]
        .foldLeft(List(1, 2, 3, 4), List.empty[Int])((xs, x) => x :: xs)
      result should be(List(4, 3, 2, 1))
    }

    it("""can be used to implement a map using a fold right""") {
      Foldable[List].foldRight(List(1, 2, 3, 4), Later.apply(List.empty[Int]))(
        (next, evalB) => evalB.map(xs => next :: xs))
    }
  }

  describe("FoldMap") {
    it(
      """will apply a map to all the elements
        |  before folding them""".stripMargin) {

      pending

//      def caesarShift(w:String, shift:Int) = {
//        Foldable[Char].foldMap(w)(c => c - 97 + ((shift % 26) + % 26) + 97)
//      }
      val result = Foldable[List]
        .foldMap(List("a", "b", "c"))(s => (s.charAt(0) - 97 + (1 % 26)).toChar.toString)
      result should be("bcd")
    }
  }
}
