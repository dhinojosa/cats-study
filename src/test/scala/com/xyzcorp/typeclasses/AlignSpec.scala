/*
 * Copyright 2021 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import cats.*
import cats.data.*
import cats.implicits.*
import org.scalatest.*
import matchers.should.*
import funspec.AnyFunSpec

class AlignSpec extends AnyFunSpec with Matchers:
  describe("Align Spec, is a zip that can zip elements") {
    it("""has a zip that returns Ior. Reminder, Ior represents
         |  Left, Right, or Both""".stripMargin) {
      val result = Align[List].align(List(1, 2, 3), List('a', 'b', 'c'))
      result should be(List(Ior.Both(1, 'a'), Ior.Both(2, 'b'), Ior.Both(3, 'c')))
    }
    it("""has a zip that will return Right if it is extending on the right""".stripMargin) {
      val result = Align[List].align(List(1, 2, 3), List('a', 'b', 'c', 'd'))
      result should be(List(Ior.Both(1, 'a'), Ior.Both(2, 'b'), Ior.Both(3, 'c'), Ior.Right('d')))
    }
    it("""has a zip that will return Left if it is extending on the left""".stripMargin) {
      val result = Align[List].align(List(1, 2, 3, 4), List('a', 'b', 'c'))
      result should be(List(Ior.Both(1, 'a'), Ior.Both(2, 'b'), Ior.Both(3, 'c'), Ior.Left(4)))
    }
    it("""has a """) {
      
    }
  }
