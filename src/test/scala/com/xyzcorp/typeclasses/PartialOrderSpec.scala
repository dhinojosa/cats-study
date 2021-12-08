/*
 * Copyright 2020 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import org.scalatest.*
import matchers.should.*
import funspec.AnyFunSpec

class PartialOrderSpec extends AnyFunSpec with Matchers:
  import cats.*
  import cats.implicits.*
  describe("PartialOrderSpec is a type class with comparison operators") {
    describe("Tuple implementation that comes from catsSyntaxPartialOrder") {
      it("contains less than") {
        (3, 1.0, "Hello") < (2, 2.0, "Scout")
      }
    }
    describe("Can be applied generically with a method") {
      def operate[A: PartialOrder](x: A, y: A) =
        x < y

      operate("Hello", "Indigo") should be(true)
      operate(Option(4), Option(10)) should be(true)
    }
  }
