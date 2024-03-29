/*
 * Copyright 2021 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.decorators

import cats.data.Validated
import cats.data.Validated.Invalid
import cats.*
import cats.data.*
import cats.implicits.*
import org.scalatest.*
import matchers.should.*
import funspec.AnyFunSpec

class NumberSpec extends AnyFunSpec with Matchers {
  describe("Other wrappers") {
    it("contains combine that uses monoid to compare") {
      val item = 12.combine(40)
      item should be(52)
    }
    it("contains an invalid to create an invalid") {
      val item:Validated[Int, String] = 12.invalid[String]
      item should be(Invalid(12))
    }
  }
}
