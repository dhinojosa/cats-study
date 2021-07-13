/*
 * Copyright 2018 Daniel Hinojosa
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
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.definitions

import org.scalatest.*
import matchers.should.*
import funspec.AnyFunSpec

class ReferentialTransparencySpec extends AnyFunSpec with Matchers:
  describe("Referential Transparency") {
    it("""is an expression that can replaced by it's evaluated value
         |  without changing the behavior
         |  of the program""".stripMargin) {
      val result = (2 * 3) + 5 * (2 * 3)
      val result2 = 6 + 5 * 6
      result should equal(result2)
    }
    it("""is also a function or a method that does not
         |  invoke a state change or a
         |  side effect""".stripMargin) {
      def fun(x: Int) = x * 2
      val result = fun(12) + 24
      val result2 = 24 + 24
      result should equal(result2)
    }
    it("""but it is not a function or a method that
         |  does invoke a state change or a
         |  side effect""".stripMargin) {
      var y = 10

      def fun(x: Int) =
        y = y + x
        y

      val result = fun(2) + fun(2)
      val result2 = 12 + 12
      info(s"result is actually $result")
      result shouldNot equal(result2)
    }
  }
