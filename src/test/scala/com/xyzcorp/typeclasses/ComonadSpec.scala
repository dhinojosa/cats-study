/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import cats.*
import cats.data.NonEmptyList
import org.scalatest.*
import matchers.should.*
import funspec.AnyFunSpec

class ComonadSpec extends AnyFunSpec with Matchers:
  describe("""A Comonad in Haskell has three properties,
             |  extract, duplicate, and extend:
             |  extract :: w a -> a
             |  duplicate :: w a -> w (w a)
             |  extend :: (w a -> b) -> w a -> w b""".stripMargin) {

    it("""has an extract that for a list will return the
         |  first element in a non-empty list""".stripMargin) {

      val nel = NonEmptyList.of(3, 1, 10, 40)
      val result = Comonad[NonEmptyList].extract(nel)
      result should be(3)
    }
  }
