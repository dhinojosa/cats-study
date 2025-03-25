/*
 * Copyright 2025 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes

import cats.data.Ior
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.data.IorT
import cats.implicits.*
import cats.Id

class IorTSpec extends AnyFunSpec with Matchers:
  describe("IorT") {
    it("has the following signature") {
      trait IorT[F[_], A, B]:
        def value: F[Ior[A, B]]
        def map[C](f: B => C): IorT[F, A, C]
        def flatMap[C](f: B => IorT[F, A, C]): IorT[F, A, C]
    }
    it("is used to represent computations that result in an inclusive-or state wrapped within a monadic context") {
      pending
    }
  }
