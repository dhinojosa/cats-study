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

import cats.data.EitherT
import org.scalatest.{FunSpec, Matchers}

class EitherTSpec extends FunSpec with Matchers {
  describe("""EitherT is a monad transformer for `Either`, allowing the
             | effect of an arbitrary type constructor `F` to be combined with the
             | fail-fast effect of `Either`. `EitherT[F, A, B]` wraps a value
             | of type `F[Either[A, B]]`""".stripMargin) {

    it("can be combined ")
    it("An `F[C]` can be lifted in to `EitherT[F, A, C]` via `EitherT.right`") {
      pending
    }

    it("An `F[C]` can be lifted in to `EitherT[F, C, B]` via `EitherT.left`") {
      pending
    }
  }
}
