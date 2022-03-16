/*
 * Copyright 2020 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*

class GroupSpec extends AnyFunSpec with Matchers:

  import cats.*
  import cats.implicits.*

  def operate[A: Group](x: A, y: A):A =
    x.inverse().combine(y.inverse())

  describe("Grouped is a Monoid where there is an inverse, which depends on the monoid instance") {
    it("contains remove which combines the first with the inverse of the second argument") {
      val result = Group[Int].remove(5, 2)
      result should be(3)
    }

    it("has a Int instance") {
      operate(4, 10) should be(-14)
    }

    it("has a Unit instance") {
      operate((), ()) should be(())
    }

    it("has Function 0 instance where it will inverse the result") {
      val fun0 = operate(() => 10, () => 20)
      fun0() should be(-30)
    }

    it("has Function 1 instance where it will inverse the result") {
      val fun1 = operate((x: Int) => x + 10, (y: Int) => y + 20)
      fun1(20) should be(-70)
    }
  }
