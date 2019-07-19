/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import cats._
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

class ApplySpec extends FunSpec with Matchers {
  describe("Apply") {
    it(
      """extends Functor, with ap. Ap transforms a value in a
        |  context.  Except that instead of providing a A => B, you provide
        |  and F[A=>B] where F is the Apply or context""".stripMargin) {

      val optionFunction: Option[Int => Int] = Some(x => x + 1)
      val maybeInt = Apply[Option].ap(optionFunction)(Some(4))
      maybeInt should be(Some(5))
    }

    it(
      """should also work with failure, here the function,
        |  will be resolved to None, while the Option with a value is a Some""".stripMargin)
    {
      val optionFunction: Option[Int => Int] = None
      val maybeInt = Apply[Option].ap(optionFunction)(Some(4))
      maybeInt should be(None)
    }

    it(
      """should also work with failure, here the function,
        |  will be resolved to Some function while the Option with a value is a
        |  None""".stripMargin) {
      val optionFunction: Option[Int => Int] = Some(x => x + 1)
      val maybeInt = Apply[Option].ap(optionFunction)(None)
      maybeInt should be(None)
    }
  }
}
