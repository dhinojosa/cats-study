/*
 * Copyright 2020 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.decorators
import cats._
import cats.data._
import cats.data.Validated.Valid
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

class OptionSpec extends FunSpec with Matchers {
  it("has specialized methods that make it easy to make options") {
    val a = 3.some
    val b = none[Int]

    val result = for {
      i <- a
      j <- b
    } yield i + j
    result should be(none[Int])
  }

  describe("""converters""") {
    it("""has toValid""") {
      "Hello".some.toValid(new Throwable("Invalid")) should be(Valid("Hello"))
    }
    it("""has lift to fill in with other types""") {
      val result = "Fun".some.liftTo[Either[String, *]]
      val value1 = result("Not Found")
      value1 should be(Right("Fun"))
    }
    it("""has a general to NEC, which is a Either[NonEmptyChain[E], A]""") {
      val z = "Complete".some.toRightNec(-1)
    }
  }
}
