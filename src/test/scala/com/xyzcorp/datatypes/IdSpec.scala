/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes

import cats.*
import cats.implicits.*
import cats.data.*
import org.scalatest.*
import matchers.should.*
import funspec.AnyFunSpec

import scala.language.{postfixOps, reflectiveCalls}

class IdSpec extends AnyFunSpec with Matchers:
  describe("Id") {
    it("""wraps a primitive in a 'container' that can be used, by
         | the variety of type classes, and is actually
         | an alias for type Id[A] = A""".stripMargin) {

      val num: Id[Int] = 4
      Functor[Id].fmap(num)(x => x + 3) should be(7: Id[Int])
    }
    it("""is automatically promoted to an Id context when in a method""") {
      def add(x: Id[Int], y: Id[Int]): Id[Int] = x + y
      val result = add(10, 20)
      result should be(30)
    }
    it("""of course is used with flatMap which is the point of the Monad""") {
      
    }
    it("can be used to create a stack safe function") {
      def factorial(n: Int): Id[Int] =
        if n <= 1 then 1
        else n * factorial(n - 1)
      factorial(10) should be(3628800)
    }
  }
