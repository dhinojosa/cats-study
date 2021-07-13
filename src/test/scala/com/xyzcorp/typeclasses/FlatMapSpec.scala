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

class FlatMapSpec extends AnyFunSpec with Matchers:
  import cats.*
  import cats.implicits.*
  describe("""FlatMapSpec which allows us to have a value in a context
             |  (F[A]) and then feed that into a function that takes a normal
             |  value and returns a value in a context (A => F[B]).
             |  One motivation for separating this out from Monad is that there
             |  are situations where we can implement flatMap but not pure.
             |  For example, we can implement map or flatMap that transforms
             |  the values of Map[K, *], but we can't implement pure (because
             |  we wouldn't know what key to use when
             |  instantiating the new Map).""".stripMargin) {
    it("""has of course flatMap for a variety of containers""") {
      val result = FlatMap[List]
        .flatMap(List(1, 2, 3, 4))(i => List(-i, i * 10))
      result should be(List(-1, 10, -2, 20, -3, 30, -4, 40))
    }

    it("""has ifM which is if statement that runs within the monad""") {
      val result = FlatMap[List]
        .ifM(List(true, false))(List(3, 10), List(2, 20))
      result should be(List(3, 10, 2, 20))
    }

    it("""has product which combines the results in a tuple""") {
      val maybeTuple = FlatMap[Option].product(Some(3), Some(10))
      maybeTuple should be(Some(3 -> 10))
    }

    it("""has tailRecM which will perform tail recursion
         |  from within a container, until it finds a Right""".stripMargin) {
      val result: Seq[Int] = FlatMap[List].tailRecM(10) { i =>
        println(">>>" + i)
        if i < 0 then List(Right(i))
        else List(Left(i - 1))
      }
      result should be(List(-1))
    }
  }
