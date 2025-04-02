/*
 * Copyright 2025 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.*
import cats.implicits.*
import cats.syntax.all.*

import scala.collection.immutable

class BiFoldableSpec extends AnyFunSpec with Matchers:
  describe("""BiFoldable instances identify data structures with two
             |  independent Foldable that fold to the same summary value.""".stripMargin) {
    it("has the following signature") {
      trait MyBiFoldable[F[_, _]]:
        def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C
        def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C],
                                                           g: (B, Eval[C]) => Eval[C]
        ): Eval[C]
    }
  }
  describe("The methods") {
    it("has bifoldLeft which collapses the structure in a left associative way") {
      val fab: (List[Int], Int) = (List(1), 2)
      val result = Bifoldable[Tuple2].bifoldLeft(fab, Option(0))((c, a) => c.map(_ + a.head), (c, b) => c.map(_ + b))
      result should be(Some(3))
    }
    it("has bifoldLeft which collapses the structure in a left associative way, here we use a map") {
      given Bifoldable[Map] = new Bifoldable[Map]:
         override def bifoldLeft[A, B, C](fab: Map[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C = {
             fab.foldLeft(c) { case (acc, (k, v)) =>
                 val afterKey = f(acc, k)
                 g(afterKey, v)
             }
         }
         override def bifoldRight[A, B, C](fab: Map[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] = {
             fab.foldRight(c) { case ((k, v), acc) =>
                 f(k, g(v, acc))
             }
         }

      val fab: Map[String, Int] = Map("Algeria" -> 210, "USA" -> 100, "France" -> 201)
      val result = Bifoldable[Map].bifoldLeft(fab, Option(0))((c, a) => c.map(_ + a.length), (c, b) => c.map(i => i + b))
      result should be(Some(527))
    }
  }
  
  describe("The implementors") {
    it("has the following default") {
      pending
    }
  }
