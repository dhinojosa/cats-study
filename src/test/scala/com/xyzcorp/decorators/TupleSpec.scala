/*
 * Copyright 2020 Daniel Hinojosa
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.decorators

import cats._
import cats.implicits._
import cats.data._

import org.scalatest.{FunSpec, Matchers}


class TupleSpec extends FunSpec with Matchers {
  describe("""Using cats.implicits._ utilities to give certain code more
             |  functionality.""".stripMargin) {

    it("""has >=, greater than or equal to from PartialOrder which
         |  is probably not
         |  something involved with tuples per say, but
         |  it still works""".stripMargin) {
      import cats.implicits._
      val tuple = (1, "Hello", 3.0)
      val result = tuple >= tuple
      result should be(true)
    }

    it("""has functional applications like map, which is a bit odd
         |  It seems to only to operate on the last tuple element.
         |  There is a functor implementation of Tuple2. This
         |  comes from catsStdInstancesForTuple2""".stripMargin) {
      import cats.implicits._
      val tuple = (1, "Hello")
      tuple.map(_ + "!") should be(1 -> "Hello!")
    }

    it("""has tuple left which creates a tuple on the left hand side,
         |  this comes from Functor""".stripMargin) {
      import cats.implicits._
      val widen = (1, "Hello").tupleLeft(90.0)
      widen should be((1, (90.0, "Hello")))
    }

    it("""has tupleRight, with changes the right element and leaves what it was
         |  previously on the left this comes from Functor. This comes from toFunctorOps and""".stripMargin) {
      import cats.implicits._
      val widen = (1, "Hello").tupleRight(90.0)
      widen should be((1, ("Hello", 90.0)))
    }

    it("""has flatMap, which takes the last element and returns another
         |  tuple. This comes from flatMapOps and
         |  catsStdCommutativeMonadForTuple2""".stripMargin) {
      import cats.implicits._
      val result = (3, "Hi").flatMap(str => (5, s"$str!"))
      result should be(8 -> "Hi!")
    }

    it("""has coflatMap, which takes the last element and returns another
         |  tuple. This comes from toCoflatMapOps""".stripMargin) {
      import cats.implicits._
      val result = (3, "Hi").coflatMap(t => (t._1 + 1, t._2 + "!"))
      result should be(3, (4, "Hi!"))
    }

    it(
        """has comparisons where it would perform a comparison by tuple.
          |  This comes from catsSyntaxPartialOrder""".stripMargin) {
      import cats.implicits._
      val result = (1, 6.0, "Sparkling") < (2, 4.0, "Zebra")
      result should be(true)
    }

    it("""combine uses Semigroup to join the elements
          |  This comes from catSyntaxSemigroup""") {
      import cats.implicits._
      val result: (Int, Double, String) = (1, 6.0, "Sparkling").combine(3, 3.0, "Wine")
      result should be(4, 9.0, "SparklingWine")
    }

    it(
        """combineN uses Semigroup to join the elements.
          |  This comes from catSyntaxSemigroup""".stripMargin) {
      import cats.implicits._
      val result: (Int, Double, String) = (1, 6.0, "Sparkling").combineN(3)
      result should be(3, 18.0, "SparklingSparklingSparkling")
    }

    it ("""Has much of the same collections, as list, like forall""".stripMargin) {
        val bool = ("Foo", "Bar").forall(_.length == 3)
        bool should be (true)
    }
  }
}
