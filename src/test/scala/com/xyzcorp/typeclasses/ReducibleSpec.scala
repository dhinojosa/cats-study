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

import cats.*
import cats.data.Ior.Both
import cats.data.NonEmptyList
import cats.implicits.*
import org.scalatest.*
import matchers.should.*
import funspec.AnyFunSpec

class ReducibleSpec extends AnyFunSpec with Matchers:
  describe("Reducible extends Foldable to contain more operations") {
    it("has a partition") {
      val reducible = Reducible[NonEmptyList]
      val nonEmptyTraverse = NonEmptyTraverse[NonEmptyList]
      val nonEmptyList = NonEmptyList(10, List(20, 30))
      val listZipped: NonEmptyList[(Int, Int)] = nonEmptyTraverse.zipWithIndex(nonEmptyList)
      val result = reducible.nonEmptyPartition(listZipped) { case (item, index) =>
        Either.cond(index == 2, item, item)
      }
      result should be(Both(NonEmptyList.of(10, 20), NonEmptyList.one(30)))
    }
    it("can be used with tuple2, but doesn't seem to make sense") {
      type ShavedTuple[X] = Tuple2[Int, X]
      val result = Reducible[ShavedTuple].isEmpty(30 -> "")
      result should be(false)
    }
    it("has a reduceK, which requires a kind of SemigroupK, if used with a list, then it will use SemigroupK recipe") {
      val result = Reducible[NonEmptyList].reduceK[List, Int](NonEmptyList.of(List(1, 2, 3), List(4, 5, 6)))
      result should be(List(1, 2, 3, 4, 5, 6))
    }
  }
