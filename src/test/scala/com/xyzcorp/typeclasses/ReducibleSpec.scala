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
    it("has a split") {
      val reducible = Reducible[NonEmptyList]
      val nonEmptyTraverse = NonEmptyTraverse[NonEmptyList]
      val nonEmptyList = NonEmptyList(10, List(20, 30))
      val listZipped: NonEmptyList[(Int, Int)] = nonEmptyTraverse.zipWithIndex(nonEmptyList)
      val result = reducible.nonEmptyPartition(listZipped) { case (item, index) =>
        if index == 2 then Left(item) else Right(item)
      }
      result should be(Both(NonEmptyList(30, List.empty[Int]), NonEmptyList(10, 20)))
    }
  }
