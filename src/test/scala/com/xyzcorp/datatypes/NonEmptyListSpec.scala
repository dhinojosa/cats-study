/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the
 * Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to
 * do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice
 * shall be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
 * ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR
 * A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
 * EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
 * OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes

import cats.data.NonEmptyList
import org.scalatest.*
import matchers.should.*
import funspec.AnyFunSpec

class NonEmptyListSpec extends AnyFunSpec with Matchers:
  describe("""A NonEmptyList is a forever non-empty-list, because
             |  it is a non-empty-list there are some
             |  behaviors that is can do that a possible
             |  empty list cannot have.""".stripMargin) {
    it("""can be created using `of` which is given a head
         |  and a repeated parameter of a tail.
         |""".stripMargin) {
      val nel = NonEmptyList.of(30, 10, 20, 11)
      nel.size should be(4)
    }
    it("""can be created using `one` which of course only accepts
         |  one element.
         |""".stripMargin) {
      val nel = NonEmptyList.one(30)
      nel.size should be(1)
    }
    it("""can be created from any `Foldable` and returns an
         |  Option of the NonEmptyList if the `Foldable` is empty.
         |  See `FoldableSpec` for more information.
         |""".stripMargin) {
      import cats.implicits.*
      val optionNel = NonEmptyList.fromFoldable(List(30, 10, 20))
      optionNel match
        case Some(nel) => nel.size should be(3)
        case None      => fail("Unexpected Condition")
    }
    it("""can be created from any `List` and returns an
         |  Option of the NonEmptyList if the `List` is empty.
         |""".stripMargin) {
      val optionNel = NonEmptyList.fromList(List(30, 10, 20))
      optionNel match
        case Some(nel) => nel.size should be(3)
        case None      => fail("Unexpected Condition")
    }

    it("""can be created from any `List` using `fromListUnsafe`
         |  that can possibly throw an exception if the
         |  `List` is empty
         |  Option of the NonEmptyList if the `Foldable` is empty.
         |  See `FoldableSpec` for more information.
         |""".stripMargin) {
      val thrown = the[IllegalArgumentException] thrownBy
        NonEmptyList.fromListUnsafe(List.empty[String])
      thrown.getMessage should be("Cannot create NonEmptyList from empty list")
    }

    it("""can be created from any `List` using `fromReducible`
         |  and returns an
         |  Option of the NonEmptyList if the `Reducible` is empty.
         |  See `ReducibleSpec` for more information.
         |""".stripMargin) {
      import cats.*
      import cats.data.*
      val nel = NonEmptyList.fromReducible(NonEmptyChain(1, 2, 3, 4))
      nel.size should be(4)
    }

    it("""can be created with ofInitLast which places the
         |  last element in front if the list provided
         |  is empty.""".stripMargin) {
      import cats.data.*
      val nel = NonEmptyList.ofInitLast(Nil, 5)
      nel.last should be(5)
    }

    it("""has a companion object called ZipNonEmptyList which
         |  contains implicit rules to make it available for
         |  fmap, and ap as a CommutativeApply""".stripMargin) {
      import cats.*
      val nel = NonEmptyList.of(1, 3, 4, 10)
      val result = Functor[NonEmptyList].fmap(nel)(x => x + 1)
      result.head should be(2)
      result.size should be(4)
      result.last should be(11)
    }
    it("""is a forever non-empty-list, because
         |  it is a non-empty-list there are some
         |  behaviors that is can do that a possible
         |  empty list cannot have, like being a
         |  comonad, see ComonadSpec for details
         |""".stripMargin) {
      val nel = NonEmptyList.of(3, 1, 10, 40)
      val head = nel.coflatMap(nel => nel.head)
      head should be(NonEmptyList.of(3, 1, 10, 40))
    }
    it("""can perform a more effective reduce without requiring a seed
         |  since there
         |  is never a chance that there will be an empty
         |  list and we would never require a seed.""".stripMargin) {
      import cats.*
      //import cats.implicits._
      implicit val monoid: Monoid[Int] = new Monoid[Int]:
        override def empty: Int = 1

        override def combine(x: Int, y: Int): Int = x * y
      val reduction = NonEmptyList.of(3, 1, 4, 5, 10).reduce
      reduction should be(600)
    }
  }
