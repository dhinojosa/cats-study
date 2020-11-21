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

import scala.language.reflectiveCalls

class MonoidSpec extends FunSpec with Matchers {
  describe("""A Monoid is a type class that combines two things.
      | What do we naturally do when we combine two Strings together?
      | What do we do when we combine two Integers together?
    """.stripMargin) {

    it("has an operation `combine` with type (A, A) => A") {
      val r = Monoid.apply[String].combine("Few", "Between")
      val s = Monoid[String].combine("Few", "Between")
      r should be("FewBetween")
      r should be(s)
    }

    it("""has a method `empty` with type A, What comes to your mind
        | when we say an empty String?
        | when we say an empty Int?
        | when we say an empty Float?
      """.stripMargin) {
      Monoid[String].empty should be("")
      Monoid[Float].empty should be(0F)
    }

    it("""What is a Monoid of an empty List of String?""") {
      Monoid[List[String]].empty should be(List.empty[String])
    }

    it("""What is a Monoid of an empty Set of String?""") {
      Monoid[Set[String]].empty should be(Set.empty[String])
    }

    it("""contains a |+| operator that is a synonym of combine from the
        |  Semigroup type class.""".stripMargin) {
      val total = 1 |+| 2 |+| Monoid[Int].empty
      total should be(3)
    }

    it("""can combine lists of strings""") {
      val monoidListString = Monoid[List[String]]
      val result = List("One", "Two") |+|
        List("Three", "Four") |+|
        monoidListString.empty
      result should be(List("One", "Two", "Three", "Four"))
    }

    it("""can combine anything including tuples""") {
        val a = (1, "Two")
        val b = (5.0, Symbol("Livid"))
    }
  }
}
