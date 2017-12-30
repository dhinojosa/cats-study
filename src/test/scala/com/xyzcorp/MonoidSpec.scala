/*
 * Copyright 2017 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp

import cats._
import cats.implicits._
import org.scalatest.{FunSuite, Matchers}

import scala.language.reflectiveCalls

class MonoidSpec extends FunSuite with Matchers {
  test("Case 1: Testing a String monoid") {
    val r = Monoid.apply[String].combine("Few", "Between")
    val s = Monoid[String].combine("Few", "Between")
    r should be("FewBetween")
    r should be(s)
  }

  case class Basket[+A <: {def cost : Float}](items: A*) {
    def count: Int = items.map(x => x.cost * 10).length
  }

  case class Egg(size: String, cost: Float)

  type CostType = AnyRef {def cost: Float}

  implicit val basketMonoid: Monoid[Basket[CostType]] =
    new Monoid[Basket[CostType]] { //At compile time, this has to resolve
      override def empty: Basket[CostType] = Basket[CostType]()

      override def combine
      (x: Basket[CostType], y: Basket[CostType]): Basket[CostType] =
        Basket(x.items ++ y.items: _*)
    }

  test("Case 2: Using a custom monoid") {
    val basketA = Basket(Egg("AA", .02f), Egg("A", .10f),
      Egg("A", .05f), Egg("AA", .04f))
    val basketB = Basket(Egg("AAA", .01f), Egg("AA", .20f))
    val combined = Monoid[Basket[CostType]].combine(basketA, basketB)
    combined.count should be(6)
  }
}
