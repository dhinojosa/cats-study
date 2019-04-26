/*
 * Copyright 2018 Daniel Hinojosa
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
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
 * IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

package com.xyzcorp

import cats._
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ListBuffer

class EvalSpec extends FunSpec with Matchers {
  describe("Eval") {
    it(
      """has a now, which is eager and memoized, similar to val in
        |  core Scala""".stripMargin) {

      val listBuffer = ListBuffer[String]()

      listBuffer += "Start"

      val eval: Eval[Int] = Eval.now {
        listBuffer += "Evaluating"
        4
      }

      listBuffer += "Showing"
      eval.value should be(4)
      eval.value should be(4)
      listBuffer.mkString(",") should be("Start,Evaluating,Showing")
    }

    it(
      """has a later, which is lazy and memoized, similar to lazy val in
        |  core Scala""".stripMargin) {

      val listBuffer = ListBuffer[String]()

      listBuffer += "Start"
      val eval: Eval[Int] = Eval.later {
        listBuffer += "Evaluating"
        4
      }
      listBuffer += "Showing"
      eval.value should be(4)
      eval.value should be(4)
      listBuffer.mkString(",") should be("Start,Showing,Evaluating")
    }

    it(
      """has an always, which is lazy and not memoized, similar to def in
        |  core Scala""".stripMargin) {

      val listBuffer = ListBuffer[String]()

      listBuffer += "Start"
      val eval: Eval[Int] = Eval.always {
        listBuffer += "Evaluating"
        4
      }

      listBuffer += "Showing"
      eval.value should be(4)
      eval.value should be(4)
      listBuffer.mkString(",") should be("Start,Showing,Evaluating,Evaluating")
    }

    it(
      """can be then memoized permanently after a non-memoized operation
        |  with memoize""".stripMargin) {
      val listBuffer = ListBuffer[String]()
      listBuffer += "Start"
      val memoize = Eval.always {
        listBuffer += "Evaluating";
        4
      }
        .map { x => listBuffer += "Map1"; x }
        .memoize
        .map { x => listBuffer += "Map2"; x }

      listBuffer += "Showing"
      memoize.value should be(4)
      memoize.value should be(4)
      listBuffer.mkString(",") should be
      "Start,Showing,Evaluating,Map1,Map2,Map2"
    }

    it(
      """has a defer method which defers evaluation, and instead of
        |  adding to the stack space,
        |  it creates a function on the heap space and connect
        |  when evaluated""".stripMargin) {
      def factorial(n: BigInt): Eval[BigInt] =
        if (n == 1) {
          Eval.now(n)
        } else {
          Eval.defer(factorial(n - 1).map(_ * n))
        }

      factorial(5).value should be(120)
      factorial(25).value should be(BigInt("15511210043330985984000000"))
    }

    it("can be used in a fold as well") {
      def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
        as match {
          case head :: tail =>
            Eval.defer(foldRight(tail, acc)(fn)).map(n => fn(head, n))
          case Nil =>
            Eval.now(acc)
        }

      foldRight(List(1,2,3,4), 0)(_+_).value should be (10)
    }
  }
}
