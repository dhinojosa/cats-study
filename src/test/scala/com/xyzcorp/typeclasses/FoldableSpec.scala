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
import org.scalatest._
import matchers.should._
import funspec.AnyFunSpec

class FoldableSpec extends AnyFunSpec with Matchers {

  describe("Foldable abstracts the familiar foldLeft and foldRight operations") {
    it("""folds elements like fold in the standard library,
         |  but uses a type class for it.
         |  Here is a fold with a list. It also uses a Monoid
         |  to combine the elements""".stripMargin) {
      val result = Foldable[List].fold(List("a", "b", "c"))
      result should be("abc")
    }

    it("""folds elements like fold in the standard library,
         |  but uses a type class for it.
         |  Here is a fold with a list. It also uses a Monoid
         |  to combine the elements, here it uses the Monoid[Int]
         |  which is of course is addition""".stripMargin) {
      val result = Foldable[List].fold(List(1, 2, 3))
      result should be(6)
    }

    it("""can be used to fold left using a show method (not related to the
         |  show method to
         |  print out what happens in a fold""".stripMargin) {
      def show[A](list: List[A]): String =
        list.foldLeft("nil")((accum, item) => s"($item then $accum)")

      show(List(1, 2, 3, 4)) should
        be("(4 then (3 then (2 then (1 then nil))))")
    }

    it("""can be used to fold right using a show method (not related to the
         |  show method to
         |  print out what happens in a fold""".stripMargin) {

      def show[A](list: List[A]): String =
        list.foldRight("nil")((item, accum) => s"($item then $accum)")

      show(List(1, 2, 3, 4)) should
        be("(1 then (2 then (3 then (4 then nil))))")
    }

    it("""can be used with a list as an accumulator""".stripMargin) {
      val result = Foldable[List]
        .foldLeft(List(1, 2, 3, 4), List.empty[Int])((xs, x) => x :: xs)
      result should be(List(4, 3, 2, 1))
    }

    it("""can be used to implement a map using a fold right""") {
      Foldable[List].foldRight(List(1, 2, 3, 4), Later.apply(List.empty[Int]))((next, evalB) =>
        evalB.map(xs => next :: xs)
      )
    }
  }

  describe("Foldable Composition") {
    it("""can be composed with other Foldables for deep traverses""") {
      val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
      val result: Int = Foldable[List]
        .compose(Foldable[Vector])
        .combineAll(ints)
      result should be(21)
    }
  }

  describe("Implicit wrappers for Foldable") {
    it("""can use an implicit wrapper to include all the features
         |  of a Foldable around the collection""".stripMargin) {
      import cats.syntax.foldable._
      val result: Int = List(1, 2, 3, 4).combineAll
      result should be(10)
    }
    it("""will use the native method on std library and
         |  not use cats' version to avoid any conflict""".stripMargin) { //useless import
      val result = List(1, 2, 3, 4, 5).foldRight(1)(_ + _)
      result should be(16)
    }
    it("""will not use the native standard library call in a case like
         | the following where we are requiring a call that demands that
         | a Foldable around a container is absolutely required""".stripMargin) {
      def mySum[F[_]](values: F[Int])(implicit foldable: Foldable[F], monoid: Monoid[Int]): Int =
        foldable.foldLeft(values, 0)(monoid.combine)
      mySum(List(10, 12, 19)) should be(41)
    }
  }

  describe("foldMap") {
    it("""will apply a map to all the elements
         |  before folding them""".stripMargin) {
      val result = Foldable[List].foldMap(List(1, 2, 3, 4))(i => 2 * i)
      result should be(20)
    }
  }

  describe("maximumOption") {
    it("""will also calculate the maximum option of a foldable,
         |  as long as there is a number""".stripMargin) {
      info("order for ints are already established")
      Foldable[List].maximumOption(List(1, 2, 3, 4, 5)) should be(Some(5))
    }
  }

  describe("collectXXX will apply a Partial Function and do a varying task") {
    it("""can collect the first element and apply a function""".stripMargin) {
      val result: Option[Int] = Foldable[List]
        .collectFirst(List(1, 2, 3, 4)) { case x if x % 2 == 0 => x * 3 }
      result should be(Some(6))
    }

    it("""can perform a fold and collect, given the pure monoid of the type""".stripMargin) {
      val result = Foldable[List]
        .collectFold(List(1, 2, 3, 4)) { case x if x % 2 == 0 => x }
      result should be(6)
    }

    it("""can collect the first element and apply a function just
         |  like the first example but with applying an
         |  a => Option(b) function""".stripMargin) {
      val result = Foldable[List]
        .collectFirstSome(List(1, 2, 3, 4)) { a => if (a % 2 == 0) Some(a * 2) else None }
      result should be(Some(4))
    }
    it("""can collect on the first element extracting from some context,
         |  here with Id""".stripMargin) {
      val result: Id[Option[Int]] =
        Foldable[List].collectFirstSomeM[Id, Int, Int](List(1, 2, 3, 4))(a => if (a % 2 == 0)(Some(a * 2)) else None)
      result should be(4.some)
    }
    it("""can collect on the first element extracting from some context,
         |  here with Either. This time either with a left is
         |  triggered with an option""".stripMargin) {

      type MyEither[A] = Either[String, A]

      def processValue(a: Int) =
        if (a % 2 == 0)
          Right(Option(a * 10))
        else
          Left("Unable to do so")

      val result: MyEither[Option[Int]] =
        Foldable[List].collectFirstSomeM[MyEither, Int, Int](List(1, 2, 3, 4))(processValue)
      result should be(Left("Unable to do so"))
    }

      it("""can collect on the first element extracting from some context,
           |  here with Either. This time either with a right
           |  is triggered with option""".stripMargin) {

          type MyEither[A] = Either[String, A]

          def processValue(a: Int) =
              if (a % 2 == 0)
                  Right(Option(a * 10))
              else
                  Left("Unable to do so")

          val result: MyEither[Option[Int]] =
              Foldable[List].collectFirstSomeM[MyEither, Int, Int](List(2, 3, 4, 5))(processValue)
          result should be(Right(20.some))
      }
  }
}
