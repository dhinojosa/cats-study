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
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
 * OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

package com.xyzcorp.typeclasses

import cats._
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

import scala.util.{Failure, Success, Try}

class AlternativeSpec extends FunSpec with Matchers {
  describe("""Alternative extends Applicative and MonoidK""".stripMargin) {
    it("""has a method called unite, which folds over the inner structure
         |  to combine all of the values with our combine method inherited
         |  from MonoidK. The result is for us to accumulate all of the
         |  "interesting" values of the inner G, so if G is Option,
         |  we collect all the Some values, if G is Either,
         |  we collect all the Right values, etc.""".stripMargin) {
      val ofs: List[Option[String]] = List(Some("Hello"), None, Some("Whoa"), Some("Cool"))
      val maybeString: List[String] = Alternative[List].unite(ofs)
      maybeString should be(List("Hello", "Whoa", "Cool"))
    }
    it("""can use unite for a variety of cases,
         |  here we will use Option and Try""".stripMargin) {
      val maybeInt = Alternative[Option].unite[Try, Int](Some(Success(40)))
      maybeInt should be(Some(40))
    }
    it("""has a method, separate, which will separate left
         |  and rights, hence the term for alternate""".stripMargin) {
      val result: (Option[String], Option[Int]) = Alternative[Option].separate(3.asRight[String].some)
      result should be(None -> Some(3))
    }
    it("""has a method, separate, which will separate left
         |  and rights, hence the term for alternate, here we will
         |  use List instead of Option""".stripMargin) {
      val result: (List[String], List[Int]) = Alternative[List].separate(List(3.asRight[String]))
      result should be(Nil -> List(3))
    }
    it("""has a method, separate, which will separate left
         |  and rights, hence the term for alternate, here we will
         |  use List instead of Option but with more elements""".stripMargin) {
      val xs: List[Either[String, Int]] =
        List(Left("No"), Left("No"), Right(3), Right(100), Left("No"), Right(12))
      val result: (List[String], List[Int]) =
        Alternative[List].separate(xs)
      result should be(List("No", "No", "No"), List(3, 100, 12))
    }
    it("""has a method, separateFoldable, which is the same as separate
         |  but more efficient for inner items that are Foldables""".stripMargin) {
      val xs: List[Either[String, Int]] =
        List(Left("No"), Left("No"), Right(3), Right(100), Left("No"), Right(12))
      val result: (List[String], List[Int]) =
        Alternative[List].separateFoldable(xs)
      result should be(List("No", "No", "No"), List(3, 100, 12))
    }
    it("""has a guard method, used to return unit if true,
         |  here we are using it with an Option""".stripMargin) {
      def process(i: Int): Option[Unit] = Alternative[Option].guard(i > 4)
      process(2) should be(None)
      process(10) should be(Some(()))
    }
    it("""has a guard method, used to return unit if true,
         |  here we are using it with an List""".stripMargin) {
      def process(i: Int): List[Unit] = Alternative[List].guard(i > 4)
      process(2) should be(Nil)
      process(10) should be(List(()))
    }
  }
  describe("""Extensions""".stripMargin) {
    it("""has unite as an extension""") {
      val ofs: List[Option[String]] = List(Some("Hello"), None, Some("Whoa"), Some("Cool"))
      val result: List[String] = ofs.unite
      result should be(List("Hello", "Whoa", "Cool"))
    }
    it("""has separate as an extension""") {
      val result: (List[String], List[Int]) = List(3.asRight[String]).separate
      result should be(Nil -> List(3))
    }
  }
}
