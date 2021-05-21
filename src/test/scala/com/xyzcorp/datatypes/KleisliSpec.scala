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

import cats._
import cats.data._
import cats.implicits._
import org.scalatest._
import matchers.should._
import funspec.AnyFunSpec

import scala.util.{Failure, Success, Try}

class KleisliSpec extends AnyFunSpec with Matchers {
  describe("Kleisli Data Type") {

    it("is a wrapper around the function A => F[B]") {
      final case class MyKleisli[F[_], A, B](run: A => F[B])
    }

    /*
     Kleisli enables composition of functions that return a monadic value, for
     instance an Option[Int] or a Either[String, List[Double]] ,
     without having functions take an Option or Either as a parameter,
     which can be strange and unwieldy. ... These
     situations are where Kleisli is immensely helpful.
     */

    it(
      """is meant for composition, take the following functions that can compose,
        |  this is the standard Scala function composition with `andThen`""".stripMargin
    ) {

      val takeFirst: ((String, Int)) => String = t => t._1
      val substring3: String => Try[String] = s => Try(s.substring(3))
      val tryToSuccessOrEmpty: Try[String] => String = ts => ts.getOrElse("")
      val stringCaps: String => String = _.toUpperCase

      val f = takeFirst.andThen(substring3).andThen(tryToSuccessOrEmpty).andThen(stringCaps)
      f("Consider", 90) should be("SIDER")
    }

    it("""can be used to wrap various elements to one type
         |  called Kleisli so as to
         |  efficiently manipulate it""".stripMargin) {
      val kleisliTakeFirst: Kleisli[List, (String, Int), String] =
        Kleisli(t => List(t._1))
      val kleisliSubstring3: Kleisli[List, String, Try[String]] =
        Kleisli(s => List(Try(s.substring(3))))
      val kleisliTryToSucessOrEmpty: Kleisli[List, Try[String], String] =
        Kleisli(ts => List(ts.getOrElse("")))
      val kleisliStringCaps: Kleisli[List, String, String] =
        Kleisli(s => List(s.toUpperCase))

      val composition = kleisliTakeFirst >>> kleisliSubstring3 >>>
        kleisliTryToSucessOrEmpty >>> kleisliStringCaps
      val result = composition.run("Consider" -> 90)
      result should be(List("SIDER"))
    }

    it("""is used to handle error in a consistent way, in this example,
         |  taking a substring, and applying a map.""".stripMargin) {
      val k1: Kleisli[Try, String, String] =
        Kleisli(s => Try(s.substring(0, 3)))
      val m = k1.map(s => s + "!")
      m("Hello") should be(Success("Hel!"))
    }

    it("""is used to handle error in a consistent way, in this example,
         |  taking a substring, and applying a map
         |  in a for-comprehension.""".stripMargin) {
      val k1: Kleisli[Try, String, String] =
        Kleisli(s => Try(s.substring(0, 3)))
      val m = for (i <- k1) yield i + "!"
      m("Hello") should be(Success("Hel!"))
    }

    it("""is used to handle error in a consistent way, in this example,
         |  taking a substring, and applying a flatMap, this will
         |  result in a Failure.""".stripMargin) {
      val k1: Kleisli[Try, String, String] =
        Kleisli(s => Try(s.substring(0, 3)))
      val k2: Kleisli[Try, String, Int] = Kleisli(s => Try(s.toInt))

      val m: Kleisli[Try, String, (String, Int)] =
        k1.flatMap(s => k2.map(t => (s, t)))
      m("Hello") shouldBe a[Failure[_]]
    }

    it("""is used to handle error in a consistent way, in this example,
         |  taking a substring, and applying a flatMap, this will
         |  result in a Success of a tuple-2.""".stripMargin) {
      val k1: Kleisli[Try, String, String] =
        Kleisli(s => Try(s.substring(0, 3)))
      val k2: Kleisli[Try, String, Int] = Kleisli(s => Try(s.toInt))
      val m: Kleisli[Try, String, (String, Int)] =
        k1.flatMap(s => k2.map(t => (s, t)))
      m("400") should be(Success(("400", 400)))
    }

    it(
      """is used to handle error in a consistent way, in this example,
        |  taking a substring, and applying a flatMap, this will
        |  result in a Success of a tuple-2 using a for comprehension.""".stripMargin
    ) {
      val k1: Kleisli[Try, String, String] =
        Kleisli(s => Try(s.substring(0, 3)))
      val k2: Kleisli[Try, String, Int] = Kleisli(s => Try(s.toInt))
      val m: Kleisli[Try, String, (String, Int)] =
        for {
          s <- k1
          t <- k2
        } yield (s, t)
      m("400") should be(Success(("400", 400)))
    }

    it("""is used to handle error in a consistent way, in this example,
         |  taking a substring, and applying a flatMap, this will
         |  result in a Success.""".stripMargin) {
      val k1: Kleisli[Try, String, String] =
        Kleisli(s => Try(s.substring(0, 3)))
      val k2: Kleisli[Try, String, Int] = Kleisli(s => Try(s.toInt))
      val m: Kleisli[Try, String, (String, Int)] =
        k1.flatMap(s => k2.map(t => (s, t)))
      m("400") should be(Success(("400", 400)))
    }

    it("""can contain an ap, like Applicative, weird""") {
      val kleisliTakeFirst: Kleisli[List, (String, Int), Int => String] =
        Kleisli(t => List((x: Int) => s"$x${t._1}"))
      val f: Kleisli[List, (String, Int), Int] = Kleisli(_ => List(40))
      val result: Kleisli[List, (String, Int), String] = kleisliTakeFirst.ap(f)
      println(result.apply("Hello" -> 34))
    }
  }
}
