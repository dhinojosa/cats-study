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

import java.net.URL

import cats.data.Kleisli
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

import scala.util.Try

class KleisliSpec extends FunSpec with Matchers {
  describe("Kleisli Data Type") {
    import scala.language.higherKinds
    it("is a wrapper around the function A => F[B]") {
      final case class MyKleisli[F[_], A, B](run: A => F[B])
    }

    it(
      """is meant for composition, take the following functions that can compose,
        | this is just using
      """.stripMargin)
    {
      val takeFirst: ((String, Int)) => String = t => t._1
      val substring3: String => Try[String] = s => Try(s.substring(3))
      val tryToSuccessOrEmpty: Try[String] => String = ts => ts.getOrElse("")
      val stringCaps: String => String = _.toUpperCase

      val f = takeFirst andThen substring3 andThen tryToSuccessOrEmpty andThen stringCaps
      f("Consider", 90) should be("SIDER")
    }

    it(
      "can be used to wrap various elements to one type called Kleisli so as to efficiently manipulate it")
    {
      val kleisli1: Kleisli[Option, Int, String] = Kleisli(
        (i: Int) => Option(i.toString))
      val kleisli2: Kleisli[Option, String, URL] = Kleisli(
        (s: String) => Option(new URL(s)))
      val result: Kleisli[Option, Int, URL] = kleisli1.andThen(kleisli2)

      val option = result.run(20)
      val value = option.getOrElse(new Object())

    }


    it(
      "can use >>> to do the compose")
    {
      val kleisli1: Kleisli[Option, Int, String] = Kleisli(
        (i: Int) => Option(i.toString))
      val kleisli2: Kleisli[Option, String, URL] = Kleisli(
        (s: String) => Option(new URL(s)))
      val result: Kleisli[Option, Int, URL] =    kleisli1 >>> kleisli2

      val option = result.run(20)
      val value = option.getOrElse(new Object())


      value should be("http://20")
    }
  }
}
