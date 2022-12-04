/*
 * Copyright 2021 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import cats.*
import cats.free.FreeApplicative
import cats.free.FreeApplicative.FA
import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class FreeApplicativeSpec extends AnyFunSpec with Matchers:
  describe("FreeApplicative can create an applicative for free") {
    it("""Lets give it a try and see the signature we can use""".stripMargin) {

      case class Box[A](value: A)
      val fa1 = FreeApplicative.lift(Box("Hello"))
      val fa2 = FreeApplicative.lift(Box((s: String) => s.length))
      val result = fa1.ap(fa2)
      val finalResult: Option[Int] = result.foldMap[Option](
        new (Box ~> Option):
          def apply[A](fa: Box[A]): Option[A] = Option(fa.value)
      )
      finalResult should be(Some(5))
    }
  }
