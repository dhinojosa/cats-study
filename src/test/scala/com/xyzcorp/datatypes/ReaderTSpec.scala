/*
 * Copyright 2022 Daniel Hinojosa
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
 * OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes

import cats.*
import cats.arrow.FunctionK
import cats.data.ReaderT
import cats.implicits.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ReaderTSpec extends AnyFunSpec with Matchers:

  describe("Discovery of ReaderTSpec") {
    it("has the following signature") {
      val item1 = ReaderT.apply[Option, String, Int](s => Option(s.length))
      val item2:ReaderT[Option, String, String] = ReaderT
        .apply[Id, String, String](s => Id(s + "?"))
        .mapK(
          new FunctionK[Id, Option]():
            def apply[A](id: Id[A]): Option[A] = Option(id)
        )
      val item3 = ReaderT.apply[Option, String, String](s => Option(s + "!"))
      val result = for {
          i <- item1
          o <- item2
          s <- item3.map(m => m + o)
      } yield s.repeat(i)
      result.run.apply("Hello") should be("Hello!Hello?Hello!Hello?Hello!Hello?Hello!Hello?Hello!Hello?".some)
    }
  }

  describe("ReaderTSpec is a Reader Transformation") {
    it("can be brought in") {
      pending
    }
  }
  describe("ReaderTSpec is also this") {
    it("Also has this") {
      pending
    }
  }
