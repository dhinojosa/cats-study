/*
 * Copyright 2020 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes

import cats._
import cats.data._
import cats.implicits._
import org.scalatest._
import matchers.should._
import funspec.AnyFunSpec

import scala.concurrent.Future

class WriterTSpec extends AnyFunSpec with Matchers {
  describe("""WriterT is a writer transformer so that it can be
             |  combined in a flatMap style""".stripMargin) {
    describe("Creation") {
      it("can be created with apply") {
        val w1: WriterT[Option, List[String], Int] = WriterT(Option(List.empty[String], 31))
        val w2: WriterT[Option, List[String], Int] = WriterT(Option(List("We have another item"), 31))
        val w3: WriterT[Option, List[String], Int] = w1.tell(List("We have no sense of logic"))
        println(w3)
      }
    }
  }
}
