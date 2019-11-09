/*
 * Copyright 2019 Daniel Hinojosa
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

import org.scalatest.{FunSpec, Matchers}

class TupleSpec extends FunSpec with Matchers {
    describe(
        """Using cats.implicits._ utilities to give certain code more
          |  functionality.""".stripMargin) {

        it(
            """has >= which is probably not
              |  something involved with tuples per say""".stripMargin) {
            import cats.implicits._
            val tuple = (1, "Hello", 3.0)
            val result = tuple >= tuple
            result should be(true)
        }

        it("""has another application""") {
            import cats._
            import cats.implicits._
            val tuple = (1, "Hello")
            tuple.map(_ + "!")
        }

        it("""has another application with +++""") {
            import cats._
            import cats.implicits._
            val widen = (1, "Hello").tupleLeft(90.0)
            println(widen)
        }

        it("""has another application with tupleRight""") {
            import cats._
            import cats.implicits._
            val widen = (1, "Hello").tupleRight(90.0)
            println(widen)
        }
    }
}
