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

import cats.data._
import cats.implicits._
import org.scalatest._
import matchers.should._
import funspec.AnyFunSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class EitherTSpec extends AnyFunSpec with Matchers {
  describe("""EitherT is a monad transformer for `Either`, allowing the
             | effect of an arbitrary type constructor `F` to be combined with the
             | fail-fast effect of `Either`. `EitherT[F, A, B]` wraps a value
             | of type `F[Either[A, B]]`""".stripMargin) {

    import cats.implicits.catsStdInstancesForFuture
    it("can be created by calling an apply") {
      import scala.concurrent.ExecutionContext.Implicits.global
      val d1 = EitherT.fromOption[Future](Some(40), -1)
      val d2 = EitherT.fromOption[Future](Some(21), -1)
      val result = for {
        x <- d1;
        y <- d2
      } yield (x, y)
      Await.result(result.value, 10 seconds)
    }

    it("An `F[C]` can be lifted in to `EitherT[F, A, C]` via `EitherT.right`") {
      //type C = Int
      //type F[_] = List[_]
      //type A = String

      val xs = List(1, 2, 4, 5)
      import cats.implicits._
      val value1: EitherT[List, String, Int] = EitherT.right[String](xs)
      value1
    }
    it("An `F[C]` can be lifted in to `EitherT[F, C, B]` via `EitherT.left`") {
      val xs = List("This", "is", "an", "error")
      import cats.implicits._
      val value1: EitherT[List, String, Int] = EitherT.left[Int](xs)
      value1
    }
    it("""EitherT has rightT which is an alias for pure""") {
      val result: EitherT[Option, Int, Int] = EitherT.rightT[Option, Int](30)
      val result2: EitherT[Future, Int, Int] = EitherT.rightT[Future, Int](-3)
    }
  }
}
