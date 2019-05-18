/*
 * Copyright 2018 Daniel Hinojosa
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
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.definitions

import org.scalatest.{FunSpec, Matchers}

class PartialUnificationSpec extends FunSpec with Matchers {
  describe("Partially Applied Parameterized Types") {
    it(
      """is an issue with regular scala is the inability
        |  to partially applied parameterized types.
        |  This is filed
        |  at https://github.com/scala/bug/issues/2712.
        |
        |  In the following a function cannot be mapped
        |  into Higher Kinded type although a Function
        |  can be Function[A,B] can be made into
        |  Function[B] given that A is known.""".stripMargin) {

      import scala.language.higherKinds

      object Foo {
        def bar[M[_], A](x: M[A]): M[A] = x
      }

      info(
        """Note the return function, it was able to resolve
          |  to Int by partially
          |  applying the function""".stripMargin)

      val fun: Function1[Int, Int] = Foo.bar((x: Int) => x + 4)
    }

    it(
      """must decide on a type and
        |  it was doing so incorrectly. Given the following
        |  functions, Scala was using partially applying arguments
        |  differently than what was expected""".stripMargin) {

      info("This is the correct type alias")
      type G[A] = Int => A

      info("This is now the correct type alias")
      type F[A] = A => Double
    }

    it(
      """is solved by partially applying from left to right,
        |  fulfilling the types on the left first. Here it
        |  is using a Functor with a definition that
        |  has a concrete first type but leaves the
        |  second one as generic""".stripMargin) {

      import cats.Functor

      type F[E] = Int => E

      implicit val intToUnknownFunctor: Functor[F] = new Functor[F] {
        override def map[A, B](fa: F[A])(f: A => B): F[B] = (x: Int) => f(fa(x))
      }

      val result: F[String] = Functor[F].fmap((x: Int) => "hello" * x)(_ + 8)
      result.apply(2) should be ("hellohello8")
    }
  }
}
