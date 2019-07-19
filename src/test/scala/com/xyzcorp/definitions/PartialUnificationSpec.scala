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
  describe("Partial Unification") {
    it(
      """is supposed to work by default in Scala, note the following
        |  where we have the following object with a method bar
        |  that uses a higher kinded type M and type A, it works great
        |  with List and Set as we discussed in
        |  HigherKindedTypesSpec""".stripMargin) {

      import scala.language.higherKinds

      object Foo {
        def bar[M[_], A](x: M[A]): M[A] = x
      }

      Foo.bar(List(1, 2, 3, 4)) should be(List(1, 2, 3, 4))
    }

    it(
      """will work if we create a type with a right projection
        |  for things that have two parameterized types like a
        |  function and either types""".stripMargin) {

      import scala.language.higherKinds

      object Foo {
        def bar[M[_], A](x: M[A]): M[A] = x
      }

      type ExceptionEither[A] = Either[Exception, A]

      val e: ExceptionEither[String] = Either
        .cond(10 > 40, "Great", new Exception("Sad"))

      Foo.bar(e) should equal(e)
    }
    it(
      """is an issue with regular scala is the inability
        |  to partially applied parameterized types of two
        |  when inlined and this is stated here
        |  at https://github.com/scala/bug/issues/2712.
      """.stripMargin) {

      import scala.language.higherKinds

      object Foo {
        def bar[M[_], A](x: M[A]): M[A] = x
      }

      info(
        """Note the return function, it was able to resolve
          |  to Int by partially
          |  applying the function, if we didn't turn on
          |  -Ypartial-unification in sbt that would
          |   not work""".stripMargin)

      val fun: Function1[Int, Int] = Foo.bar((x: Int) => x + 4)
    }
  }
}
