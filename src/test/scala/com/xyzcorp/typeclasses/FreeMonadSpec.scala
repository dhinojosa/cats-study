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
import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.free.Free
class FreeMonadSpec extends AnyFunSpec with Matchers:

  /**
   * A free monad is a construction which allows you to build a monad from any Functor. Like other monads, it is a pure
   * way to represent and manipulate computations.
   *
   * In particular, free monads provide a practical way to:
   *
   * represent stateful computations as data, and run them run recursive computations in a stack-safe way build an
   * embedded DSL (domain-specific language) retarget a computation to another interpreter using natural transformations
   *
   * You will need to add the cats-free module
   */

  describe("It can create a monad from a functor, that is the underlying principle") {
    it("""has factory called Free[S[_], A] and the interesting thing is that it does not
         | have a constraint for it to be a functor""".stripMargin) {
      case class Box[A](a: A)

      def compiler: Box ~> Id = new (Box ~> Id):
        def apply[A](p: Box[A]): Id[A] =
          Id(p.a)

      val free: Free[Box, Int] = Free.pure(30)
      val free2: Free[Box, Int] = Free.pure(40)
      val resultFree: Free[Box, Int] = free.flatMap(i => free2.map(j => i + j))
      val result = resultFree.foldMap(compiler)
      result should be(70)
    }

    it("""has factory called Free[S[_], A] but this time I want
         |  to use it with an actual functor""".stripMargin) {
      case class Box[A](value: A)

//      given Functor[Box] = new Functor[Box]:
//        def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.value))

      def compiler: Box ~> Id = new (Box ~> Id):
        def apply[A](p: Box[A]): Id[A] =
          Id(p.value)

      val free: Free[Box, Int] = Free.liftF(Box(30))
      val free2: Free[Box, Int] = Free.liftF(Box(40))
      val resultFree: Free[Box, Int] = free
        .flatMap(i => free2.map(j => i + j))
      val result = resultFree.foldMap(compiler)
      result should be(70)
    }
  }

  //  case class Student(id: Long, firstName: String, lastName: String)
  //
  //  trait StudentService[A]
  //  case class Register(firstName: String, lastName: String) extends StudentService[Long]
  //  case class FindById(id: Long) extends StudentService[Student]
  //
  //  type StudentServiceFree[A] = Free[StudentService, A]
  //  import cats.free.Free.liftF
  //
  //  def register(firstName: String, lastName: String): StudentServiceFree[Long] = {
  //    liftF[StudentService, Long](Register(firstName, lastName))
  //  }
  //
  //  def findById(longId:Long): StudentServiceFree[Student] = {
  //    liftF[StudentService, Student](FindById(longId))
  //  }
