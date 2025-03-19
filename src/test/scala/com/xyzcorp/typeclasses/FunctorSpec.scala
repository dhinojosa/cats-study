/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import cats.*
import cats.data.Nested
import cats.implicits.*
import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.language.{postfixOps, reflectiveCalls}

class FunctorSpec extends AnyFunSpec with Matchers:
  case class Box[A](contents: A)

  trait MyFunctor[M[_]]:
    def myMap[A, B](m: M[A])(f: A => B): M[B]

  describe("Functor") {
    it("is a map that uses a type class") {
      val result = Functor[List].fmap(List(1, 2, 3, 4))(x => x + 30)
      result should be(List(31, 32, 33, 34))
    }

    it("can be used with an Option") {
      val result = Functor[Option].fmap(Option(13))(x => x * 10)
      result should be(Some(130))
    }

    it("can be used with an Option that is also a None") {
      val result = Functor[Option].fmap(Option.empty[Int])(x => x * 10)
      result should be(None)
    }

    it("can be used with a Function and used for composition") {
      import cats.instances.function.*
      import cats.syntax.functor.*

      val func1 = (a: Int) => a + 1
      val func2 = (a: Int) => a * 2
      val func3 = (a: Int) => s"$a!"
      val func4 = func1.map(func2).map(func3)

      func4(123)
    }
    it("can be used with Futures") {
      val future: Future[Int] = Future {
        40 * 10
      }
      val result = Functor[Future].fmap(future)(x => x / 2)
      result.foreach(_ should be(200))
      Await.ready(result, 3 seconds)
    }
    it("can be used with a type class of your choosing") {

      implicit val boxFunctor: Functor[Box] = new Functor[Box]:
        override def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.contents))

      val box = Box(100)
      val result = Functor[Box].fmap(box)(x => x + 100)
      result should be(Box(200))
    }
    it("can be be composed with other Functors") {
      // These are the same
      type fun1 = Functor[List]
      // type fun2 = Functor[List[?]] Requires type-projector
      // type fun3 = Functor[λ[α => List[α]]] Requires type-projector

      // Functor[List[Option[?]] is Functor[List[λ[α => Option[α]]]] which isn't
      // what you want. The type variable needs to range over the whole
      // type expression; i.e., Functor[λ[α => List[Option[α]]]]

      type ListOption[A] = List[Option[A]]
      val composedFunctor: Functor[ListOption] = Functor[List].compose(Functor[Option])
      var maybeInts = composedFunctor
        .map(List(Some(3), Some(2), Some(5)))(_ + 1)
      maybeInts should be(List(Some(4), Some(3), Some(6)))
      val composedFunctor2: Functor[({ type λ[α] = List[Option[α]] })#λ] =
        Functor[List].compose(Functor[Option])
      val result2 = composedFunctor2
        .map(List(Some(3), Some(2), Some(5)))(_ + 1)
    }

    it("""can use Nested, represents a composition of two Monadic containers.
         | So for this example , using a List[Option[A]] as an object
         | with map""".stripMargin) {
      val result = Nested(List(Some(3), Some(2), Some(5), None))
        .map(x => x + 1)
      result should be(Nested(List(Some(4), Some(3), Some(6), None)))
    }
  }

  describe("Functor Laws") {
    it("""must map the id function over a functor, and
         |  the functor that we get back should
         |  be the same as the original functor""".stripMargin) {
      val original = List(1, 2, 3, 4)
      val result = Functor[List].map(original)(identity)
      original should be(result)
    }

    it("""must compose two functions and then mapping the
         |  resulting function over a functor should be the same as first mapping
         |  one function over the functor and then mapping the other one,
         |  In other words the following fmap should work
         |  fmap (f . g) = fmap f . fmap g""".stripMargin) {

      val addOne = (x: Int) => x + 1
      val multiString = (x: Int) => "Hello" * x
      val composition = addOne.andThen(multiString)

      val functor = Functor[List]
      val f: List[Int] => List[Int] = functor.fmap(_: List[Int])(addOne)
      val g: List[Int] => List[String] = functor
        .fmap(_: List[Int])(multiString)

      val rightCompose: List[Int] => List[String] = f.andThen(g)
      val leftCompose: List[Int] => List[String] = functor
        .fmap(_: List[Int])(composition)

      val xs = List(1, 2, 3, 4)
      leftCompose(xs) should be(rightCompose(xs))
    }

    it("""can have extensions""") {

      def doIt[F[_] : Functor, A : Numeric, B](f:F[A]): F[A] = {
          import cats.syntax.functor.*
          f.map(x => Numeric[A].minus(x, Numeric[A].one))
      }
    }
  }
