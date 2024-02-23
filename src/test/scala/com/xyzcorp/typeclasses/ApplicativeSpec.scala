/*
 * Copyright 2019 Daniel Hinojosa
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import java.time.LocalDate
import java.time.temporal.{ChronoUnit, TemporalUnit}

import cats.*
import cats.data.NonEmptyChain
import cats.implicits.*
import org.scalatest.*
import matchers.should.*
import funspec.AnyFunSpec

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import scala.language.postfixOps

class ApplicativeSpec extends AnyFunSpec with Matchers:

  describe("""Applicative like Apply, extends Functor with ap,
             |  where the first argument is F[A=>B]
             |  and the second is F[A]. It is distinctly different than a Functor
             |  in that we can wrap a function in a context. As opposed to
             |  Apply, Applicative has pure""".stripMargin) {

    // M[A]
    // functor: [List(1,2,3,4)][x => x * 2]
    // applicative: [List(x => x * 2)][List(1,2,3,4)]

    it("""Should have a diff between application and monad, here is the monad.
         | Notice that any moment there is an \"evil\" element, then the whole
         | thing is that \"evil\" element""".stripMargin) {
      val o1 = Option(102)
      val o2 = Option(90)
      val o3 = Option.empty[Int]

      val maybeInt =
        o1.flatMap(x => o2.flatMap(y => o3.map(z => x + y + z)))

      maybeInt should be(empty)
//      val option = Applicative[Option]
//        .ap(Option(x => String.valueOf(x) + "!"))(Option(30))
//      option should be(empty)
    }

    it("Should have a diff between application and monad") {
      val o1 = Option(102)
      val o2 = Option(90)
      val o3 = Option(10)
      val result = Applicative[Option].ap3(Option((a, b, c) => ("Point", a, b, c)))(o1, o2, o3)
      result should be(Option("Point", 102, 90, 10))
    }

    it("Should have a diff between application and monad in what happens if there is an \"evil\" element") {
      val o1 = Option(102)
      val o2 = Option(90)
      val o3 = Option.empty[Int]
      val result = Applicative[Option]
        .ap3(Option((a, b, c) => ("Point", a, b, c)))(o1, o2, o3)
      result should be(empty)
    }

    it("""Applies an ap see Ap for more details""".stripMargin) {
      val result =
        Applicative[List]
          .ap(List((x: Int) => x + 1))(List(1, 2, 3))
      result should be(List(2, 3, 4))
    }

    it("extends Functor with pure that creates the Applicative") {
      val intApplicative: List[Int => Int] = Applicative[List].pure((x: Int) => x + 1)
      val result = intApplicative.ap(List(1, 2, 3))
      result should be(List(2, 3, 4))
    }

    it("introduces a <*> operation that is the same as ap") {
      val result = Applicative[Option].<*>(Some[Int => Int](i => 4 * i))(Some(3))
      result should be(Some(12))
    }

    it("can be a variable that is extracted and used as an applicative") {
      val ao = Applicative[Option]
      val result = ao.<*>(Some[Int => Int](i => 4 * i))(Some(3))
      result should be(Some(12))
    }

    it("can also be imported so as to just include the operator") {
      val ao = Applicative[Option]
      import ao.<*>
      val result = <*>(Option((x: Int) => 4 * x))(Some(3))
      result should be(Some(12))
    }

    it("can also be applied in as an infix operator") {
      val result = Option((x: Int) => 4 * x) <*> Some(3)
      result should be(Some(12))
    }

    it("""can do <*> with whatever kind of F[_].
         | The resulting list has every possible combination
         | of applying a function from the left list to a value in the right one.
         |""".stripMargin) {
      val axs = Applicative[List]
      val result = axs.ap(List[Int => String](_.toString, _.toHexString))(List(1, 20, 30, 40))
      result should be(List("1", "20", "30", "40", "1", "14", "1e", "28"))
    }

    it("""can do also with Function[A,B] where we can apply both sides""") {
      val axs = Applicative[List]
      val functions = List[(Int, Int) => Int](_ + _, _ * _)
      val result = functions.ap2(List(1, 2, 3), List(4, 5, 6))
      result should
        be(List(5, 4, 6, 5, 7, 6, 6, 8, 7, 10, 8, 12, 7, 12, 8, 15, 9, 18))

      // (1,4)
      // 1 + 4 = 5
      // 1 * 4 = 4
      // (1, 5)
      // 1 + 5 = 6
      // 1 * 5 = 5
      // (1, 6)
      // 1 + 6 = 7
      // 1 * 6 = 6
      // (2, 4)
      // 2 + 4 = 6
      // 2 * 4 = 8
      // etc.
    }
  }


  describe("new idea for applicative"):
    it("should have a <*>: so that we can apply with right association"):
        pending
        extension[F[_]] (t: Applicative[F])
           def <*>:[A, B](y: F[A])(x: F[A => B]): F[B] = t.ap(x)(y)
        val o1 = Option((x:Int) => x + 1)
        val o3 = Option(3)
        //val result = o1 <*>: o3
        //result should be (Some(4))

  describe("chaining of Applicative") {
    it("can be chained like in the learn you haskell book") {
      val f = (x: Int, y: Int) => x * y
      Applicative[Option].ap2(Option(f))(Option(3), Option(3))
    }
  }

  describe("mapX applies a map serially to contexts") {
    it("can take any number of elements") {
      case class MyItems(a: Int, b: Int, c: Int)
      val o1 = Option(102)
      val o2 = Option(90)
      val o3 = Option(10)
      val result = Applicative[Option]
        .map3(o1, o2, o3)((a, b, c) => MyItems(a, b, c))
      result should be(Option(MyItems(102, 90, 10)))
    }

    it("can take any number of elements, but if one is wrong...") {
      case class MyItems(a: Int, b: Int, c: Int)
      val o1 = Option(102)
      val o2 = Option.empty[Int]
      val o3 = Option(10)
      val result = Applicative[Option]
        .map3(o1, o2, o3)((a, b, c) => MyItems(a, b, c))
      result should be(Option.empty[Int])
    }
  }

  describe("The purpose of an Applicative") {
    import cats.data.Validated.{Invalid, Valid}
    import cats.data.Validated

    def validatePerson(x: String): Validated[String, String] =
      if x.isEmpty then Invalid("Name cannot be blank")
      else Valid(x)

    def validateDateOfBirth(localDate: LocalDate): Validated[String, LocalDate] =
      if localDate.isBefore(LocalDate.now().minus(120, ChronoUnit.YEARS)) then Invalid("Invalid Date")
      else Valid(localDate)

    def validateSSN(s: String): Validated[String, String] =
      if """\d{3}-\d{2}-\d{4}""".r.matches(s) then Valid(s)
      else Invalid("Not a valid format")

    case class Person(s: String, ssn: String, dob: LocalDate)

    it("can process the above algebra to combine messages") {
      val result: Validated[String, Person] = Applicative[[A] =>> Validated[String, A]]
        .map3(validatePerson("Boomer Clownface"),
              validateSSN("123-44-3201"),
              validateDateOfBirth(LocalDate.of(1982, 10, 11))
        )((p, s, b) => Person(p, s, b))
      result should be(Valid(Person("Boomer Clownface", "123-44-3201", LocalDate.of(1982, 10, 11))))
    }

    it("process the errors of what has been entered") {
      val result: Validated[String, Person] = Applicative[[A] =>> Validated[String, A]]
        .map3(validatePerson(""), validateSSN("123-44-3201"), validateDateOfBirth(LocalDate.of(1982, 10, 11)))(
          (p, s, b) => Person(p, s, b)
        )
      result should be(Invalid("Name cannot be blank"))
    }

    it("falls apart when multiple items are added") {
      val result: Validated[String, Person] = Applicative[[A] =>> Validated[String, A]]
        .map3(validatePerson(""), validateSSN("123-44-32013"), validateDateOfBirth(LocalDate.of(1982, 10, 11)))(
          (p, s, b) => Person(p, s, b)
        )
      result should be(Invalid("Name cannot be blankNot a valid format"))
    }
  }
