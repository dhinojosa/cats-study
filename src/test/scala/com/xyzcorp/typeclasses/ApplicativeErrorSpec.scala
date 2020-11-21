/*
 * Copyright 2020 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without
 * limitation the rights to use, copy, modify, merge, publish,
 *  distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import cats._
import org.scalatest.{FunSpec, Matchers}

class ApplicativeErrorSpec extends FunSpec with Matchers {

  describe("""ApplicativeError is an Applicative (see Applicative) that
             |  can handle or raise errors.""".stripMargin) {

    it("""Process an error, where the error is hard coded to
         |  any one of the error types, Either""".stripMargin) {

      def process(x: Int, y: Int): Either[String, Int] = {
        if (y == 0) Left("divisor is zero")
        else {
          Right(x / y)
        }
      }
      process(10, 5) should be(Right(2))
    }

    it("""What would happen though if we want to
         |  abstract to where someone would like to any of
         |  the error types, like Either or Try both of which have
         |  two parametric types. This abstraction is where Applicative Error.
         |  In the following example, just for kicks we are using `map2` which is
         |  a member of applicative.
         |  ApplicativeError is an Applicative therefore all possible functions
         |  that are available to an Applicative are here""".stripMargin) {

      import cats.implicits._
      def process[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] = {
        if (y == 0) ae.raiseError("divisor is zero")
        else {
          val fa = ae.pure(x)
          val fb = ae.pure(y)
          ae.map2(fa, fb)(_ / _)
        }
      }

      type OnError[A] = Either[String, A]
      val e: OnError[Int] = process(30, 10)
    }

    it("""Let's use the error type of Validated""".stripMargin) {
      import cats.implicits._
      import cats.data.Validated
      def process[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] = {
        if (y == 0) ae.raiseError("divisor is zero")
        else {
          ae.pure(x / y)
        }
      }
      type MyValidated[A] = Validated[String, A]
      val e = process[MyValidated](30, 10)
    }

    it("""Here is what it looks like when there is an error, notice there is no
         |  hardcoding with the error and it is abstracted away.
         |  There is no distinction on Either and we can use either
         |  of the Error types""".stripMargin) {

      def process[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[_] = {
        if (y == 0) ae.raiseError("divisor is zero")
        else {
          val fa = ae.pure(x)
          val fb = ae.pure(y)
          ae.map2(fa, fb)(_ / _)
        }
      }

      import cats.implicits._
      type OnError[A] = Either[String, A]
      process[Either[String, *]](30, 0) should be(Left("divisor is zero"))
    }

    it("""Process an Applicative Error where F represents types
         |  that represent a applicative but with a type lambda""".stripMargin) {
      import cats.implicits._
      def process[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[_] = {
        if (y == 0) ae.raiseError("divisor is error")
        else {
          val fa = ae.pure(x)
          val fb = ae.pure(y)
          ae.map2(fa, fb)(_ + _)
        }
      }
      process[({ type T[A] = Either[String, A] })#T](30, 10)
    }

    it("""Process an Applicative Error if an attempt to create the Applicative fails""".stripMargin) {
      import cats.implicits._
      def process[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] =
        if (y == 0) ae.raiseError("Bad Math")
        else if (y == 1) ae.raiseError("Waste of Time")
        else ae.pure(x / y)

      def present[F[_]](f: F[Int])(implicit ae: ApplicativeError[F, String]): F[Int] = {
        ae.handleError(f) {
          case "Bad Math"      => -1
          case "Waste of Time" => -2
          case _               => -3
        }
      }
      present(process(3, 0)) should be(Right(-1))
    }

    it("""Process an Applicative Error if an attempt to create the Applicative fails with HandleErrorWith""".stripMargin) {
      import cats.implicits._
      def process[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] =
        if (y == 0) ae.raiseError("Bad Math")
        else if (y == 1) ae.raiseError("Waste of Time")
        else ae.pure(x / y)

      def present[F[_], M[_], A](f: F[A])(implicit F: ApplicativeError[F, String], M:Monoid[A]): F[A] = {
        F.handleErrorWith(f)(_ => F.pure(M.empty))
      }
      present(process(3, 0)) should be(Right(0))
    }

//    it("""Let's try to do a generic recursion of any process""".stripMargin) {
//      trait TemperatureError
//      case class TemperatureTooHigh(unit: Int, value: Int) extends TemperatureError
//      case class TemperatureTooLow(unit: Int, value: Int) extends TemperatureError
//      case class TemperatureGaugeMalfunctioning(unit: Int) extends TemperatureError
//
//      import cats.implicits._
//      def getReading(x: Int): Either[TemperatureError, Int] = Left(TemperatureTooHigh(1, 120))
//
//      type OnError[A] = Either[TemperatureError, A]
//      val result: OnError[Int] = implicitly[ApplicativeError[OnError, TemperatureError]]
//        .onError(getReading(40)) {
//          case TemperatureTooHigh(i, j) => Right(println("Temp Too High"))
//          case TemperatureTooLow(i, j) => Right(println("Temp Too Cold"))
//          case TemperatureGaugeMalfunctioning(i) => Left(println("Temp Too Cold"))
//        }
//      println(result)
//    }
  }
}
