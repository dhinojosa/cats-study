/*
 * Copyright 2020 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import cats._
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

class MonadErrorSpec extends FunSpec with Matchers {

  describe("""Trait MonadError[F[_], E] can raise or handle an error value.
             |  It extends Monad, and ApplicativeError""".stripMargin) {
    it("is used to manipulate errors much like either to allow flatMapping") {

      type MyEither[A] = Either[String, A]

      def getCityClosestToCoordinate[F[_]](x: (Int, Int))(implicit ae: ApplicativeError[F, String]): F[String] = {
        ae.pure("Minneapolis, MN")
      }

      def getTemperatureByCity[F[_]](city: String)(implicit ae: ApplicativeError[F, String]): F[Int] = {
        ae.pure(78)
      }

      def getTemperatureFromByCoordinates[F[_]](x: (Int, Int))(implicit me: MonadError[F, String]): F[Int] = {
        if (x._1 < 0 || x._2 < 0) me.raiseError("Invalid Coordinates")
        //me.flatMap(getCityClosestToCoordinate[F](x))(city => getTemperatureByCity[F](city))
          for { c <- getCityClosestToCoordinate[F](x)
                t <- getTemperatureByCity[F](c) } yield t
      }

      getTemperatureFromByCoordinates[MyEither](120 -> 10)
    }
  }

  it("is used to manipulate errors much like either to allow flatMapping " +
      "  but with it as a wrapper to guarantee that we can flatMap") {

    type MyEither[A] = Either[String, A]

    def getCityClosestToCoordinate[F[_]](x: (Int, Int))(implicit ae: ApplicativeError[F, String]): F[String] = {
      ae.pure("Minneapolis, MN")
    }

    def getTemperatureByCity[F[_]](city: String)(implicit ae: ApplicativeError[F, String]): F[Int] = {
      ae.pure(78)
    }

    def getTemperatureFromByCoordinates[F[_]: MonadError[*[_], String]](x: (Int, Int)): F[Int] = {
      for { c <- getCityClosestToCoordinate[F](x)
            t <- getTemperatureByCity[F](c) } yield t
    }

    getTemperatureFromByCoordinates[MyEither](44 -> 93) should be (Right(78))
  }
}
