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

package com.xyzcorp

import java.util.concurrent.Future

import cats.data.Kleisli
import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.Future

import scala.util.Try

class KleisliSpec extends FunSpec with Matchers {
   describe("Kleisli Data Type") {
     import scala.language.higherKinds
     it ("is a wrapper around the function A => F[B]") {
       final case class MyKleisli[F[_], A, B](run: A => F[B])
     }
     it ("is meant for composition, take the following functions that can compose") {
       val takeFirst: ((String, Int)) => String = t => t._1
       val substring3: String => Try[String] = s => Try(s.substring(3))
       val tryToSuccessOrEmpty: Try[String] => String = ts => ts.getOrElse("")
       val stringCaps : String => String = _.toUpperCase

       val f = takeFirst andThen substring3 andThen tryToSuccessOrEmpty andThen stringCaps
       f("Consider", 90) should be ("SIDER")
     }

     it ("can be used to wrap various elements to one type called Kleisli so as to efficiently manipulate it") {

       pending

       import scala.language.higherKinds
       import scala.concurrent.ExecutionContext.Implicits.global
       import scala.concurrent.Future
       type DB = String
       val k1:Kleisli[Future, DB, String] = Kleisli { db =>
         Future{"Hello"}
       }
       val k2:Kleisli[Option, DB, String] = Kleisli { db =>
         Some("Reached optimal settings")
       }
       //TODO:Fix This
//       import cats.implicits._
//       val resultr = for {
//         a <- k1
//         b <- k2
//       } yield (a + b)
//       resultr
     }
   }
}
