/*
 * Copyright 2017 Daniel Hinojosa
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp

import cats._
import cats.data.Nested
import cats.implicits._


import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import org.scalatest.{FunSuite, Matchers}

import scala.language.{postfixOps, reflectiveCalls}

class FunctorSpec extends FunSuite with Matchers {
  test("Case 1: Functor for List") {
    val result = Functor[List].fmap(List(1, 2, 3, 4))(x => x + 30)
    result should be(List(31, 32, 33, 34))
  }

  test("Case 2: Functor for Option") {
    val result = Functor[Option].fmap(Some(13))(10 *)
    result should be(Some(130))
  }

  test("Case 3: Functor for Future") {
    val future = Future {
      40 * 10
    }
    val result = Functor[Future].fmap(future)(x => x / 2)
    result.foreach(_ should be (200))
    Await.ready(result, 3 seconds)
  }

  case class Box[A](contents: A)

  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    override def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.contents))
  }

  test("Case 4: Functor for Custom Objects") {
    val box = Box(100)
    val result = Functor[Box].fmap(box)(x => x + 100)
    result should be(Box(200))
  }

  //These are the same
  type fun1 = Functor[List]
  //type fun2 = Functor[List[?]] Requires type-projector
  //type fun3 = Functor[λ[α => List[α]]] Requires type-projector


  //Functor[List[Option[?]] is Functor[List[λ[α => Option[α]]]] which isn't
  // what you want. The type variable needs to range over the whole
  // type expression; i.e., Functor[λ[α => List[Option[α]]]]

  test("Case 5: Composition for functors.  Functors can compose as one") {
    type ListOption[A] = List[Option[A]]
    val composedFunctor: Functor[ListOption] = Functor[List] compose Functor[Option]
    var maybeInts = composedFunctor.map(List(Some(3), Some(2), Some(5)))(_ + 1)
    maybeInts should be (List(Some(4), Some(3), Some(6)))
    val composedFunctor2: Functor[({ type λ[α] = List[Option[α]] })#λ]= Functor[List] compose Functor[Option]
    val result2 =  composedFunctor2.map(List(Some(3), Some(2), Some(5)))(_ + 1)
  }

  test("""Case 6: Nested with map""") {
    val result = Nested(List(Some(3), Some(2), Some(5), None)).map(x => x + 1)
    result should be (List(Some(4), Some(2), Some(6)))
  }
}
