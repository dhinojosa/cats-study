/*
 * Copyright 2017 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp

import cats._
import cats.implicits._
import org.scalatest.{FunSuite, Matchers}

import scala.language.postfixOps

class ApplicativeSpec extends FunSuite with Matchers {
  test("""Case 1: Applicative extends Functor with ap and pure""") {
    val result = Applicative[List].ap(List((x: Int) => x + 1))(List(1, 2, 3))
    result should be(List(2, 3, 4))
  }

  test(
    """Case 2: Create an Applicative List with pure, pure places the
      |function into an applicative""") {
    val intApplicative = Applicative[List].pure((x: Int) => x + 1)
    val result = intApplicative.ap(List(1, 2, 3))
    result should be(List(2, 3, 4))
  }

  test("""Case 3: Create an Applicative with <*> operation with uses ap""") {
    val result = Applicative[Option].<*>(Some[Int => Int](4 *))(Some(3))
    result should be(Some(12))
  }

  test(
    """Case 4: Create an Applicative and an alias for Applicative Option<*>
      |operation with uses |ap""") {
    val ao = Applicative[Option]
    val result = ao.<*>(Some[Int => Int](4 *))(Some(3))
    result should be(Some(12))
  }
}
