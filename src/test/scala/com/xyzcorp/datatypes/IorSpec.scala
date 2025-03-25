/*
 * Copyright 2025 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes

import cats.*
import cats.data.{Ior, NonEmptyChain as Nec}
import cats.implicits.*
import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*

class IorSpec extends AnyFunSpec with Matchers:
  describe("""Ior represents an inclusive-or relationship between two data types,
               where a value can exist in either or in both simultaneously.
               This is opposed to `Either` which is an exclusive or""") {
    it("can be created using the `left` method") {
      val result = Ior.left[Exception, Int](new Exception("error"));
      result.isLeft should be(true)
      result.isRight should be(false)
      result.isBoth should be(false)
    }
    it("can be created using the `right` method") {
      val result = Ior.right[Exception, Int](4);
      result.isLeft should be(false)
      result.isRight should be(true)
      result.isBoth should be(false)
    }
    it("can be created using the `both` method") {
      val result = Ior.both[Exception, Int](new Exception("error"), 4);
      result.isLeft should be(false)
      result.isRight should be(false)
      result.isBoth should be(true)
    }
    it("has a type of Ior[L, R] or L Ior R") {
      val result1: Ior[String, Int] = Ior.both[String, Int]("error", 4);
      val result2: String Ior Int = Ior.both[String, Int]("error", 4);
      result1 should be(result2)
    }

    it("has a use case of holding on to an attempted element as well as the error message") {
      type Failures = Nec[String]

      case class Username(value: String)
      case class Password(value: String)
      case class User(username: Username, password: Password)

      info("Here we are using an Ior because we want to determine the error but also hold the object")
      def validateUsername(u: String): Failures Ior Username =
        if u.isEmpty then Nec.one("Can't be empty").leftIor
        else if u.contains(".") then Ior.both(Nec.one("Dot in name is deprecated"), Username(u))
        else Username(u).rightIor

      def validatePassword(p: String): Failures Ior Password =
        if p.length < 8 then Nec.one("Password too short").leftIor
        else if p.length < 10 then Ior.both(Nec.one("Password should be longer"), Password(p))
        else Password(p).rightIor

      def validateUser(name: String, password: String): Failures Ior User =
        (validateUsername(name), validatePassword(password)).mapN(User.apply)
    }

    describe("Type classes that are available for Ior") {
      it("has functor which only applies to the right, here since it is a left, there is no consequence") {
        import cats.implicits.*
        type IorLeftString = [A] =>> Ior[String, A]
        val functor: Functor[IorLeftString] = summon[Functor[IorLeftString]]
        functor.map(Ior.left[String, Int]("error"))(s => s"$s!") should be(Ior.left("error"))
      }
      it("has functor which only applies to the right, here we will manipulate the right") {
        import cats.implicits.*
        type IorLeftString = [A] =>> Ior[String, A]
        val functor: Functor[IorLeftString] = summon[Functor[IorLeftString]]
        functor.map(Ior.right[String, Int](1230))(i => i * 30) should be(Ior.right(36900))
      }
      it("has functor which only applies to the right, here we will manipulate the right, but using a both") {
        import cats.implicits.*
        type IorLeftString = [A] =>> Ior[String, A]
        val functor: Functor[IorLeftString] = summon[Functor[IorLeftString]]
        functor.map(Ior.both[String, Int]("error", 1230))(i => i * 30) should be(Ior.both("error", 36900))
      }
    }
  }
