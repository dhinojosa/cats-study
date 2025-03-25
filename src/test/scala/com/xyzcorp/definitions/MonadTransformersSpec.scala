/*
 * Copyright 2025 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.definitions

import cats.Id
import cats.data.*
import cats.implicits.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class MonadTransformersSpec extends AnyFunSpec with Matchers:
  describe("Monad Transformers") {
    it("Is used to perform flatMap on nested containers without breaking the structure") {
      val listWithOptions = List(Option(1), Option(2), None, Option(3))

      info("Wrap List[Option[Int]] in OptionT to transform Option inside List")
      val optionT: OptionT[List, Int] = OptionT(listWithOptions)

      info("Perform a flatMap operation that works seamlessly over the nested structure")
      info("x is an int which is wrapped in Option")

      val result: OptionT[List, Int] = optionT.flatMap { x =>
        info("Lifting List[Int] into OptionT[List, Int] which is required for `flatMap`")
        OptionT.liftF(List(x * 10))
      }

      info("Unwrap OptionT to get back to List[Option[Int]]")
      val finalResult: List[Option[Int]] = result.value

      finalResult should be(List(Some(10), Some(20), None, Some(30)))
    }
    it("can be sandwiched with other monads to represent behavior") {
      val result = for
        a <- OptionT.liftF(Try[Int](12 / 3))
        b <- OptionT.liftF(Try[Int](24 / 3))
      yield a + b

      println(result)
    }
    it("would be rather a bit complicated to do without OptionT") {
      val result: Try[Option[Int]] =
        for
          a <- Try(Some(12 / 3)) // Try[Option[Int]]
          b <- Try(Some(24 / 3)) // Try[Option[Int]]
        yield for
          x <- a
          y <- b
        yield x + y
      println(result)
    }
    it("would be rather a bit complicated to do with even more layers of nesting") {
      val result: Try[Option[Int]] =
        for
          a <- Try(Some(12 / 3)) // Try[Option[Int]]
          b <- Try(Some(24 / 3)) // Try[Option[Int]]
        yield for
          x <- a
          y <- b
        yield x + y
      println(result)
    }
    it("We can nest transformers together") {
      type Log = List[String]
      type Stack[A] = OptionT[WriterT[Id, Log, *], A]

      def validateNonEmpty(input: String): Stack[String] =
        if input.nonEmpty then OptionT.liftF(WriterT.tell[Id, Log](List(s"Received valid input: $input")).as(input))
        else OptionT.none[WriterT[Id, Log, *], String]

      def toUpperCase(input: String): Stack[String] =
        OptionT.liftF(
          WriterT.tell[Id, Log](List("Converted to uppercase")).as(input.toUpperCase)
        )

      val program: Stack[String] = for
        raw <- validateNonEmpty("hello")
        upper <- toUpperCase(raw)
      yield upper

      val (log, result): (Log, Option[String]) = program.value.run

      println("Log:")
      log.foreach(println)
      println(s"Result: $result")
    }

    it("We can nest transformers together using TypeLambdas") {
      type Log = List[String]
      type Stack = [A] =>> OptionT[[B] =>> WriterT[Id, Log, B], A]

      def validateNonEmpty(input: String): Stack[String] =
        if input.nonEmpty then OptionT.liftF(WriterT.tell[Id, Log](List(s"Received valid input: $input")).as(input))
        else OptionT.none[WriterT[Id, Log, *], String]

      def toUpperCase(input: String): Stack[String] =
        OptionT.liftF(
          WriterT.tell[Id, Log](List("Converted to uppercase")).as(input.toUpperCase)
        )

      val program: Stack[String] = for
        raw <- validateNonEmpty("hello")
        upper <- toUpperCase(raw)
      yield upper

      val (log, result): (Log, Option[String]) = program.value.run

      println("Log:")
      log.foreach(println)
      println(s"Result: $result")

      type MyList = List[?]
    }

  }
