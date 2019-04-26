/*
 * Copyright 2018 Daniel Hinojosa
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
 * IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

package com.xyzcorp

import cats.Id
import cats.data.{Writer, WriterT}
import org.scalatest.{FunSpec, Matchers}

class WriterMonadSpec extends FunSpec with Matchers {
  describe("Writer Monad") {
    it("carries a log with a computation") {
      import cats.data.Writer
      import cats.instances.vector._

      val writer: WriterT[Id, Vector[String], Int] = Writer(Vector(
        "It was the best of times",
        "it was the worst of times"
      ), 1859)

      val newWriter: WriterT[Id, Vector[String], Int] =
        writer.flatMap(i => Writer(Vector("Added another line"), i * 2))

      info(
        """run returns the contents of the writer, first is the log,
          |  second is the value""".stripMargin)

      val tuple = newWriter.run
      tuple._1 should be(Vector("It was the best of times",
        "it was the worst of times", "Added another line"))
    }

    it(
      """is defined as WriterT[Id, W, A] which is a Writer Transformer,
        |  however, the Id is defined and you just deal with Writer[W,A]
        |  where W is what is used to collect entries, and A is the state.
        |  To use with something like pure, you would have to define
        |  one of the types explicitly.""".stripMargin) {
      import cats.instances.vector._ // for Monoid
      import cats.syntax.applicative._ // for pure

      type Logged[A] = Writer[Vector[String], A]
      val writer = 123.pure[Logged] //Logged has one parameterized type
      val writer2 = writer.tell(Vector("This is a number"))
      val tuple = writer2.run
      tuple._1 should be(Vector("This is a number"))
      tuple._2 should be(123)
    }

    it(
      """can be told no result purely using implicits to
        |  decorate a Vector, this will return a Writer
        |  with a Unit""".stripMargin) {
      import cats.syntax.writer._ // for tell
      val writer: Writer[Vector[String], Unit] =
        Vector("msg1", "msg2", "msg3").tell
      writer.run._1 should be(Vector("msg1", "msg2", "msg3"))
    }

    it(
      """Writer has an apply method, to get started with the
        |  writer monad""".stripMargin) {
      import cats.syntax.writer._ // for writer
      val a = Writer(Vector("msg1", "msg2", "msg3"), 123)

      info("Using writer decoration")
      val b = 123.writer(Vector("msg1", "msg2", "msg3"))

      info("value extracts the value")
      a.value should be(123)

      info("written extracts the log")
      a.written should be(Vector("msg1", "msg2", "msg3"))
    }

    it(
      """is a monad afterall and therefore can be used with
        |  flatmap and for comprehensions.""".stripMargin) {

      import cats.syntax.writer._ // for writer
      import cats.instances.vector._ // for Monoid
      import cats.syntax.applicative._ // for pure

      type Logged[A] = Writer[Vector[String], A]

      info("The following underneath is using combinations of flatMap and map")
      val writer1 = for {
        a <- 10.pure[Logged]
        _ <- Vector("a", "b", "c").tell
        b <- 32.writer(Vector("x", "y", "z"))
      } yield a + b

      writer1.value should be(42)
      writer1.written should be(Vector("a", "b", "c", "x", "y", "z"))
    }

    it("""contains a reset, that can erase the log""") {
      import cats.syntax.writer._ // for writer
      import cats.instances.vector._ // for Monoid

      val writer = 32.writer(Vector("Msg1", "Msg2", "Msg3"))
      writer.reset.written should be(Vector.empty[String])
    }

    it("""contains a swap which changes the log with the value""") {
      import cats.syntax.writer._ // for writer
      import cats.instances.vector._ // for Monoid
      import cats.instances.int._

      val writer = 32.writer(Vector("Msg1", "Msg2", "Msg3"))
      val result = writer.swap

      result.written should be(32)
      result.value should be(Vector("Msg1", "Msg2", "Msg3"))

      //Using semigroup for Int
      result.tell(19).written should be(51)
    }

    it("""can be used concurrently to maintain better logging""") {

      import cats.syntax.writer._ // for writer
      import cats.instances.vector._ // for Monoid
      import cats.syntax.applicative._ // for pure

      def slowly[A](body: => A) =
        try body finally Thread.sleep(100)

      type Logged[A] = Writer[Vector[String], A]

      def factorial(n:Int):Logged[Int] = {
        for {
          i <- if (n == 0) {
            1.pure[Logged]
          } else {
            slowly(factorial(n -1).map(_ * n))
          }
          _ <- Vector(s"fact $n $i").tell
        } yield i
      }

      import scala.concurrent._
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.duration._

      val Vector((w1, v1), (w2, v2)) = Await.result(Future.sequence(Vector(
        Future(factorial(3).run),
        Future(factorial(5).run)
      )), 5.seconds)

      w1.mkString(", ") should be ("""fact 0 1, fact 1 1, fact 2 2, fact 3 6""")
      w2.mkString(", ") should be (
        "fact 0 1, fact 1 1, fact 2 2, fact 3 6, fact 4 24, fact 5 120")
      v1 should be (6)
      v2 should be (120)
    }
  }
}
