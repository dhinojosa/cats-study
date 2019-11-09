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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes

import org.scalatest.{FunSpec, Matchers}

class IOSpec extends FunSpec with Matchers {
  describe("IO Monad") {
    it(
      """ captures an IO Effect, something
        | that happens to the outside world and does so lazily
      """.stripMargin) {
      import cats.effect.IO
      def sayHelloWorldTwice(s: String): IO[Unit] = {
        IO[Unit](println(s)).flatMap(_ => IO[Unit](println(s)))
      }
      val container: IO[Unit] = sayHelloWorldTwice("Hello World")

      println("Nothing has happened yet!")
      container.unsafeRunSync()
    }

    it("""can be rewritten using a flatMap for better clarity""".stripMargin) {
      import cats.effect.IO
      def printStringTwice(s: String): IO[Unit] = {
        for {_ <- IO(println(s))
             _ <- IO(println(s))
            } yield ()
      }
      val printHelloWorldTwice: IO[Unit] = printStringTwice("Hello World")
      printHelloWorldTwice.unsafeRunSync()
    }
    it("""can be rewritten now using assignment since the invocation is lazy""".stripMargin) {
      import cats.effect.IO
      def sayHelloWorldTwice(s: String): IO[Unit] = {
        val printString = IO(println(s))
        for {_ <- printString
             _ <- printString
        } yield ()
      }
      val container: IO[Unit] = sayHelloWorldTwice("Hello World")
      container.unsafeRunSync()
    }
    it ("""can be used to program interaction of events""") {
      import cats.effect.IO

      def putStrLn(value: String) = IO(println(value))
      val readLn = IO(scala.io.StdIn.readLine)

      val io: IO[Unit] = for {
        _ <- putStrLn("What's your name?")
        n <- readLn
        _ <- putStrLn(s"Hello, $n!")
      } yield ()

      io.unsafeRunSync()
    }
  }
}
