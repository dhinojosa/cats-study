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
import cats.data.Reader

class ReaderMonadSpec extends FunSpec with Matchers {
  describe("Reader Monad") {
    it("""sequence operations that depend on input, and is created
         |  using an apply that takes a function which starts the chain
         |  of events
      """.stripMargin) {

      case class Cat(name: String, favoriteFood: String)
      val catReader: Reader[Cat, String] = Reader(cat => cat.name)

      info("You can then run the reader using the run method")

      catReader.run(Cat("Roger", "Tuna")) should be("Roger")
    }
    it("""is great for configuration or dependency injection, and reading
         |  information from an outside source""".stripMargin) {
      import cats.syntax.applicative._ // for pure

      case class Db(
        usernames: Map[Int, String],
        passwords: Map[String, String]
      )

      type DbReader[A] = Reader[Db, A]

      def findUsername(userId: Int): DbReader[Option[String]] =
        Reader(_.usernames.find { case (k, v) => k == userId }.map(_._2))

      def checkPassword(username: String, password: String): DbReader[Boolean] =
        Reader(_.passwords.exists(_ == (username, password)))

      def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
        val result: DbReader[Boolean] = for {
          o <- findUsername(userId)
          s <- o
            .map(un => checkPassword(un, password))
            .getOrElse(false.pure[DbReader])
        } yield s
        result
      }

      val users = Map(
        1 -> "dade",
        2 -> "kate",
        3 -> "margo"
      )
      val passwords = Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")

      val db = Db(users, passwords)
      checkLogin(1, "zerocool").run(db) should be(true)
      checkLogin(4, "davinci").run(db) should be(false)
    }
  }
}
