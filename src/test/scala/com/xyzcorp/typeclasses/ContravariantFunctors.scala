/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import org.scalatest._
import matchers.should._
import funspec.AnyFunSpec

class ContravariantFunctors extends AnyFunSpec with Matchers {
  describe("Contravariant Functors") {
    it(
      """represents prepending of an operation in a chain.
        |  In other words, apply a function before creating
        |  the type class.""".stripMargin) {

      trait Printable[A] {
        outer =>
        def format(value: A): String

        def contramap[B](func: B => A): Printable[B] = new Printable[B] {
          override def format(value: B): String = {
            outer.format(func(value))
          }
        }
      }

      def format[A](value: A)(implicit p: Printable[A]): String =
        p.format(value)

      implicit val stringPrintable: Printable[String] =
        (value: String) => "\"" + value + "\""

      implicit val cm: Printable[Int] =
        stringPrintable.contramap((x: Int) => x.toString)

      implicit val booleanPrintable: Printable[Boolean] =
        (value: Boolean) => if (value) "yes" else "no"

      format("Hello") should be("\"Hello\"")
      format(true) should be("yes")
      format(10) should be("\"10\"")
    }

    it("can be summoned by use of imports") {
      import cats.{Contravariant, Show}
      import cats.instances.string._

      info("first we bring in the show typeclass")
      val showString = Show[String]

      info(
        """We then use Contravariant[Show] with the show type class and
          |  present a function that is to be applied before.""".stripMargin)
      val showSymbol = Contravariant[Show].
        contramap(showString)((sym: Symbol) => s"'${sym.name}")
      showSymbol.show(Symbol("dave")) should be("'dave")
    }

    it("""can be summoned by use of imports, and this time with the use of
        |  bringing one package that has everything needed""".stripMargin) {
      import cats.Show
      import cats.instances.string._
      import cats.syntax.contravariant._

      info("first we bring in the show typeclass")
      val showString = Show[String]

      info(
        """We then use Contravariant[Show] with the show type class and
          |  present a function that is to be applied before.""".stripMargin)

      val showSymbol  = showString
        .contramap((sym: Symbol) => s"'${sym.name}").show(Symbol("dave"))
      showSymbol should be("'dave")
    }

    it (
      """can be used to hitch onto another typeclass, say, a Monoid[A]
        |  where the `combine` method does the following with Symbols
        |  only providing a `Monoid` of `String`:
        |
        |  1. accept two Symbols as parameters;
        |  2. convert the Symbols to Strings;
        |  3. combine the Strings using Monoid[String];
        |  4. convert the result back to a Symbol.
        |
        |  This can be implemented using `imap`""".stripMargin) {

      import cats.Monoid
      import cats.instances.string._
      import cats.syntax.invariant._
      import cats.syntax.semigroup._ // for |+|

      implicit val symbolMonoid: Monoid[Symbol] =
        Monoid[String].imap(Symbol.apply)(_.name)

      info("establish the empty Monoid")
      Monoid[String].empty

      val result = Symbol("hello") |+| Symbol("does") |+| Symbol("this") |+| Symbol("work")
      result shouldEqual Symbol("hellodoesthiswork")
    }
  }
}
