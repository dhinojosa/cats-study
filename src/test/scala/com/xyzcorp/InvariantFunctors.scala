/*
 * Copyright 2018 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without
 * limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and
 * to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp

import org.scalatest.{FunSpec, Matchers}

class InvariantFunctors extends FunSpec with Matchers {
  describe("Invariant Functors") {
    trait Codec[A] {
      self =>
      def encode(value: A): String

      def decode(value: String): A

      def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
        override def encode(value: B): String = self.encode(enc(value))

        override def decode(value: String): B = dec(self.decode(value))
      }
    }

    def encode[A](value: A)(implicit c: Codec[A]): String =
      c.encode(value)

    def decode[A](value: String)(implicit c: Codec[A]): A =
      c.decode(value)

    implicit val stringCodec: Codec[String] =
      new Codec[String] {
        def encode(value: String): String = value

        def decode(value: String): String = value
      }

    implicit val intCodec: Codec[Int] =
      stringCodec.imap(_.toInt, _.toString)

    implicit val booleanCodec: Codec[Boolean] =
      stringCodec.imap(_.toBoolean, _.toString)

    implicit val doubleCodec: Codec[Double] =
      stringCodec.imap(_.toDouble, _.toString)


    it("""implements a method called imap that is informally
         |  equivalent to a combination of map and contramap. imap generates
         |  them via a pair of bidirectional transformations""".stripMargin) {
      intCodec.encode(10) should be("10")
      intCodec.decode("10") should be(10)
      doubleCodec.encode(1230.00) should be("1230.0")
    }

    it("""can be used for custom types of course, done so this time, by
         | using a method to extend where to find the codec.""".stripMargin) {

      case class Box[A](value: A)
      implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
        c.imap[Box[A]](Box(_), _.value)

      val box = Box(40)

      boxCodec[Int].encode(box) should be("40")

      def foo[A](b:Box[A])(implicit c:Codec[Box[A]]): String = {
        c.encode(b)
      }

      foo(box) should be ("40")
    }
  }
}
