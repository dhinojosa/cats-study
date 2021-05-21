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
package com.xyzcorp.definitions

import org.scalatest._
import matchers.should._
import funspec.AnyFunSpec

class HigherKindsSpec extends AnyFunSpec with Matchers {
    describe("Higher Kinds") {
        it("""is a parameterized type that represents the container or
              |  collection. List[A] means that I have a generic A (if the
              |  the type is not found in the classpath.  Imagine in Java
              |  if we have M[A]? Where M can either represent a List, a
              |  Set, a Future. But not a Map or a Function why?""".stripMargin) {
            case class Box[A](value: A)

            trait MyFunctor[M[_]] {
                def myMap[A, B](m: M[A])(f: A => B): M[B]
            }

            object MyFunctor {
                def apply[T[_]](implicit f: MyFunctor[T]): MyFunctor[T] = f
            }

            implicit val functorForBox: MyFunctor[Box] = new MyFunctor[Box] {
                override def myMap[A, B](m: Box[A])(f: A => B): Box[B] = Box(
                    f(m.value))
            }

            val myBox = Box(200)
            MyFunctor.apply[Box].myMap(myBox)(x => "Hello" * 3)
        }
    }
}
