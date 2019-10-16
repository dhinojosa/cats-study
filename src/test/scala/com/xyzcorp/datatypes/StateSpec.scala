/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes

import cats.data.State
import org.scalatest.{FunSpec, Matchers}

class StateSpec extends FunSpec with Matchers {
  describe(
    """State Monad allows us to pass around state
      |  and a description of that state. The state
      |  is composed using map and flatMap.
      |
      |  State[S, A] is the type where:
      |     * S is the state,
      |     * A is the type of the result
      |
      |  This allows us to manage
      |  state in a functional way.
      |
      |  State[S, A] = S => (S,A)""".stripMargin) {


    it (
      """can do something simple like manage something that
        | a variable would handle, but since we do not like to
        | use variables this is where the state monad
        | can be used, int will be the state, and the String will
        | be the result where the result will be something
        | simple like adding two numbers together.
      """.stripMargin) {

      val s0: State[Int, String] = State.apply(s => (s, ""))
      val s1 = s0.modify(i => i * 3)

    }






    it("can run a state as a function") {
      val count: State[Int, String] = State.pure[Int,String]("Initializing State")
      val result = for (i <- count) yield i * 3
      val value = result.runS(0).value
      value should be (0)
    }

    it("pure just returns the pure identity for the state given the State monad") {
//      case class Account(name:String, password:String)
//      val state: State[Account, String] = State.pure[Account, String]("Initializing Account")
//      for {
//        ac <- account
//           <- State[Account,String] {ac => ac}
//      }
    }
  }
}