/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp

import cats.data.State
import org.scalatest.Matchers

class StateMonadSpec extends FunctorSpec with Matchers {
  describe(
    """State Monad allows us to pass around state
      |  and a description of that state. The state
      |  is composed using map and flatMap. State[S, A] is the
      |  type where S is the state, and A is the type of
      |  the result. This allows us to manage
      |  state in a functional way.
      |  State[S, A] = S => (S,A)""".stripMargin) {

    it("can run a state as a function") {
      val count: State[Int, String] = State.pure[Int,String]("Initializing State")
      val result = for (i <- count) yield i * 3
      val value = result.runS(0).value
      value should be (0)
    }

    it("pure just returns the pure identity for the state given the State monad") {
      case class Account(name:String, password:String)
      val account: State[Account, String] = State.pure[Account, String]("Initializing Account")





    }
  }
}
