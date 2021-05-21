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

import cats.{Eval, Monoid}
import cats.data.State
import org.scalatest._
import matchers.should._
import funspec.AnyFunSpec

class StateSpec extends AnyFunSpec with Matchers {
  describe("""State Monad allows us to pass around state
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
             |  This about S being a random generator seed and
             |  A being a the result of the random generator
             |
             |  State[S, A] = S => (S,A)""".stripMargin) {

    it("""can do something simple like manage something that
         | a variable would handle, but since we do not like to
         | use variables this is where the state monad
         | can be used, int will be the state, and the String will
         | be the result where the result will be something
         | simple like adding two numbers together.
      """.stripMargin) {

      import scala.language.postfixOps

      val s0: State[Int, String] = State(s => (s, ""))
      val s1 = s0.modify(1 +)
      val s2 = s1.modify(1 +)

      val eval: Eval[(Int, String)] = s2.run(0)
      val result = eval.value
      result._1 should be(2)
    }

    it("should be used by the following were A is a datatype using flatMap") {

      type MyStack = List[Int]

      def pop: State[MyStack, Option[Int]] =
        State {
          case x :: rest => (rest, Option(x))
          case Nil       => (Nil, Option.empty[Int])
        }

      def push(a: Int): State[MyStack, Unit] =
        State { xs =>
          (a :: xs, ())
        }

      val result: State[MyStack, Option[Int]] = push(3)
        .flatMap(_ =>
          pop
            .flatMap(x =>
              push(9)
                .flatMap(_ =>
                  push(12)
                    .flatMap(_ =>
                      pop
                        .map(y => y)
                    )
                )
            )
        )

      result.runA(Nil).value should be(Some(12))
    }

    it("should be used by the following were A is a datatype (I think)") {

      type MyStack = List[Int]

      def pop: State[MyStack, Option[Int]] =
        State {
          case x :: rest => (rest, Option(x))
          case Nil       => (Nil, Option.empty[Int])
        }

      def push(a: Int): State[MyStack, Unit] =
        State { xs =>
          (a :: xs, ())
        }

      val result: State[MyStack, Option[Int]] = for {
        _ <- push(3)
        x <- pop
        _ <- push(9)
        _ <- push(12)
        y <- pop
      } yield y

      result.runA(Nil).value should be(Some(12))
    }
  }
  it("can also be used as the tennis puzzle") {
    trait TennisScore {
      def nextState(opponent: TennisScore): (TennisScore, TennisScore)
    }
    case object Love extends TennisScore {
      def nextState(opponent: TennisScore): (TennisScore, TennisScore) = (fifteen, opponent)
    }
    case object Win extends TennisScore {
      override def nextState(opponent: TennisScore): (TennisScore, TennisScore) = (this, opponent)
    }
    case object Advantage extends TennisScore {
      override def nextState(opponent: TennisScore): (TennisScore, TennisScore) =
        opponent match {
          case Advantage    => (forty, forty)
          case x @ Score(_) => (Win, x)
        }
    }
    case class Score(score: Int)(nextStateRule: TennisScore => (TennisScore, TennisScore)) extends TennisScore {
      def nextState(opponent: TennisScore): (TennisScore, TennisScore) = nextStateRule(opponent)
    }

    lazy val fifteen = Score(15) { o => (thirty, o) }

    lazy val thirty = Score(30) { o => (forty, o) }

    lazy val forty: Score = Score(40) {
      case o @ Score(40) => (Advantage, o)
      case Advantage     => (forty, forty)
      case o             => (Win, o)
    }

    def scoreForA: State[(TennisScore, TennisScore), Unit] =
      State { s => (s._1.nextState(s._2), ()) }

    def scoreForB: State[(TennisScore, TennisScore), Unit] =
      State { s => (s._2.nextState(s._1).swap, ()) }
  }

//
//    it("can run a state as a function") {
//      val count: State[Int, String] = State.pure[Int,String]("Initializing State")
//      val result = for (i <- count) yield i * 3
//      val value = result.runS(0).value
//      value should be (0)
//    }
//
//    it("pure just returns the pure identity for the state given the State monad") {
//      case class Account(name:String, password:String)
//      val state: State[Account, String] = State.pure[Account, String]("Initializing Account")
//      for {
//        ac <- account
//           <- State[Account,String] {ac => ac}
//      }
//    }
//  }
}
