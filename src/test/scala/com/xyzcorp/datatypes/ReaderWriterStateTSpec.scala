/*
 * Copyright 2025 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes

import cats.Id
import cats.data.ReaderWriterStateT
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ReaderWriterStateTSpec extends AnyFunSpec with Matchers:
  describe("ReaderWriterStateT") {
    it("""is a transformer type that combines:
         | **Reader** (`R => F[A]`) for dependency injection and passing contextual information,
         | **Writer** (`(F[A], L)` with a log/monoidal value) for logging or accumulating side-effect-free information, and
         | **State** (`S => (F[A], S)`) for maintaining transformations to mutable state in an immutable way.
         |
         | - **`F[_]` **: The effect type (e.g., `Id`, `Option`, `Either`, etc.).
         | - **`R`**: The type of the contextual environment (Reader).
         | - **`L`**: The type of the logged value (Writer).
         | - **`S`**: The type of the state being modified (State).
         | - **`A` **: The resulting output of the computation.
         |
         | """.stripMargin) {

      type RWS[R, L, S, A] = ReaderWriterStateT[Id, R, L, S, A]
      val computation: RWS[String, Vector[String], Int, String] = ReaderWriterStateT { (env, state) =>
        info("Use context (Reader), logging (Writer), and state (State) together")
        val log = Vector(s"Environment was: $env", s"Starting state was: $state")
        val result = s"Result from environment: $env"
        val newState = state + 1
        (log, newState, result)
      }

      info("Run the computation (with Reader context and initial state)")
      val (log, finalState, result) = computation.run("Starting Test Run", 0)

      println(log)
      log should be(Vector("Environment was: Starting Test Run", "Starting state was: 0"))
      finalState should be(1)
      result should be("Result from environment: Starting Test Run")
    }
    it("""can be used in a monadic style""") {
      type RWS[R, L, S, A] = ReaderWriterStateT[Id, R, L, S, A]
      val computation1: RWS[String, Vector[String], Int, String] = ReaderWriterStateT.apply((env, state) =>
        val log = Vector(s"Environment was: $env", s"Starting state was: $state")
        val result = s"Result from environment 1: $env"
        val newState = state + 1
        (log, newState, result)
      )
      val computation2: RWS[String, Vector[String], Int, String] = ReaderWriterStateT.apply((env, state) =>
        val log = Vector(s"Environment 2 was: $env", s"Starting state 2 was: $state")
        val result = s"Result from environment 2: $env"
        val newState = state + 2
        (log, newState, result)
      )

      val result = for
        x <- computation1
        y <- computation2
        _ <- computation1.tell(Vector("This is a another log entry"))
        _ <- computation2.tell(Vector("This is a another log entry from 2"))
        z <- computation1.listen
      yield x + y + z

      val (log, finalState, finalResult) = result.run("Starting Test Run", 0)
      println(s"Log: $log\nFinal State: $finalState\nFinal Result: $finalResult\n")
    }
    it("contains local which allows you to modify the context") {
      type RWS[R, L, S, A] = ReaderWriterStateT[Id, R, L, S, A]
      val computation: RWS[String, Vector[String], Int, String] = ReaderWriterStateT.apply((env, state) =>
        val log = Vector(s"Environment was: $env", s"Starting state was: $state")
        val result = s"Result from environment $env"
        val newState = state + 1
        (log, newState, result)
      )
      val tuple = computation
        .local(ee => s"New Environment: $ee")
        .run("Starting Test Run", 0)

      val environment: Seq[String] = tuple._1
      environment should be(Vector("Environment was: New Environment: Starting Test Run", "Starting state was: 0"))
      val state: Int = tuple._2
      state should be(1)
      val result: String = tuple._3
      result should be("Result from environment New Environment: Starting Test Run")
    }
  }
