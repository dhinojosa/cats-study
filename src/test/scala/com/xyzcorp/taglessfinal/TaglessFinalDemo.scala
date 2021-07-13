/*
 * Copyright 2021 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.taglessfinal

import cats.*
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*

import scala.concurrent.Future
import scala.language.postfixOps

trait SimpleMath[F[_]]:
  def add(i: F[Int], j: F[Int]): F[Int]
  def subtract(i: F[Int], j: F[Int]): F[Int]
  def squared(i: Int): F[Int]

object SimpleMath:
  def apply[F[_]](implicit calculator: SimpleMath[F]): SimpleMath[F] = calculator

object Calculator:
  def pythagoreanTheorem[F[_]: SimpleMath](a: Int, b: Int): F[Int] =
    val simpleMath = implicitly[SimpleMath[F]]
    import simpleMath.*
    add(squared(a), squared(b))

import scala.concurrent.ExecutionContext.Implicits.global

object Interpreter:
  def addM[F[_]](i: F[Int], j: F[Int])(implicit m: Monad[F]): F[Int] =
    for
      a <- i
      b <- j
    yield a + b

  def subtractM[F[_]](i: F[Int], j: F[Int])(implicit m: Monad[F]): F[Int] =
    for
      a <- i
      b <- j
    yield a - b

  implicit object FutureSimpleMath extends SimpleMath[Future]:
    override def add(i: Future[Int], j: Future[Int]): Future[Int] = addM(i, j)
    override def subtract(i: Future[Int], j: Future[Int]): Future[Int] = subtractM(i, j)
    override def squared(i: Int): Future[Int] = Future(i * i)

  implicit object OptionSimpleMath extends SimpleMath[Option]:
    override def add(i: Option[Int], j: Option[Int]): Option[Int] = addM(i, j)
    override def subtract(i: Option[Int], j: Option[Int]): Option[Int] = subtractM(i, j)
    override def squared(i: Int): Option[Int] = Option(i * i)

  implicit object IOSimpleMath extends SimpleMath[IO]:
    override def add(i: IO[Int], j: IO[Int]): IO[Int] = addM(i, j)
    override def subtract(i: IO[Int], j: IO[Int]): IO[Int] = subtractM(i, j)
    override def squared(i: Int): IO[Int] = IO(i * i)

object MyApp extends IOApp:
  import Calculator.*
  import Interpreter.IOSimpleMath
  override def run(args: List[String]): IO[ExitCode] =
    for
      x <- IO(3)
      y <- IO(4)
      r <- pythagoreanTheorem(x, y)
      ec <- IO(println(r)).as(ExitCode.Success)
    yield ec
