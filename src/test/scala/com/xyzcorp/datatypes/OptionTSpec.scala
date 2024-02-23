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

import cats.arrow.FunctionK
import cats.data.OptionT
import cats.implicits.*
import cats.~>
import org.scalatest.*
import matchers.should.*
import funspec.AnyFunSpec

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import ExecutionContext.Implicits.global

class OptionTSpec extends AnyFunSpec with Matchers:

    case class Org(id: Int, name: String)

    case class User(id: Int, name: String)

    case class Property(name: String)

    def getOrg: Future[Option[Org]] = Future
        .successful(Some(Org(1, "Accounting")))

    def getUser(orgId: Int): Future[Option[User]] = Future
        .successful(Some(User(2, "danno")))

    def getUserProps(userId: Int): Future[List[Property]] = Future
        .successful(List(Property("read"), Property("write")))

    describe("OptionT is a transformer") {
        it(
            """has the signature F[Option[_]] and can perform transformations,
              | here is a chain of maps showing inefficient programming""".stripMargin) {
            val eventualMaybeInt: Future[Option[Int]] = Future(Option(3))
            val result: Future[Option[Int]] = eventualMaybeInt.map(o => o.map(i => i + 40))
            result
        }
        it(
            """has the ability to perform efficient programming like the following making it
              | more succinct and using just one flatMap""".stripMargin) {
            val ot: OptionT[Future, Int] = OptionT[Future, Int](Future(Option(3)))
            val result = ot.flatMap(i => OptionT(Future(Option(i + 40))))
            Await.result(result.value, 2 seconds) should be(Option(43))
        }
        it(
            """has the ability to for comprehensions since it is monadic""".stripMargin) {
            val ot0: OptionT[Future, Int] = OptionT[Future, Int](Future(Option(3)))
            val ot1: OptionT[Future, Int] = OptionT[Future, Int](Future(Option(2)))
            val ot2: OptionT[Future, Int] = OptionT[Future, Int](Future(Option(1)))
            val result = for {i <- ot0
                              j <- ot1
                              k <- ot2} yield s"$i $j $k"
            Await.result(result.value, 2 seconds) should be(Some("3 2 1"))
        }

        it("""offers easier composition using a for comprehension""".stripMargin) {
            val ot0: OptionT[Future, Int] = OptionT[Future, Int](Future(Option(3)))
            val result = for {i <- ot0
                              j <- OptionT.some[Future](i * 2)
                              k <- OptionT.some[Future](j * 2)} yield s"$i $j $k"
            Await.result(result.value, 2 seconds) should be(Some("3 6 12"))
        }


        it("Inverse Cats Implicits") {
            val withFlatMap =
                OptionT[Future, Org](getOrg)
                    .flatMap(o => OptionT(getUser(o.id)))
                    .flatMap(u => OptionT.liftF(getUserProps(u.id)))

            val withForComprehension: OptionT[Future, List[Property]] =
                for
                    o <- OptionT[Future, Org](getOrg)
                    u <- OptionT(getUser(o.id))
                    xs <- OptionT.liftF(getUserProps(u.id))
                yield xs


            List(withFlatMap, withForComprehension).foreach { res =>
                Await.ready(res.value, 3 seconds).onComplete {
                    case Success(o) =>
                        o.fold(fail("empty option")) { xs =>
                            println(xs)
                            xs should be(List(Property("read"), Property("write")))
                        }

                    case Failure(exception) =>
                        exception.printStackTrace()
                }
            }
        }
        describe("various ways to construct") {
            it("has an apply") {
                val optionT: OptionT[Try, Int] = OptionT.apply(Try(Option(8)))
                optionT.value should be(Success(Option(8)))
            }
            it("has a pure") {
                val result: OptionT[Future, Int] = OptionT.pure[Future](10)
                Await.result(result.value, 2 seconds) should be(Some(10))
            }
            it(
                """has a some which is the same as a call to pure but
                  |  pronounces that we are wanting a some""".stripMargin) {
                val result: OptionT[Future, Int] = OptionT.some[Future](30)
                Await.result(result.value, 2 seconds) should be(Some(30))
            }
            it("""has a none which will return a none as part of it's option""".stripMargin) {
                val result: OptionT[Future, Int] = OptionT.none[Future, Int]
                Await.result(result.value, 2 seconds) should be(None)
            }
            it(
                """has a liftF which takes a F[_] and whatever value it contains it will place that
                  | in an option as part of the OptionT""".stripMargin) {
                val result: OptionT[Future, Int] = OptionT.liftF[Future, Int](Future(40))
                Await.result(result.value, 2 seconds) should be(Some(40))
            }
            it("has a liftK which returns a FunctionK") {
                type OptionTFuture = [B] =>> OptionT[Future, B]
                val arrow: FunctionK[Future, OptionTFuture] = OptionT.liftK[Future]
                val funcK = arrow.apply(Future(10))
                Await.result(funcK.value, 2 seconds) should be(Some(10))
            }
            it("has a liftK which returns a FunctionK with inlined lambda types") {
                val arrow: FunctionK[Future, [A] =>> OptionT[Future, A]] = OptionT.liftK[Future]
                val funcK = arrow.apply(Future(10))
                Await.result(funcK.value, 2 seconds) should be(Some(10))
            }
            it(
                """has a unlessF which takes a boolean, and returns a functionK with
                  |  the F[_] if false or none, here is boolean set to true""".stripMargin) {
                val functionK: OptionT[Future, Int] = OptionT.unlessF(true)(Future(10))
                Await.result(functionK.value, 2 seconds) should be(None)
            }
            it(
                """has a unlessF which takes a boolean, and returns a functionK with
                  |  the F[_] if false or none, here is boolean set to false""".stripMargin) {
                val functionK: OptionT[Future, Int] = OptionT.unlessF(false)(Future(10))
                Await.result(functionK.value, 2 seconds) should be(Some(10))
            }
            it(
                """has unlessK which is the same of unlessF, but returns an arrow""".stripMargin) {
                val arrow: FunctionK[Future, [A] =>> OptionT[Future, A]] = OptionT.unlessK[Future](true)
                val funcK = arrow.apply(Future(10))
                Await.result(funcK.value, 2 seconds) should be(None)
            }
            it(
                """has unlessK which is the same of unlessF, but returns an arrow; here with an alias""".stripMargin) {
                type Mon = ~>[Future, [A] =>> OptionT[Future, A]]
                val arrow: Mon = OptionT.unlessK[Future](true)
                val funcK = arrow.apply(Future(10))
                Await.result(funcK.value, 2 seconds) should be(None)
            }
            it(
                """has unlessK which is the same of unlessF, but returns an arrow; here with an infix""".stripMargin) {
                type Mon = Future ~> ([A] =>> OptionT[Future, A])
                val arrow: Mon = OptionT.unlessK[Future](true)
                val funcK = arrow.apply(Future(10))
                Await.result(funcK.value, 2 seconds) should be(None)
            }
            it(
                """has unlessK which is the same of unlessF, but returns an arrow; here with an infix and inline""".stripMargin) {
                val arrow: Future ~> ([A] =>> OptionT[Future, A]) = OptionT.unlessK[Future](true)
                val funcK = arrow.apply(Future(10))
                Await.result(funcK.value, 2 seconds) should be(None)
            }
            it(
                """has whenF but return the value as Some if the boolean is true""".stripMargin) {
                val whenF = OptionT.whenF(true)(Future(100))
                Await.result(whenF.value, 2 seconds) should be(Some(100))
            }
        }

        describe("use in a list") {
            it(
                """should be able to retrieve the value
                  |  and if not available it would return a default""".stripMargin) {
                val optionT = OptionT.apply(List(Some(30), None, Some(40)))
                val result = optionT.getOrElse(-1)
                result should be(List(30, -1, 40))
            }
        }

        describe("use in a future") {
            it(
                """should be able to retrieve the value
                  |  and if not available it would return a default""".stripMargin) {
                val optionT = OptionT.liftF(Future.successful(20))
                val result = optionT.getOrElse(-1)
                Await.result(result, 2 seconds) should be(20)
            }
            it(
                """catamorphism - general abstraction that enables you to handle multiple values,
                  |  for example in order to reduce them to a single value""".stripMargin) {
                val optionT = OptionT.liftF(Future.successful(20))
                val result = optionT.cata("Hello", x => s"$x!")
                Await.result(result, 2 seconds) should be("20!")
            }
            it(
                """collect can take the value of the context and perform a match, if the match is
                  |  resolved then it will perform a map on the value""".stripMargin) {
                val optionT = OptionT.liftF(Future.successful(20))
                val result = optionT.collect {
                    case n if (1 to 3).contains(n) => "Three"
                    case n if (4 to 100).contains(n) => "Four to One Hundred"
                    case _ => "Another Number"
                }
                Await.result(result.getOrElse("Other"), 2 seconds) should be("Four to One Hundred")
            }
        }
    }
