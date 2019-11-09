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

import cats.data.OptionT
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.{Await, ExecutionContext}
import scala.language.postfixOps
import scala.util.{Failure, Success}

class OptionTSpec extends FunSpec with Matchers {

    import scala.concurrent.Future

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
        it("Inverse Cats Implicits") {
            import ExecutionContext.Implicits.global

            val withFlatMap =
                OptionT[Future, Org](getOrg)
                    .flatMap(o => OptionT(getUser(o.id)))
                    .flatMap(u => OptionT.liftF(getUserProps(u.id)))

            val withForComprehension: OptionT[Future, List[Property]] =
                for {
                    o <- OptionT[Future, Org](getOrg)
                    u <- OptionT(getUser(o.id))
                    xs <- OptionT.liftF(getUserProps(u.id))
                } yield xs

            import scala.concurrent.duration._

            List(withFlatMap, withForComprehension).foreach { res =>
                Await.ready(res.value, 3 seconds).onComplete {
                    case Success(o) =>
                        o.fold(fail("empty option"))(xs =>
                            xs should contain inOrder
                                (Property("read"), Property("write")))

                    case Failure(exception) =>
                        exception.printStackTrace()
                }
            }
        }
    }
}
