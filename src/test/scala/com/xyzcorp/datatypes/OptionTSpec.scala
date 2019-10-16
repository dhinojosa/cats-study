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

import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

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

  alert("This needs to be defined better")
  describe("OptionT is a transformer") {
    it("Inverse Cats Implicits") {
      Seq(Option(1), Option(2), Option(3)).toList.traverse[Option, Int](
        identity).map(_.toSeq)
    }
  }
}
