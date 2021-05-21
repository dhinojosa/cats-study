/*
 * Copyright 2021 Daniel Hinojosa
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
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import org.scalatest._
import matchers.should._
import funspec.AnyFunSpec
import cats.free.Free

class FreeMonadSpec extends AnyFunSpec with Matchers {
  case class Student(id: Long, firstName: String, lastName: String)

  trait StudentService[A]
  case class Register(firstName: String, lastName: String) extends StudentService[Long]
  case class FindById(id: Long) extends StudentService[Student]

  type StudentServiceFree[A] = Free[StudentService, A]
  import cats.free.Free.liftF

  def register(firstName: String, lastName: String): StudentServiceFree[Long] = {
    liftF[StudentService, Long](Register(firstName, lastName))
  }

  def findById(longId:Long): StudentServiceFree[Student] = {
    liftF[StudentService, Student](FindById(longId))
  }



}
