/*
 * Copyright 2019 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.typeclasses

import org.scalatest.{FunSpec, Matchers}

class EqSpec extends FunSpec with Matchers {

  describe("Eq") {
    it("is a type class that has the following definition") {
      trait Eq[A] {
        def eqv(a: A, b: A): Boolean
      }
    }
    it("is available with Cats via import") {
      import cats.Eq
      import cats.instances.int._
      val eqInt = Eq[Int]
      eqInt.eqv(4, 5) should be(false)
      eqInt.eqv(4, 4) should be(true)
    }
    it("has specialized functions === and =!=") {
      (4 === 5) should be(false) //This is lying, it is using ScalaTest ===
    }
    it("can also obviously be used for custom types") {
      import cats.Eq
      case class Employee(firstName: String, lastName: String, salary: Int)
      object Employee {

        import cats.instances.int._
        import cats.instances.string._

        implicit val eqFirstNameOnly: Eq[Employee] = (x: Employee, y: Employee) => {
          implicitly[Eq[String]].eqv(x.firstName, y.firstName)
        }
        implicit val eqLastNameOnly: Eq[Employee] = (x: Employee, y: Employee) => {
          implicitly[Eq[String]].eqv(x.lastName, y.lastName)
        }
        implicit val eqSalaryOnly: Eq[Employee] = (x: Employee,
                                          y: Employee) => implicitly[Eq[Int]]
          .eqv(x.salary, y.salary)

        implicit val eqAll: Eq[Employee] = (x: Employee, y: Employee) => {
            implicitly[Eq[String]].eqv(x.firstName, y.firstName) &&
            implicitly[Eq[String]].eqv(x.lastName, y.lastName) &&
            implicitly[Eq[Int]].eqv(x.salary, y.salary)
        }
      }

      val deNiro1 = Employee("Robert","DeNiro", 30000)
      val deNiro2 = Employee("Robert","DeNiro", 30000)
      val duvall = Employee("Robert","Duvall", 17000)
      val downeyJr = Employee("Robert","Downey Jr", 17000)

      import Employee.eqAll
      implicitly[Eq[Employee]].eqv(duvall, downeyJr) should be (false)
      implicitly[Eq[Employee]].eqv(deNiro1, deNiro2) should be (true)
    }
  }
}
