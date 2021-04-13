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

package com.xyzcorp.definitions

import org.scalatest.{FunSpec, Matchers}

class TypeClassSpec extends FunSpec with Matchers {

  import scala.language.higherKinds

  describe("Type classes") {
    it("""takes an abstraction and offers varying
         |  ways to implement that abstraction for
         |  any kind of type that you wish to represent.""".stripMargin) {

      case class Employee(firstName: String, lastName: String, salary: Int)

      object Employee {
        implicit val sortByFirstName: Ordering[Employee] = new Ordering[Employee] {
          override def compare(x: Employee, y: Employee): Int =
            x.firstName.compareTo(y.firstName)
        }

        implicit val sortByLastName: Ordering[Employee] = new Ordering[Employee] {
          override def compare(x: Employee, y: Employee): Int =
            x.lastName.compareTo(y.lastName)
        }
      }

      val employees = List(
        Employee("Dan", "Hinojosa", 40000),
        Employee("Brian", "Sletten", 42000),
        Employee("Venkat", "Subramaniam", 50000),
        Employee("Jonathan", "Johnson", 20000),
        Employee("Chris", "Maki", 35000)
      )

      import Employee.sortByLastName
      (employees.sorted should contain).inOrder(
        Employee("Dan", "Hinojosa", 40000),
        Employee("Jonathan", "Johnson", 20000),
        Employee("Chris", "Maki", 35000),
        Employee("Brian", "Sletten", 42000),
        Employee("Venkat", "Subramaniam", 50000)
      )
    }
  }
}
