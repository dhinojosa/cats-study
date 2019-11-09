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

package com.xyzcorp.typeclasses

import java.text.NumberFormat
import java.util.Locale

import org.scalatest.{FunSpec, Matchers}

class ShowSpec extends FunSpec with Matchers {
  it("is a type class that has the following definition") {
    trait Show[T] {
      def show(t: T): String
    }
  }
  it("is available with Cats via import") {
    import cats.Show
    import cats.instances.float._
    import cats.instances.int._
    implicitly[Show[Int]].show(100) should be("100")
    implicitly[Show[Float]].show(90.0f) should be("90.0")
  }
  it(
    """is absolutely flexible to do what you want
      | to do as you provide the context for whatever
      | use you have!""".stripMargin){

    import cats.Show
    implicit val scientificFloatShow:Show[Float] = new Show[Float] {
      override def show(t: Float): String = f"$t%2.2e"
    }
    implicit val scientificDoubleShow:Show[Double] = new Show[Double] {
      override def show(t: Double): String = f"$t%2.2e"
    }
    Show[Float].show(3920301.00f) should be ("3.92e+06")
  }

  it("can also obviously be used for custom types") {
    import cats.Show
    case class Employee(firstName: String, lastName: String, salary: Int)
    object Employee {
      val formatter: NumberFormat = java.text.NumberFormat.getCurrencyInstance(Locale.US)

      implicit val showFirstNameThenLastAndSalary: Show[Employee] = new Show[Employee] {
        override def show(t: Employee): String = {
          f"${t.firstName}%s ${t.lastName}%s : ${formatter.format(t.salary)}%s"
        }
      }
      implicit val showLastThenFirstAndSalary: Show[Employee] = new Show[Employee] {
        override def show(t: Employee): String = {
          f"${t.lastName}%s, ${t.firstName}%s : ${formatter.format(t.salary)}%s"
        }
      }
      implicit val showLastThenFirstAndMaskedSalary: Show[Employee] = new Show[Employee] {
        override def show(t: Employee): String = {
          val regex = """\d""".r
          val masked = regex.replaceAllIn(formatter.format(t.salary), "X")
          f"${t.lastName}%s, ${t.firstName}%s : $masked%s"
        }
      }
    }

    val deNiro1 = Employee("Robert","DeNiro", 30000)
    val deNiro2 = Employee("Robert","DeNiro", 30000)
    val duvall = Employee("Robert","Duvall", 17000)
    val downeyJr = Employee("Robert","Downey Jr", 17000)

    {
        import Employee.showLastThenFirstAndMaskedSalary
        implicitly[Show[Employee]].show(deNiro1) should
            be("DeNiro, Robert : $XX,XXX.XX")
        implicitly[Show[Employee]].show(downeyJr) should
            be("Downey Jr, Robert : $XX,XXX.XX")
    }

      {
          import Employee.showLastThenFirstAndSalary
          implicitly[Show[Employee]].show(deNiro1) should
              be("DeNiro, Robert : $30,000.00")
          implicitly[Show[Employee]].show(downeyJr) should
              be("Downey Jr, Robert : $17,000.00")
      }
  }
}
