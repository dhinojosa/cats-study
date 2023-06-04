package com.xyzcorp.showexample
import cats.Show
import java.text.NumberFormat
import java.util.Locale

@main def somethingWrong():Unit =
    val deNiro1 = Employee("Robert", "DeNiro", 30000)
    val deNiro2 = Employee("Robert", "DeNiro", 30000)
    val duvall = Employee("Robert", "Duvall", 17000)
    val downeyJr = Employee("Robert", "Downey Jr", 17000)

    {
        import Employee.showLastThenFirstAndMaskedSalary
        println(summon[Show[Employee]].show(deNiro1))
        println(summon[Show[Employee]].show(downeyJr))
    }

    {
        import Employee.showLastThenFirstAndSalary
        println(summon[Show[Employee]].show(deNiro1))
        println(summon[Show[Employee]].show(downeyJr))
    }

case class Employee(firstName: String, lastName: String, salary: Int)

object Employee:
    val formatter: NumberFormat = java.text.NumberFormat.getCurrencyInstance(Locale.US)

    given showFirstNameThenLastAndSalary: Show[Employee] = new Show[Employee]:
        override def show(t: Employee): String =
            f"${t.firstName}%s ${t.lastName}%s : ${formatter.format(t.salary)}%s"

    given showLastThenFirstAndSalary: Show[Employee] = new Show[Employee]:
        override def show(t: Employee): String =
            f"${t.lastName}%s, ${t.firstName}%s : ${formatter.format(t.salary)}%s"

    given showLastThenFirstAndMaskedSalary: Show[Employee] = new Show[Employee]:
        override def show(t: Employee): String =
            val regex = """\d""".r
            val masked = regex.replaceAllIn(formatter.format(t.salary), "X")
            f"${t.lastName}%s, ${t.firstName}%s : $masked%s"
