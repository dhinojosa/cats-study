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

import org.scalatest.{FunSpec, Matchers}

class ValidatedSpec extends FunSpec with Matchers {
  describe(
    """Doing Validation without the Validation type. Taken from
      |  https://typelevel.org/cats/datatypes/validated.html
    """.stripMargin) {
    it(
      """can be done by creating our own Validated types, whereas each
        |  data constructors are Valid and Invalid""".stripMargin) {
      sealed abstract class Validated[+E, +A] extends Product with Serializable

      info("Notice that if it is Valid there is no type on the left")

      final case class Valid[+A](a: A) extends Validated[Nothing, A]

      info("Notice that if it is Invalid there is no type on the right")

      final case class Invalid[+E](e: E) extends Validated[E, Nothing]

      info(
        """Now we create a form that we want to process in this
          |  case a case class each field with their
          |  own constraints""".stripMargin)

      final case class RegistrationData(username: String, password: String,
                                        firstName: String, lastName: String,
                                        age: Int)

      info(
        """We can then create a data type for all the errors
          |  that can possibly be done here""".stripMargin)

      sealed trait DomainValidation {
        def errorMessage: String
      }

      case object UsernameHasSpecialCharacters extends DomainValidation {
        def errorMessage: String = "Username cannot contain special characters."
      }

      case object PasswordDoesNotMeetCriteria extends DomainValidation {
        def errorMessage: String = "Password must be at least 10 characters long, including an uppercase and a lowercase letter, one number and one special character."
      }

      case object FirstNameHasSpecialCharacters extends DomainValidation {
        def errorMessage: String = "First name cannot contain spaces, numbers or special characters."
      }

      case object LastNameHasSpecialCharacters extends DomainValidation {
        def errorMessage: String = "Last name cannot contain spaces, numbers or special characters."
      }

      case object AgeIsInvalid extends DomainValidation {
        def errorMessage: String = "You must be aged 18 and not older than 75 to use our services."
      }

      info("Here is the proposed implementation which has problems")

      sealed trait FormValidator {
        def validateUserName(userName: String): Either[DomainValidation, String] =
          Either.cond(
            userName.matches("^[a-zA-Z0-9]+$"),
            userName,
            UsernameHasSpecialCharacters
          )

        def validatePassword(password: String): Either[DomainValidation, String] =
          Either.cond(
            password.matches(
              "(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$"),
            password,
            PasswordDoesNotMeetCriteria
          )

        def validateFirstName(firstName: String): Either[DomainValidation, String] =
          Either.cond(
            firstName.matches("^[a-zA-Z]+$"),
            firstName,
            FirstNameHasSpecialCharacters
          )

        def validateLastName(lastName: String): Either[DomainValidation, String] =
          Either.cond(
            lastName.matches("^[a-zA-Z]+$"),
            lastName,
            LastNameHasSpecialCharacters
          )

        def validateAge(age: Int): Either[DomainValidation, Int] =
          Either.cond(
            age >= 18 && age <= 75,
            age,
            AgeIsInvalid
          )

        def validateForm(username: String, password: String, firstName: String,
                         lastName: String,
                         age: Int): Either[DomainValidation, RegistrationData] = {
          for {
            validatedUserName <- validateUserName(username)
            validatedPassword <- validatePassword(password)
            validatedFirstName <- validateFirstName(firstName)
            validatedLastName <- validateLastName(lastName)
            validatedAge <- validateAge(age)
          } yield RegistrationData(validatedUserName, validatedPassword,
            validatedFirstName, validatedLastName, validatedAge)
        }
      }

      info("Create a singleton object from the FormValidator trait")

      object FormValidator extends FormValidator

      info(
        """The point to all this is that for comprehensions are fail fast or short circuited,
          |  the first sign of trouble, and the whole thing fails"""
          .stripMargin)

      val result: Either[DomainValidation, RegistrationData] = FormValidator
        .validateForm(
          username = "fakeUs3rname",
          password = "password",
          firstName = "John",
          lastName = "Doe",
          age = 15
        )

      info(
        "This will only return one of the failures, not all of them, we need all of them")

      val message = result match {
        case Left(UsernameHasSpecialCharacters) => "user name incorrect"
        case Left(AgeIsInvalid) => "age incorrect"
        case Left(PasswordDoesNotMeetCriteria) => "password incorrect"
        case Left(LastNameHasSpecialCharacters) => "last name special"
        case Left(FirstNameHasSpecialCharacters) => "first name special"
        case Right(y) => "everything correct"
      }
      message should be ("password incorrect")
    }
  }

  describe("Validated Data Type") {
    it(
      """is a datatype that is will process validations
        |  with returning hopefully helpful information
        |  if it is not valid.""".stripMargin) {
      pending
    }
  }
}
