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

import org.scalatest.*
import matchers.should.*
import org.scalatest.funspec.AnyFunSpec

class ValidatedSpec extends AnyFunSpec with Matchers:
    describe(
        """Doing Validation without the Validation type. Taken from
          |  https://typelevel.org/cats/datatypes/validated.html
    """.stripMargin) {
        it(
            """can be done by creating our own Validated types, whereas each
              |  data constructors are Valid and Invalid""".stripMargin) {

            info(
                """Now we create a form that we want to process in this
                  |  case a case class each field with their
                  |  own constraints""".stripMargin)

            info("Here is the proposed implementation which has problems")

            info(
                """The point to all this is that for comprehensions are fail fast or short circuited,
                  |  the first sign of trouble, and the whole thing fails""".stripMargin)

            val result: Either[DomainValidation, RegistrationData] = FormValidator
                .validateForm(
                    username = "fakeUs3rname",
                    password = "password",
                    firstName = "John",
                    lastName = "Doe",
                    age = 15
                )

            info("This will only return one of the failures, not all of them, we need all of them")

            val message = result match
                case Left(UsernameHasSpecialCharacters) => "user name incorrect"
                case Left(AgeIsInvalid) => "age incorrect"
                case Left(PasswordDoesNotMeetCriteria) => "password incorrect"
                case Left(LastNameHasSpecialCharacters) => "last name special"
                case Left(FirstNameHasSpecialCharacters) => "first name special"
                case Right(y) => "everything correct"
            message should be("password incorrect")
        }
    }

    describe("Validated with use of a Chain or Non-Empty-Chain or Nec") {
        it(
            """can be moved towards a validated type but using an iteration to show
              |  all the error messages, the problem is that Validated is
              |  not a Monad, and therefore cannot be flatMapped nor can
              |  it be placed inside of a for-comprehension""".stripMargin) {

            info("First we import what is needed")

            import cats.data.Validated
            import cats.{data, implicits}

            info("Here we have the case class that is to be filled")

            info("We then define our Validation objects again from the previous example")

            info(
                """Creating FormValidatorNec.  A Nec is a non-empty chain.
                  |  A chain is a collection much like a list but with better
                  |  performance in certain situations. See
                  |  com.xyzcorp.datatypes.ValidatedSpec and
                  |  https://typelevel.org/cats/datatypes/chain.html. A non-empty-chain
                  |  will ensure that we have at least one element""".stripMargin)


            info("Now we can run our validation")
            val result: FormValidatorNec.ValidationResult[RegistrationData] = FormValidatorNec
                .validateForm(
                    username = "Joe%%%",
                    password = "password",
                    firstName = "John",
                    lastName = "Doe",
                    age = 21
                )
            result.isInvalid should be(true)
        }
    }
