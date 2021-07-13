/*
 * Copyright 2021 Daniel Hinojosa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.xyzcorp.datatypes
import cats.implicits

sealed trait DomainValidation:
  def errorMessage: String

case object UsernameHasSpecialCharacters extends DomainValidation:
  def errorMessage: String = "Username cannot contain special characters."

case object PasswordDoesNotMeetCriteria extends DomainValidation:
  def errorMessage: String =
    "Password must be at least 10 characters long, including an uppercase and a lowercase letter, one number and one special character."

case object FirstNameHasSpecialCharacters extends DomainValidation:
  def errorMessage: String = "First name cannot contain spaces, numbers or special characters."

case object LastNameHasSpecialCharacters extends DomainValidation:
  def errorMessage: String = "Last name cannot contain spaces, numbers or special characters."

case object AgeIsInvalid extends DomainValidation:
  def errorMessage: String = "You must be aged 18 and not older than 75 to use our services."
