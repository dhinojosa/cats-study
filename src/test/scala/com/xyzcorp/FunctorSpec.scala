package com.xyzcorp

import cats._
import cats.implicits._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import org.scalatest.{FunSuite, Matchers}

import scala.language.postfixOps

class FunctorSpec extends FunSuite with Matchers {
  test("Functor for List") {
    val result = Functor[List].fmap(List(1, 2, 3, 4))(x => x + 30)
    result should be(List(31, 32, 33, 34))
  }

  test("Functor for Option") {
    val result = Functor[Option].fmap(Some(13))(10 *)
    result should be(Some(130))
  }

  test("Functor for Future") {
    val future = Future {40 * 10}
    val result = Functor[Future].fmap(future)(x => x / 2)
    result.foreach(println)
    Await.ready(result, 3 seconds)
  }

  case class Box[A](contents: A)

  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    override def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.contents))
  }

  test("Functor for Custom Objects") {
    val box = Box(100)
    val result = Functor[Box].fmap(box)(x => x + 100)
    result should be (Box(200))
  }
}
