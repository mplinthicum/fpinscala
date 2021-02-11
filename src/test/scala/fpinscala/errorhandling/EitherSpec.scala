package fpinscala.errorhandling

import Either._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EitherSpec extends AnyFlatSpec with Matchers {

  val right: Either[String, Int] = Right(1)
  val err: Either[String, Int] = Left("error")

  "map" should "return the result of the function" in {
    right.map(_ + 2) should be(Right(3))
  }

  it should "return the left value" in {
    err.map(_ + 2) should be(Left("error"))
  }

  def maybeAddTwo(x: Int): Either[String, Int] = Right(x + 2)

  "flatMap" should "return the result of the function" in {
    right.flatMap(maybeAddTwo) should be(Right(3))
  }

  it should "return the left side" in {
    err.flatMap(maybeAddTwo) should be(Left("error"))
  }

  "orElse" should "return the right side" in {
    right.orElse(Right("fish")) should be(Right(1))
  }

  it should "return the default value" in {
    err.orElse(Right("fish")) should be(Right("fish"))
  }

  def addToString(x: Int, y: Int): String = (x + y).toString

  "map2" should "perform a function on two eithers" in {
    def x: Either[String, Int] = Right(1)
    def y: Either[String, Int] = Right(2)

    x.map2(y)(addToString) should be(Right("3"))
  }

  it should "return the left value of the error value" in {
    def x: Either[String, Int] = Left("error 1")
    def y: Either[String, Int] = Right(2)

    x.map2(y)(addToString) should be(Left("error 1"))
  }

  it should "return the left value of the second error value" in {
    def x: Either[String, Int] = Right(1)
    def y: Either[String, Int] = Left("error 2")

    x.map2(y)(addToString) should be(Left("error 2"))
  }

  it should "return the left value of the first error when both are errors" in {
    def x: Either[String, Int] = Left("error 1")
    def y: Either[String, Int] = Left("error 2")

    x.map2(y)(addToString) should be(Left("error 1"))
  }

  "sequence" should "construct a list" in {
    val list = List[Either[String, Int]](Right(1), Right(2), Right(3))
    sequence(list) should be(Right(List(1, 2, 3)))
  }

  it should "return the error" in {
    val list = List[Either[String, Int]](Right(1), Left("fish"), Right(3))
    sequence(list) should be(Left("fish"))
  }

  it should "return the first error" in {
    val list = List[Either[String, Int]](Right(1), Left("fish 1"), Left("fish 2"))
    sequence(list) should be(Left("fish 1"))
  }

  def smallStringToInt(a: String): Either[String, Int] = a match {
    case "1" => Right(1)
    case "2" => Right(2)
    case "3" => Right(3)
    case _ => Left("fish")
  }

  "traverse" should "transform a list of strings to ints" in {
    traverse(List("1", "2", "3"))(smallStringToInt) should be(Right(List(1, 2, 3)))
  }

  it should "return the error case" in {
    traverse(List("1", "fish", "3"))(smallStringToInt) should be(Left("fish"))
  }
}
