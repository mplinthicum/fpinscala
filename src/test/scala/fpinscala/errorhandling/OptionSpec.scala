package fpinscala.errorhandling

import Option._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OptionSpec extends AnyFlatSpec with Matchers {

  "map" should "perform a function on an Option" in {
    val x = Some(2)
    x.map(_ + 2) should be(Some(4))
  }

  it should "return None when called on None" in {
    val x: Option[Int] = None
    x.map(_ + 2) should be(None)
  }

  def maybeAddTwo(x: Int): Option[Int] = Some(x + 2)

  "flatMap" should "return Some" in {
    val x = Some(2)
    x.flatMap(maybeAddTwo) should be(Some(4))
  }

  it should "return None" in {
    val x: Option[Int] = None
    x.flatMap(maybeAddTwo) should be(None)
  }

  "getOrElse" should "return the value of Some" in {
    val x = Some(2)
    x.getOrElse(1) should be(2)
  }

  it should "return the default value" in {
    val x: Option[Int] = None
    x.getOrElse(1) should be(1)
  }

  "orElse" should "return the original Some" in {
    val x = Some(2)
    x.orElse(Some(1)) should be(Some(2))
  }

  it should "return the default value" in {
    val x: Option[Int] = None
    x.orElse(Some(1)) should be(Some(1))
  }

  "filter" should "return Some if true" in {
    val x = Some(2)
    x.filter(_ % 2 == 0) should be(Some(2))
  }

  it should "return None if false" in {
    val x = Some(3)
    x.filter(_ % 2 == 0) should be(None)
  }

  it should "return None if None" in {
    val x: Option[Int] = None
    x.filter(_ % 2 == 0) should be(None)
  }

  "variance" should "return the variance of a sequence" in {
    variance(Seq(1.0, 1.0, 2.0, 2.0)) should be(Some(0.25))
  }

  it should "return None for an empty dataset" in {
    variance(Seq()) should be(None)
  }

  it should "return 0 for all equal numbers" in {
    variance(Seq(1, 1, 1, 1)) should be(Some(0.0))
  }

  it should "return 0 for a sequence of length 1" in {
    variance(Seq(1)) should be(Some(0.0))
  }

  it should "return 0 for a list of 0s" in {
    variance(Seq(0, 0, 0)) should be(Some(0.0))
  }

  def addToString(x: Int, y: Int): String = (x + y).toString

  "map2" should "lift a function" in {
    def x: Option[Int] = Some(1) // "1".toInt
    def y: Option[Int] = Some(2) // "2".toInt

    map2(x, y)(addToString) should be(Some("3"))
  }

  it should "lift a function and return none" in {
    def x: Option[Int] = Some(1) // "1".toInt
    def y: Option[Int] = None // "fish".toInt

    map2(x, y)(addToString) should be(None)
  }

  "sequence" should "transform a list of options into an optional list" in {
    sequence(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
  }

  it should "return None if one of the items is None" in {
    sequence(List(Some(1), None, Some(3))) should be(None)
  }

  def smallStringToInt(a: String): Option[Int] = a match {
    case "1" => Some(1)
    case "2" => Some(2)
    case "3" => Some(3)
    case _ => None
  }

  "traverse" should "transform a list of strings to ints" in {
    traverse(List("1", "2", "3"))(smallStringToInt) should be(Some(List(1, 2, 3)))
  }

  it should "return none" in {
    traverse(List("1", "50", "3"))(smallStringToInt) should be(None)
  }

  "sequenceViaTraverse" should "transform a list of options into an optional list" in {
    sequenceViaTraverse(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
  }

  it should "return None if one of the items is None" in {
    sequenceViaTraverse(List(Some(1), None, Some(3))) should be(None)
  }

}
