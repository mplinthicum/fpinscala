package fpinscala.errorhandling

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

}
