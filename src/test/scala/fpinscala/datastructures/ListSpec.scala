package fpinscala.datastructures

import List._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFlatSpec with Matchers {

  // 3.1
  "list" should "match correctly" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    x should be(3)
  }

  // 3.2
  "tail" should "return Nil for an empty list" in {
    tail(Nil) should be(Nil)
  }

  it should "return the tail of a list" in {
    tail(List(1, 2, 3, 4)) should be(List(2, 3, 4))
  }

  it should "return Nil for a list of length one" in {
    tail(List(1)) should be(Nil)
  }

  // 3.3
  "setHead" should "return Nil for an empty list" in {
    setHead(Nil, 2) should be(Nil)
  }

  it should "return a list of length one for a list of length one" in {
    setHead(List(1), 2) should be(List(2))
  }

  it should "replace the head" in {
    setHead(List(1, 2, 3, 4), 0) should be(List(0, 2, 3, 4))
  }

  // 3.4
  "drop" should "return Nil for an empty list" in {
    drop(Nil, 2) should be(Nil)
  }

  it should "return the original list when dropping 0 items" in {
    drop(List(1, 2, 3), 0) should be(List(1, 2, 3))
  }

  it should "drop the first item in a list" in {
    drop(List(1, 2, 3), 1) should be(List(2, 3))
  }

  it should "drop items from a list" in {
    drop(List(1, 2, 3, 4, 5), 2) should be(List(3, 4, 5))
  }

  // 3.5
  "dropWhile" should "drop numbers less than 3" in {
    dropWhile(List(0, 1, 2, 3, 4))(a => a < 3) should be(List(3, 4))
  }

  it should "drop only leading items" in {
    dropWhile(List(0, 1, 2, 3, 0, 1))(a => a < 3) should be(List(3, 0, 1))
  }

  it should "clear out a full list" in {
    dropWhile(List(0, 1, 2, 3, 4))(a => a < 5) should be(Nil)
  }

  // 3.6
  "init" should "drop the last item in the list" in {
    init(List(1, 2, 3, 4)) should be(List(1, 2, 3))
  }

  "init2" should "drop the last item in the list" in {
    init2(List(1, 2, 3, 4)) should be(List(1, 2, 3))
  }

  "length" should "compute the length of a list" in {
    len(List(5, 2, 3, 4)) should be(4)
  }

  it should "return 0 for an empty list" in {
    len(Nil) should be(0)
  }

  it should "work for types other than Int" in {
    len(List("fish", "but", "like", "a", "lot", "of", "them")) should be(7)
  }

  "foldLeft" should "sum a list" in {
    foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) should be(15)
  }

  it should "return the base case for an empty list" in {
    foldLeft(Nil: List[Int], 10)(_ + _) should be(10)
  }

  it should "work on a list of length one" in {
    foldLeft(List(4), 0)(_ + _) should be(4)
  }

  "reverse" should "reverse a list" in {
    reverse(List(1, 2, 3)) should be(List(3, 2, 1))
  }

  "reverseFold" should "reverse a list" in {
    reverseFold(List(1, 2, 3)) should be(List(3, 2, 1))
  }

}
