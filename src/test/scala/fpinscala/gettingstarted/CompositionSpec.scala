package fpinscala.gettingstarted

import Composition._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompositionSpec extends AnyFlatSpec with Matchers {

  "compose" should "compose two functions" in {
    def square(a: Int): Int = a * a
    def addTen(b: Int): Int = b + 10

    // 2 * 2 + 10
    compose(addTen, square)(2) should be(14)
  }
}
