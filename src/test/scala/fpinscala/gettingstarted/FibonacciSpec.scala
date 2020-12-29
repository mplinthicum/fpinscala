package fpinscala.gettingstarted

import Fibonacci._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FibonacciSpec extends AnyFlatSpec with Matchers {

  "fib" should "return 0 for fib(0)" in {
    fib(0) should be(0)
  }

  "fib" should "return 1 for fib(1)" in {
    fib(1) should be(1)
  }

  "fib" should "return 1 for fib(2)" in {
    fib(2) should be(1)
  }

  "fib" should "return 2 for fib(3)" in {
    fib(3) should be(2)
  }

  "fib" should "return 3 for fib(4)" in {
    fib(4) should be(3)
  }

  "fib" should "return 5 for fib(5)" in {
    fib(5) should be(5)
  }

  "fib" should "return 6765 for fib(20)" in {
    fib(20) should be(6765)
  }
}
