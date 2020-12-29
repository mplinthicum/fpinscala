package fpinscala.gettingstarted

import scala.annotation.tailrec

object Fibonacci {

  // 2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(a: Int, b: Int, count: Int): Int = {
      if (count < n) go(b, a + b, count + 1)
      else a
    }
    go(0, 1, 0)
  }

}
