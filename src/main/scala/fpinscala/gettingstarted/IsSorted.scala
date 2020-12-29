package fpinscala.gettingstarted

import scala.annotation.tailrec

object IsSorted {

  // 2.2
  // not great for edge cases, could fix with more if conditions
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) ordered(as(n - 1), as(n))
      else if (ordered(as(n - 1), as(n))) go(n + 1)
      else false
    }
    go(1)
  }

  def safeIsSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean = n match {
      case 0 => true
      case 1 => true
      case _ if ordered(as(n - 2), as(n - 1)) => go(n - 1)
      case _ => false
    }
    go(as.length)
  }

}
