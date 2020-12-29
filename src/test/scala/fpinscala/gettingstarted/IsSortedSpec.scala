package fpinscala.gettingstarted

import IsSorted._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IsSortedSpec extends AnyFlatSpec with Matchers {

  def lessThan(a: Int, b:Int): Boolean = a < b

  "isSorted" should "indicate an integer array is sorted by ascending order" in {
    val arr = Array[Int](1, 2, 3, 4)
    isSorted(arr, lessThan) should be(true)
  }

  it should "indicate an integer array is not sorted by ascending order" in {
    val arr = Array[Int](1, 3, 2, 4)
    isSorted(arr, lessThan) should be(false)
  }


  "safeIsSorted" should "indicate an integer array is sorted by ascending order" in {
    val arr = Array[Int](1, 2, 3, 4)
    safeIsSorted(arr, lessThan) should be(true)
  }

  it should "indicate an integer array is not sorted by ascending order" in {
    val arr = Array[Int](1, 3, 2, 4)
    safeIsSorted(arr, lessThan) should be(false)
  }

  it should "return true for an empty array" in {
    val arr = Array[Int]()
    safeIsSorted(arr, lessThan) should be(true)
  }

  it should "return true for an array of length one" in {
    val arr = Array[Int](1)
    safeIsSorted(arr, lessThan) should be(true)
  }
}
