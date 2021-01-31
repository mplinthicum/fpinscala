package fpinscala.datastructures

import Tree._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeSpec extends AnyFlatSpec with Matchers {
  "size" should "count the number of nodes in a tree" in {
    Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) should be(4)
  }

  "max" should "find the maximum value" in {
    max(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) should be(4)
  }

  "depth" should "return the longest path" in {
    depth(Branch(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1)), Branch(Leaf(1), Leaf(1)))) should be(4)
  }

  it should "return 0 for a leaf" in {
    depth(Leaf(1)) should be(0)
  }

  "map" should "add 1 to each value" in {
    map(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(_ + 1) should be(Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5))))
  }

  "sizeViaFold" should "count the number of nodes in a tree" in {
    sizeViaFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) should be(4)
  }

  "maxViaFold" should "find the maximum value" in {
    maxViaFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) should be(4)
  }

  "depthViaFold" should "return the longest path" in {
    depthViaFold(Branch(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1)), Branch(Leaf(1), Leaf(1)))) should be(4)
  }

  it should "return 0 for a leaf" in {
    depthViaFold(Leaf(1)) should be(0)
  }

  "mapViaFold" should "add 1 to each value" in {
    mapViaFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(_ + 1) should be(Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5))))
  }
}
