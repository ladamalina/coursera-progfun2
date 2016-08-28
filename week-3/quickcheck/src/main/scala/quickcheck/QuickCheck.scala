package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = ???
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insertTwo") = forAll { (n: Int, m: Int) =>
    val h1 = insert(n, empty)
    val h2 = insert(m, h1)
    val min = findMin(h2)
    if (n <= m)
      min == n
    else
      min == m
  }

  property("insertThree") = forAll { (n: Int, m: Int, k: Int) =>
    val h1 = insert(n, empty)
    val h2 = insert(m, h1)
    val h3 = insert(k, h2)
    findMin(h3)== List(n, m, k).min
  }

  property("insertThreeAndDelete") = forAll { (n: Int, m: Int, k: Int) =>
    val h1 = insert(n, empty)
    val h2 = insert(m, h1)
    val h3 = insert(k, h2)
    val max = List(n, m, k).max
    deleteMin(deleteMin(h3)) == insert(max, empty)
  }

  property("insertAndDelete") = forAll { n: Int =>
    val h = insert(n, empty)
    deleteMin(h) == empty
  }

  property("findMinMeld") = forAll { (n: Int, m: Int) =>
    val h1 = insert(n, empty)
    val h2 = insert(m, empty)
    val h3 = meld(h1, h2)
    val min = findMin(h3)
    if (n <= m)
      min == n
    else
      min == m
  }
}
