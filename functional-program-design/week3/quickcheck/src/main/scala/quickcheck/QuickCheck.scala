package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- Gen.choose[Int](0, 1000)
    l <- Gen.listOfN(n, arbitrary[Int])
    h <- l.foldRight(empty)((x, h) => insert(x, h))
  } yield h

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val heap = insert(a, empty)
    findMin(heap) == a
  }

  property("min of 2 elements") = forAll { (a: Int, b: Int) =>
    val minVal = a min b
    val heap = insert(b, insert(a, empty))
    findMin(heap) == minVal
  }

  property("empty1") = forAll { a: Int =>
    val heap = insert(a, empty)
    isEmpty(deleteMin(heap))
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val minVal = findMin(h1) min findMin(h2)
    val meldedHeap = meld(h1, h2)
    val expected = findMin(meldedHeap)

    expected == minVal
  }
}
