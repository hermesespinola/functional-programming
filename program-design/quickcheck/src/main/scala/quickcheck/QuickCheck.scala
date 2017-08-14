package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[A]
      h <- frequency(1 -> const(empty), 9 -> genHeap)
    } yield insert(k, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("smallest of two") = forAll { (a1: A, a2: A) =>
    findMin(insert(a1, insert(a2, empty))) == math.min(a1, a2)
  }

  property("heap sort") = forAll { h: H =>
    def heapSort(h: H): List[A] =
      if (isEmpty(h)) List()
      else findMin(h)::heapSort(deleteMin(h))

    val sorted = heapSort(h)
    sorted.sorted == sorted
  }

  property("minimum of melding of two heaps returns a minimum of one or the other") = forAll { (h1: H, h2: H) =>
    val n1 = if (isEmpty(h1)) insert((math.random * 100).toInt, h1) else h1
    val n2 = if (isEmpty(h2)) insert((math.random * 100).toInt, h1) else h2
    findMin(meld(n1, n2)) == math.min(findMin(n1), findMin(n2))
  }

  property("insert two elements and extracting them should return the max at last") = forAll { (a1: A, a2: A) =>
    val h = deleteMin(insert(a1, insert(a2, empty)))
    findMin(h) == math.max(a1, a2)
  }

  property("empty heap if delete an element from a 1 element heap") = forAll { a: A =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("inserting an element in an arbitrary heap should contain that element") = forAll { (h: H, a: A) =>
    def contains(a: A, h: H): Boolean = {
      if (isEmpty(h)) false
      else {
        val m = findMin(h)
        if (m == a) true
        else contains(a, deleteMin(h))
      }
    }

    contains(a, insert(a, h))
  }
}
