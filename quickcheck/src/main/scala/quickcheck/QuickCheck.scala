package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (heap: H) =>
    val min = if (isEmpty(heap)) 0 else findMin(heap)
    findMin(insert(min, heap)) == min
  }

  /**
   * If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the
   * smallest of the two elements back.
   */
  property("insert two and get minimum") = forAll { (a: Int, b: Int) =>
    val heap = insert(b, insert(a, empty))

    if (a > b) findMin(heap) == b
    else findMin(heap) == a
  }

  /**
   * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
   */
  property("insert and delete minimum") = forAll { i: Int =>
    isEmpty(deleteMin(insert(i, empty)))
  }

  /**
   * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
   * (Hint: recursion and helper functions are your friends.)
   */
  property("sorted sequence") = forAll { (heap: H) =>
    def pop(heap: H, last: Int): Boolean = {
      val min = findMin(heap)
      val heap2 = deleteMin(heap)
      last <= min && (isEmpty(heap2) || pop(heap2, min))
    }

    isEmpty(heap) || pop(heap, Int.MinValue)
  }

  /**
   * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
   */
  property("min meld") = forAll { (heap1: H, heap2: H) =>
    var melded = meld(heap1, heap2)
    val min = findMin(melded)
    min == findMin(heap1) || min == findMin(heap2)
  }

  property("insert a list and check") = forAll { list: List[Int] =>
    def pop(heap: H, list: List[Int]): Boolean = {
      if (isEmpty(heap)) {
        list.isEmpty
      } else {
        !list.isEmpty && findMin(heap) == list.head && pop(deleteMin(heap), list.tail)
      }
    }
    val sortedList = list.sorted
    val heap = list.foldLeft(empty)((heap, a) => insert(a, heap))
    pop(heap, sortedList)
  }

}
