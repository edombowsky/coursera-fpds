package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      x <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("find min of 2") =
    
    forAll { (m1: A, m2: A) =>
      val m = if (m1 < m2) m1 else m2
      findMin(insert(m2, insert(m1, empty))) == m
    }

  property("delete the only element") =
    
    forAll { (m: A) =>
      deleteMin(insert(m, empty)) == empty
    }

  property("heap is sorted") =
    
    forAll { (h: H) =>
      def isSorted(h: H, last: A): Boolean = {
        if (isEmpty(h)) true
        else {
          val m  = findMin(h)
          if (m < last) false
          else isSorted(deleteMin(h), m)
        }
      }

      isSorted(deleteMin(h), findMin(h))
    }


  property("heap move 1 element is sorted") =
    
    forAll { (h: H) => 
      isSorted(deleteMin(h))
    }

  property("find min of 2 heap") =
    
    forAll { (h1: H, h2: H) =>
      val meldHeap = meld(h1, h2)
      val m1 = findMin(h1)
      val m2 = findMin(h2)

      findMin(meldHeap) == (if (m1 < m2) m1 else m2)
    }

  property("check move 1 element meld") =
    
    forAll { (h1: H, h2: H) =>
      val meld1 = meld(h1, h2)
      val m1 = findMin(h1)
      val meld2 = meld(deleteMin(h1), insert(m1, h2))

      isEqual(meld1, meld2)
    }


  property("check heap not lost ele") =
    
    forAll { (x: List[A]) =>
      def loopInsert(h: H, x: List[A]): H = {
        x match {
          case Nil => h
          case head::tail => loopInsert(insert(head, h), tail)
        }
      }

      val h = loopInsert(empty, x)
      getSize(h) == x.size
    }

  private def isSorted(h: H): Boolean = {
    
    if (isEmpty(h)) true
    else {
      val m  = findMin(h)
      val hNext = deleteMin(h)
      isEmpty(hNext) || (m <= findMin(hNext) && isSorted(hNext))
    }
  }

  private def isEqual(h1: H, h2: H): Boolean =
  
    if (isEmpty(h1) && isEmpty(h2)) true
    else {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      m1 == m2 && isEqual(deleteMin(h1), deleteMin(h2))
    }

  private def getSize(h: H): Int = {
    
    def getNum(h: H, num: Int): Int = {
      if (isEmpty(h)) num
      else getNum(deleteMin(h), num + 1)
    }

    getNum(h, 0)
  }
}
