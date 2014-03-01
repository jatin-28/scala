package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import org.scalacheck.Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- frequency((1, value(empty)), (9, genHeap))
  } yield insert(a,h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def heapToList( h:H) : List[Int] = {
    if( isEmpty(h)) List() else findMin(h) :: heapToList(deleteMin(h))
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minBetween2") = forAll { (a: Int, b:Int) =>
    val h = insert(a, empty)
    val s = insert(b, h)
    if(a < b) findMin(s) == a else findMin(s) == b
  }

  property("deleteMin") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("deleteMinOrder") = forAll { (h1:H, h2:H) =>
    heapToList(meld(h1,h2)) == heapToList(meld(deleteMin(h1),insert(findMin(h1), h2)))
  }

  property("minOfTwoMeldedHeaps") = forAll { (a: Int, b:Int) =>
    val h = insert(a, empty)
    val h2 = insert( { if (a < Int.MaxValue) a + 1 else a }, h)

    val s = insert(b, empty)
    val s2 = insert({ if (b < Int.MaxValue) b + 1 else b }, s)

    val melded = meld(h2,s2)

    if(a < b) findMin(melded) == a else findMin(melded) == b
  }

  property("delMinOfTwoMeldedHeaps") = forAll { (h1 : H, h2 : H) =>
    def checkOrder(h: H): Boolean = {
      if(isEmpty(h)) true
      else if( isEmpty(deleteMin(h))) true
      else {
        if( findMin(h) > findMin(deleteMin(h)))
          false
        else
          checkOrder(deleteMin(h))
      }
    }

    checkOrder(meld(h1,h2))
  }

 property("insert and delete between numbers") = {
   forAll { (a: Int, b:Int) =>
     val s = insert(b, insert(a, empty))
     val t = findMin(deleteMin(s))

     if(a < b)  t == b else t == a
   }
  }

  property("check ordering") = forAll { (h: H) =>
    def checkOrder(h: H): Boolean = {
      if(isEmpty(h)) true
      else if( isEmpty(deleteMin(h))) true
      else {
        if( findMin(h) > findMin(deleteMin(h)))
          false
        else
          checkOrder(deleteMin(h))
      }
    }

    checkOrder(deleteMin(h))
  }
}
