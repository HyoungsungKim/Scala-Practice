package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(this.empty),
    for
      v <- arbitrary[A]
      h <- oneOf(const(this.empty), genHeap)
    yield this.insert(v, h)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("mini1") = forAll {(a: Int) => 
    val h = insert(a, empty)
    findMin(h) == a
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("mini2") = forAll {(a: Int, b: Int) =>
    val aH = insert(a, empty)
    val bH = insert(b, aH)
    findMin(bH) == math.min(a, b)
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("mini3") = forAll {(a: Int) => 
    val h = insert(a, empty)  
    deleteMin(h) == empty
  }
  
  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("min4") = forAll {(h1: H, h2: H) =>
    val melded = meld(h1, h2)
    val h1Min = findMin(h1)
    val h2Min = findMin(h2)
    findMin(melded) == math.min(h1Min, h2Min)
  }


