package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

    lazy val genHeap: Gen[H] = for {
        x <- arbitrary[Int]
        h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)

    implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

    property("gen") = forAll { (h: H) =>
        val m = if (isEmpty(h)) 0 else findMin(h)
        findMin(insert(m, h)) == m
    }

    property("findMin") = forAll { a: Int =>
        val h = insert(a, empty)
        findMin(h) == a
    }

    property("findMin2") = forAll { (a: Int, b: Int) =>
        val h = insert(b, insert(a, empty))
        findMin(h) == scala.math.min(a, b)
    }

    property("delMin") = forAll { a: Int =>
        val h = insert(a, empty)
        isEmpty(deleteMin(h))
    }

    property("delMin2") = forAll { (a: Int, b: Int) =>
        val h = insert(b, insert(a, empty))
        findMin(deleteMin(h)) == scala.math.max(a, b)
    }

    property("delMin3") = forAll { (a: Int, b: Int, c: Int) =>
        val h = insert(c, insert(b, insert(a, empty)))
        findMin(deleteMin(deleteMin(h))) == scala.math.max(c, scala.math.max(a, b))
    }

    property("meld") = forAll { (a: H, b: H) =>
        val min = findMin(meld(a, b))
        min == scala.math.min(findMin(a), findMin(b))
    }

    property("meld2") = forAll { a: Int =>
        val h = insert(a, empty)
        isEmpty(deleteMin(meld(h, empty)))
    }

    property("sorted") = forAll { a: H =>
        def isSorted(min: Int, h: H): Boolean =
            if (isEmpty(h))
                true
            else if(min > findMin(h))
                false
            else
                isSorted(findMin(h), deleteMin(h))

        isSorted(findMin(a), deleteMin(a))
    }

}
