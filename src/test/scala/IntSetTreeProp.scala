import lec_6_IntegerSetTree.{Empty, IntSet, NonEmpty}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{choose, chooseNum, listOf}
import org.scalatest.prop.PropertyChecks.whenever

object IntSetTreeProp extends Properties("IntSetTree") {


  def height(set : IntSet): Int = set match {
    case Empty =>
      0
    case NonEmpty (_, left, right) =>
      1 + Math.max(height(left), height(right))
  }


  def size(set : IntSet): Int = set match {
    case Empty =>
      0
    case NonEmpty (_, left, right) =>
      1 + size(left) + size(right)
  }

  def appendSetViaIncludingIntSequence(set:IntSet, size: Int): IntSet={
    if (size > 0)
      appendSetViaIncludingIntSequence(
        set include choose(-20, 20).sample.get,
        size -1)
    else
      set
  }

  def generateSet(size:Int): IntSet ={
    appendSetViaIncludingIntSequence(
      NonEmpty(choose(-20, 20).sample.get, Empty, Empty),
      choose(1, 7).sample.get )
  }

/*
  property("positive_height") =
    forAll { (n:Int, d:Int) => whenever( n < 2147483627)(Boolean) {
      assert( (size(generateSet(choose(n, n + 10).sample.get)) >= 0) == true)
    }
  }
*/


  /*
  property("concatenate") = forAll { (a: String, b: String) =>
    (a+b).length > a.length && (a+b).length > b.length
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }

*/
}