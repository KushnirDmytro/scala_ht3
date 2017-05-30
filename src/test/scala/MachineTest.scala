import HT_3_ITSELF.TinyLangV1.{Bool, Number, Prod, Sum}
import RationlaProps.property
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{choose, chooseNum, listOf}
import org.scalatest.prop.PropertyChecks.whenever
import practice_lec_RationalNumbers.rationalFancyOperators.Rational

/**
  * Created by d1md1m on 30.05.17.
  */
object MachineTest extends Properties("IntSetTree"){

  property("NumberConstructAndEval") = forAll { (n:Int, d:Int) =>
    Number(n).eval == n

    Number(d).show == d.toString
  }
  property("SumCalculatesOk") = forAll { (n:Int, d:Int) =>

    Sum(Number(n),Number(d)).eval == n + d

    Sum(Number(n),Number(d)).show == n.toString + " + " + d.toString

  }

  property("ProdCalculatesOk") = forAll { (n:Int, d:Int) =>

    Prod(Number(n),Number(d)).eval == n * d

    Prod(Number(n),Number(d)).show == n.toString + " * " + d.toString

  }

  /*
  property("NumberConstructAndEval") = forAll { (b:Boolean) =>
    Bool(b).eval == b

    Bool(b).eval == b
  }
  */

}
