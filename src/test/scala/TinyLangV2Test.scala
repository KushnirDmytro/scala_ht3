import HT_3_ITSELF.TinyLangV2._
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
object TinyLangV2Test extends Properties("V2Test"){

  val env = Map("A" -> 1)
  /////////////////////////// EXPRESSIONS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  property("Number does not reduce") =
    forAll { (n:Int, d:Int, s:String) =>
      whenever(s != "") {
        val mach = new Machine()
        val env = Map(s -> n)
        Number(n).eval(env) == n
        Number(d).show == d.toString
      }
    }
    /*
    test("Bool does not reduce") {
      test("Number Var reduces to its value") {
        test("Bool Var reduces to its value") {
          test("Unknown Var does not reduce") {
*/

    property("NumberConstructAndEval") = forAll { (n:Int, d:Int) =>
      Number(n).eval(env) == n

      Number(d).show == d.toString
    }
  property("SumCalculatesOk") = forAll { (n:Int, d:Int) =>

    Sum(Number(n),Number(d)).eval(env) == n + d

    Sum(Number(n),Number(d)).show == n.toString + " + " + d.toString

  }

  property("ProdCalculatesOk") = forAll { (n:Int, d:Int) =>

    Prod(Number(n),Number(d)).eval(env) == n * d

    Prod(Number(n),Number(d)).show == n.toString + " * " + d.toString

  }

  /*
  property("NumberConstructAndEval") = forAll { (b:Boolean) =>
    Bool(b).eval == b

    Bool(b).eval == b
  }
  */

}
