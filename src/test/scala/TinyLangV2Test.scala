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

  property("NumberConstructAndEval") = forAll { (n:Int, d:Int) =>
    Number(n).eval(env) == n

    Number(d).show == d.toString
  }


  val env = Map("A" -> 1)
  /////////////////////////// EXPRESSIONS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  property("Number does not reduce") =
    forAll { (n:Int, d:Int) =>
        val mach = new Machine()
        val env = Map("B" -> n)
        Number(n).eval(env) == n
        mach.reduce(Number(n), env) ==  Number(n)
        Number(d).show == d.toString
    }
  property("Bool does not reduce") =
    forAll { (b:Boolean) =>
      val mach = new Machine()
      val env = Map("B" -> b)
      Bool(b).eval(env) == b
      mach.reduce(Bool(b), env) == Bool(b)
      Bool(b).show == b.toString
    }
  property("Number Var reduces to its value") =
    forAll { (n: Int, s:String) =>
      val mach = new Machine()
      val env = Map(s -> n)
      Number(n).eval(env) == n
      mach.reduce(Var(s), env) == Number(n)
      Var(s).show == s
    }

  property("Bool Var reduces to its value") =
    forAll { (b: Boolean, s:String) =>
      val mach = new Machine()
      val env = Map(s -> b)
      Bool(b).eval(env) == b
      mach.reduce(Var(s), env) == Bool(b)
      Var(s).show == s
    }
  property("Unknown Var does not reduce") =
    forAll { (k: String, s:String) =>
      if (k != s) {
        val mach = new Machine()
        val env = Map(s -> 1)
        Var(k).eval(env) == None
        mach.reduce(Var(s), env) == ErrorExpr
        Var(k).show == k
      } else true
    }

    /*
    test("Bool does not reduce") {
      test("Number Var reduces to its value") {
        test("Bool Var reduces to its value") {
          test("Unknown Var does not reduce") {
*/


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
