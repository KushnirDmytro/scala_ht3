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

  val mach = new Machine()
  val env = Map("A" -> 1)





  // SUM ==============================================
  property("Sum of two Numbers reduces to Number with their sum") =
    forAll { (n:Int, k:Int) =>
        Sum(Number(n), Number(k)).eval(env) == k + n
        mach.reduce(Sum(Number(n), Number(k)), env) == Number(n+k)
      Sum(Number(n), Number(k)).show == n.toString + " + " + k.toString
      //} else true
    }

  property("Sum of Number and Bool does not reduce") =
    forAll { (n:Int, b:Boolean) =>
      Sum(Number(n), Bool(b)).eval(env) == None
      mach.reduce(Sum(Number(n), Bool(b)), env) == ErrorExpr
      Sum(Number(n), Bool(b)).show == n.toString + " + " + b.toString
      //} else true
    }

  property("Sum of Bool and Number does not reduce") =
    forAll { (n:Int, b:Boolean) =>
      Sum( Bool(b), Number(n)).eval(env) == None
      mach.reduce(Sum( Bool(b), Number(n)), env) == ErrorExpr
      Sum(Bool(b), Number(n) ).show == b.toString + " + " + n.toString
      //} else true
    }


  property("left Sum operand reduces if it is reducible and right is left unchanged") =
    forAll { (n:Int, b:Boolean, k:Int) =>
      Sum( Sum( Number(k), Number(n)), Number(n)).eval(env) == n + k + n
      mach.reductionStep(Sum( Sum( Number(k), Number(n)), Number(n)), env) == Sum(Number(n+k), Number(n))
      Sum(Sum( Number(k), Number(n)), Number(n) ).show == k.toString + " + " + n.toString + " + " + n.toString
      //} else true
    }

  property("otherwise right Sum operand reduces") =
    forAll { (n:Int, b:Boolean, k:Int) =>
      Sum( Number(n), Sum( Number(k), Number(n))).eval(env) == n + k + n
      mach.reductionStep(Sum(  Number(n), Sum( Number(k), Number(n))), env) == Sum(Number(n), Number(n+k))
      Sum( Number(n), Sum( Number(k), Number(n)) ).show == n.toString + " + " + k.toString + " + " + n.toString
      //} else true
    }




  // PROD ==============================================
  property("PROD of two Numbers reduces to Number with their sum") =
    forAll { (n:Int, k:Int) =>
      Prod(Number(n), Number(k)).eval(env) == k*n
      mach.reduce(Prod(Number(n), Number(k)), env) == Number(n*k)
      Prod(Number(n), Number(k)).show == n.toString + " * " + k.toString
      //} else true
    }

  property("PROD of Number and Bool does not reduce") =
    forAll { (n:Int, b:Boolean) =>
      Prod(Number(n), Bool(b)).eval(env) == None
      mach.reduce(Prod(Number(n), Bool(b)), env) == ErrorExpr
      Prod(Number(n), Bool(b)).show == n.toString + " * " + b.toString
      //} else true
    }

  property("PROD of Bool and Number does not reduce") =
    forAll { (n:Int, b:Boolean) =>
      Prod( Bool(b), Number(n)).eval(env) == None
      mach.reduce(Prod( Bool(b), Number(n)), env) == ErrorExpr
      Prod(Bool(b), Number(n) ).show == b.toString + " * " + n.toString
      //} else true
    }


  property("left PROD operand reduces if it is reducible and right is left unchanged") =
    forAll { (n:Int, b:Boolean, k:Int) =>
      Prod( Sum( Number(k), Number(n)), Number(n)).eval(env) == (n + k) * n
      mach.reductionStep(Sum( Sum( Number(k), Number(n)), Number(n)), env) == Prod(Number(n+k), Number(n))
      Prod(Sum( Number(k), Number(n)), Number(n) ).show == "(" + k.toString + " + " + n.toString + ")" + " * " + n.toString
      //} else true
    }

  property("otherwise right PROD operand reduces") =
    forAll { (n:Int, b:Boolean, k:Int) =>
      Prod( Number(n), Sum( Number(k), Number(n))).eval(env) == n * (k + n)
      mach.reductionStep(Prod(  Number(n), Sum( Number(k), Number(n))), env) == Prod(Number(n), Number(n+k))
      Prod( Number(n), Sum( Number(k), Number(n)) ).show == n.toString + " * " + "(" + k.toString + " + " + n.toString + ")"
      //} else true
    }


  // LESS =============================================================================
  property("Less of two Numbers reduces to Bool indicating whether first number is less than the second") =
    forAll { (n:Int, k:Int) =>
      Less(Number(n), Number(k)).eval(env) == (n < k)
      mach.reduce(Less(Number(n), Number(k)), env) == Bool(n<k)
      Less(Number(n), Number(k)).show == n.toString + " < " + k.toString
      //} else true
    }

  property("Less of Number and Bool does not reduce") =
    forAll { (n:Int, b:Boolean) =>
      Less(Number(n), Bool(b)).eval(env) == None
      mach.reduce(Less(Number(n), Bool(b)), env) == ErrorExpr
      Less(Number(n), Bool(b)).show == n.toString + " < " + b.toString
      //} else true
    }

  property("Less of Bool and Number does not reduce") =
    forAll { (n:Int, b:Boolean) =>
      Less( Bool(b), Number(n)).eval(env) == None
      mach.reduce(Less( Bool(b), Number(n)), env) == ErrorExpr
      Less(Bool(b), Number(n) ).show == b.toString + " < " + n.toString
      //} else true
    }


  property("left Less operand reduces if it is reducible and right is left unchanged") =
    forAll { (n:Int, b:Boolean, k:Int) =>
      Less( Sum( Number(k), Number(n)), Number(n)).eval(env) == ((n + k) < n)
      mach.reductionStep(Less( Sum( Number(k), Number(n)), Number(n)), env) == Less(Number(n+k), Number(n))
      Less(Sum( Number(k), Number(n)), Number(n) ).show == "(" + k.toString + " + " + n.toString + ")" + " < " + n.toString
      //} else true
    }

  property("otherwise right Less operand reduces") =
    forAll { (n:Int, b:Boolean, k:Int) =>
      Less( Number(n), Sum( Number(k), Number(n))).eval(env) == (n < (k + n))
      mach.reductionStep(Less(  Number(n), Sum( Number(k), Number(n))), env) == Less(Number(n), Number(n+k))
      Less( Number(n), Prod( Number(k), Number(n)) ).show == n.toString + " < " + "(" + k.toString + " * " + n.toString + ")"
      //} else true
    }

  // IfElse ===============================================================================
  property("IfElse reduces to thenExpr for Bool(true) and elseExpr for Bool(false)  condition") =
    forAll { (n:Int, k:Int, b: Boolean) =>
      IfElse(Bool(b), Number(k), Number(n)).eval(env).get == (if (b) k else n)
      mach.reduce( IfElse( Bool(b), Number(k), Number(n)), env) == (if (b) Number(k) else Number(n))
      IfElse( Bool(b), Number(k), Number(n)).show ==
        "{ (" + Bool(b).show + ")_?_(" + Number(k).show + ")_:_(" + Number(n).show + ") }"
      //} else true
    }


  /*

// IfElse
test("IfElse reduces to thenExpr for Bool(true) condition") {
test("IfElse reduces to elseExpr for Bool(false) condition") {
test("IfElse for Number condition does not reduce") {
test("IfElse for reducible condition reduces its condition") {

/////////////////////////// STATEMENTS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
test("DoNothing does not alter environment") {

// Assign
test("Assign adds new variable for number expression") {
test("Assign adds new variable for boolean expression") {
test("Assign updates existing variable for number expression") {
test("Assign updates existing variable for boolean expression") {
test("Assign updates existing variable for expression with the same variable") {
test("Assign does not occur for erroneous expression") {

// If
test("'If' runs thenStat if condition is Bool(true)") {
test("'If' runs elseStat if condition is Bool(false)") {
test("'If' statement fails for erroneous condition") {
test("'If' statement fails for condition expression that reduces to Number") {

// Seq
test("'Seq' does nothing if empty") {
test("'Seq' executes one its statement if contains only one") {
test("'Seq' executes its statements one by one") {
test("'Seq' does not execute remained statements after first failure") {

// While
test("'While' executes thenStat multiple times while condition reduces to Bool(true)") {
test("'While' does not execute thenStat if condition reduces to Bool(false) from the start") {
test("'While' statement fails for erroneous condition") {
test("'While' statement fails for condition expression that reduces to Number") {
test("'While' statement fails if thenStat statement fails") {

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
