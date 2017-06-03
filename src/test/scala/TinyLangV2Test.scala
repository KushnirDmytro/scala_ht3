import HT_3_ITSELF.TinyLangV2
import HT_3_ITSELF.TinyLangV2.{Assign, _}
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

  val mach = new Machine()
  val env = Map("A" -> 1)

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

  property("IfElse for Number condition does not reduce") =
    forAll { (n:Int, k:Int, b: Int) =>
      IfElse(Number(b), Number(k), Number(n)).eval(env).isEmpty
      mach.reduce( IfElse( Number(b), Number(k), Number(n)), env) == ErrorExpr
      IfElse( Number(b), Number(k), Number(n)).show ==
        "{ (" + Number(b).show + ")_?_(" + Number(k).show + ")_:_(" + Number(n).show + ") }"
      //} else true
    }

  property("IfElse for reducible condition reduces its condition") =
    forAll { ( k:Int, b: Int, n:Int) =>
      IfElse(Less(Number(b), Number(k)), Number(k), Number(n)).eval(env).get == (if(b<k) k else n)
      mach.reductionStep( IfElse(Less(Number(b), Number(k)), Number(k), Number(n)), env) == IfElse(Bool(b<k), Number(k), Number(n))
      mach.reductionStep(IfElse( Less(Number(b), Number(k)), Number(k), Number(n)), env).show ==
        "{ (" + Bool(b<k).show + ")_?_(" + Number(k).show + ")_:_(" + Number(n).show + ") }"
      //} else true
    }



  /////////////////////////// STATEMENTS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  property("DoNothing does not alter environment") =
    forAll { ( k:Int) =>
         mach.run(DoNothing(), env) == env
      DoNothing().show == "DoNothing "
    }


  // Assign ============================================================

  property("Assign adds new variable for number expression") =
    forAll { ( k:Int, n:Int, s:String) =>
      val test1 = mach.run(Assign(s, Sum(Number(k), Number(n))), env)
      val test2 = mach.run(Assign(s,Number(k)), env)
      test1.contains(s)
      test2.contains(s)
      test1(s) == n+k
      test2(s) == k
    }

  property("Assign adds new variable for boolean expression") =
    forAll { ( k:Int, n:Int, b:Boolean , s:String) =>
      val test1 = mach.run(Assign(s,Less(Number(k), Number(n))), env)
      val test2 = mach.run(Assign(s,Bool(b)), env)
      test1.contains(s)
      test2.contains(s)
      test1(s) == (k < n)
      test2(s) == b
    }


  property("Assign updates existing variable for number expression") =
    forAll { ( k:Int, n:Int, s:String) =>
      val test1 =  mach.run(Assign(s,Sum(Number(k+1), Number(n))), env)
      val test1_2 = mach.run(Assign(s,Sum(Number(k), Number(n))), test1)
      val test2 = mach.run(Assign(s,Number(k+1)), env)
      val test2_2 = mach.run(Assign(s,Number(k)), test2)
      test1_2.contains(s)
      test2_2.contains(s)
      test1_2(s) == n+k
      test1_2(s) != test1(s)
      test2_2(s) == k
      test2_2(s) != test2(s)
    }

  property("Assign updates existing variable for bool expression") =
    forAll { ( k:Int, n:Int, b:Boolean, s:String) =>
      val test1 = mach.run(Assign(s,Sum(Number(k+1), Number(n))), env)
      val test1_2 = mach.run(Assign(s,Less(Number(k), Number(n))), test1)
      val test2 = mach.run(Assign(s,Number(k+1)), env)
      val test2_2 = mach.run(Assign(s,Bool(b)), test2)
      test1_2.contains(s)
      test2_2.contains(s)
      test1_2(s) == b
      test1_2(s) != n+k+1
      test2_2(s) == b
      test2_2(s) != k+1
    }
  property("Assign updates existing variable for expression with the same variable") =
    forAll { ( k:Int, n:Int, b:Boolean, s:String) =>
      val test1 = mach.run(Assign(s,Sum(Number(k+1), Number(n))), env)
      val test1_2 = mach.run(Assign(s, Var(s)), test1)
      test1_2.contains(s)
      test1_2(s) == s
      !test1_2.equals(test1)
    }
  property("Assign does not occur for erroneous expression") =
    forAll { ( k:Int, n:Int, b:Boolean, s:String) =>
      val test1 = mach.run(Assign(s,Less(Number(k+1), Bool(b))), env)
      val test2 = mach.run(Assign(s,Number(k)), env)
      val test2_2 = mach.run(Assign(s, Less(Number(n), Bool(b))), test2)
      test1 == env
      test2.contains(s)
      test2_2(s) == test2(s)
    }


  // If =======================================================
  property("'If' runs thenStat if condition is Bool(true)") =
    forAll { ( k:Int, s:String) =>
      if (k < 2147483647) {
        val test1 = mach.run(
        If (Bool(true),
          Assign(s, Number(k)),
          Assign(s, Number(k+1))),
      env)
      val test2 = mach.run(
        If (Less(Number(k), Number(k+1)),
          Assign(s, Number(k)),
          Assign(s, Number(k+1))),
        env)
      test1.contains(s)
      test2.contains(s)
      test1(s) == k
      test1(s) != k+1
      test2(s) == k
      test2(s) != k+1
      }
      else true
    }

  property("'If' runs elseStat if condition is Bool(false)") =
    forAll { ( k:Int, s:String) =>
      if (k < 2147483647) {
      val test1 = mach.run(
        If (Bool(false),
          Assign(s, Number(k)),
          Assign(s, Number(k+1))),
        env)
      val test2 = mach.run(
        If (Less(Number(k+1), Number(k)),
          Assign(s, Number(k)),
          Assign(s, Number(k+1))),
        env)
      test1.contains(s)
      test2.contains(s)
      test1(s) != k
      test1(s) == k+1
      test2(s) != k
      test2(s) == k+1
      }
      else true
    }

  property("'If' statement fails for erroneous condition") =
    forAll { ( k:Int, s:String) =>
      val test1 = mach.run(
        If (ErrorExpr("testError"),
          Assign(s, Number(k)),
          Assign(s, Number(k+1))),
        env)
      val test2 = mach.run(
        If (Less(Bool(false), Number(k)),
          Assign(s, Number(k)),
          Assign(s, Number(k+1))),
        env)
      !test1.contains(s)
      !test2.contains(s)
      test1.contains("__error")
      test2.contains("__error")
      }
  property("'If' statement fails for condition that reduces to  Number") =
    forAll { ( n:Int, k:Int, s:String) =>
        val test1 = mach.run(
          If(Sum(Number(n), Number(k)),
            Assign(s, Number(k)),
            Assign(s, Number(k + 1))),
          env)
        val test2 = mach.run(
          If(Number(k),
            Assign(s, Number(k)),
            Assign(s, Number(k + 1))),
          env)
        !test1.contains(s)
        !test2.contains(s)
        test1.contains("__error")
        test2.contains("__error")
    }

  // Seq ================================================================
  property("'Seq' does nothing if empty") =
    forAll { ( n:Int, k:Int, s:String) =>
        val test1 = mach.run(
          Seq(
          ),
          env)
        !test1.contains(s)
        !test1.contains("__error")
    }

  property("'Seq' executes one its statement if contains only one") =
      forAll { (k: Int, s: String) =>{
        if (k < 2147483647) {
          val test1 = mach.run(
            Seq(
              Assign(s, Number(k))
            ),
            env)
          val test2 = mach.run(
            Seq(
              Assign(s, Sum(Number(1), Number(k))
              )),
            env)
          test1.contains(s)
          test2.contains(s)
          test1(s) == k
          test2(s) == k + 1
        } else true
        }
      }

        property("'Seq' executes its statements one by one") =
          forAll { (n: Int, k: Int, s: String, s2: String) =>
            if (k < 2147483647) {
              val test1 = mach.run(
                Seq(
                  Assign(s, Number(k)),
                  Assign(s, Sum(Var(s), Number(1)))
                ),
                env)
             // test1.contains(s)
              test1.contains(s)
              //test1(s) == k
              test1(s) == k + 1

            val test2 = mach.run(
              Seq(
                Assign(s, Number(k)),
                Assign(s2, Sum(Var(s), Number(1)))
              ),
              env)
            test2.contains(s)
            test2.contains(s2)
            test2(s) == k
            test2(s2) == k + 1
          }  else true
          }

            property("'Seq' does not execute remained statements after first failure") =
              forAll { (n: Int, k: Int, s: String, s2: String, er: String) =>
                if (k < 2147483647) {
                  val test1 = mach.run(
                    Seq(
                      Assign(s, Number(k)),
                      Assign(er, ErrorExpr("testErrot")),
                      Assign(s2, Sum(Var(s), Number(1)))
                    ),
                    env)
                  test1.contains(s)
                  test1.contains("__error")
                  !test1.contains(s2)
                  test1(s) == k
                } else true
              }

  // While ===========================================

  property("'Seq' executes its statements one by one") =
    forAll { (n: Int, k: Int, s: String, s2: String) =>
      if (k < 2147483647) {
        val test1 = mach.run(
          Seq(
            Assign(s, Number(k)),
            Assign(s, Sum(Var(s), Number(1)))
          ),
          env)
        // test1.contains(s)
        test1.contains(s)
        //test1(s) == k
        test1(s) == k + 1

        val test2 = mach.run(
          Seq(
            Assign(s, Number(k)),
            Assign(s2, Sum(Var(s), Number(1)))
          ),
          env)
        test2.contains(s)
        test2.contains(s2)
        test2(s) == k
        test2(s2) == k + 1
      }  else true
    }

  property("'While' executes thenStat multiple times while condition reduces to Bool(true)") =
    forAll { (n: Int, checker: String, decrement: String) =>
      if ((n > 0) && (n < 200) && (checker!=decrement)) {
        val preperationEnv = mach.run(Assign(decrement, Number(n)), env)
        val testEnv = mach.run(Assign(checker, Number(0)), preperationEnv)
        val test1 = mach.run(
          While( Less(Number(0), Var(decrement))
            ,
            Seq(
              Assign(decrement, Sum( Var(decrement), Number(-1) )), //decrementation
              Assign(checker, Sum( Var(checker), Number(1) ))
          )
          ),
          testEnv)
        test1.contains(checker)
        test1.contains(decrement)
        !test1.contains("__error")
        test1(checker) == n
        test1(decrement) == 0
      } else true
    }


/*
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
