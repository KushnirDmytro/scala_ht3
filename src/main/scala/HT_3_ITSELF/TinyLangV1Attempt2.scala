package HT_3_ITSELF

import IntSet.lec_6_IntegerSetTree.Empty

import scala.runtime.Nothing$


object TinyLangV1Attempt2 {

  var env =  Map("a" -> Number(2), "b" -> Number(3), "k" -> Bool(false))



  /**
    * Created by d1md1m on 25.05.17.
    */

  trait UnarOper extends Expr{
    def value:Any
  }

  trait ComposedOperation extends Expr{
    def lOp: Expr
    def rOp: Expr
    def sign: String


    def ComposedOperMemberReduce(l:Expr, r:Expr): Expr = this match {
      case ne:NumExpr => ne match {
        case Prod(_,_) => Prod(l, r)
        case Sum(_,_) => Sum(l, r)
      }
      case le:LogExpr => le match  {
        case Less(_,_) =>  Less(l, r)
      }
      case IfElse(cond,_,_) => IfElse(cond,l, r)
      case _ => ErrorExpr("BinOperReduceError")
    }


  } // Binary operations
  trait Conj extends ComposedOperation // Conjunctive operations
  trait Disj extends ComposedOperation // Disjunctive operations

  trait NumExpr extends Expr{
  }

  trait LogExpr extends Expr{

  }

  case class Number(value: Int) extends NumExpr with UnarOper{
  }

  case class Var(value: String) extends NumExpr with LogExpr with UnarOper {
  }

  case class Bool(value:Boolean) extends LogExpr with UnarOper{
  }

  case class ErrorExpr(value:String) extends Expr with UnarOper

  case class Prod(lOp: Expr, rOp: Expr) extends NumExpr with Conj{
    def sign = " * "
  }

  case class Sum(lOp: Expr, rOp: Expr) extends NumExpr with Disj{
    def sign = " + "
  }

  case class Less(lOp: Expr, rOp: Expr) extends LogExpr with Conj{
    def sign = " < "
  }

  case class IfElse(cond:LogExpr, lOp:Expr, rOp: Expr) extends Conj{
    def sign = " : "
  }


  trait Expr {

    def isReduciable :Boolean = this match {
      case bo:ComposedOperation=> true
      case Var(s: String) => true
      case _ => false
    }


    def ReduceToUnar:Expr = this match {
      case lg:LogExpr => lg.eval match {
        case rez:Some[Boolean] => Bool(rez.get)
        case _ => ErrorExpr("BoolCalculationFail")
      }
      case ne:NumExpr => ne.eval match {
        case rez:Some[Int] => Number(rez.get)
        case _ => ErrorExpr("NumberCalculationFail")
      }
      case IfElse(cond, l, r) => cond.reductionStep match {
        case rez:Bool => if (rez.value) l else  r
        case _ => ErrorExpr("ErrorIfElseInferrence")
      }
      case _ => println("Unknown expression" + this.show)
        ErrorExpr("Reduction fail")
    }


    def reductionStep[T<:UnarOper] : Expr = this match {
      case bo:ComposedOperation =>
        if (bo.lOp.isReduciable) bo.ComposedOperMemberReduce(bo.lOp.reductionStep, bo.rOp)
        else  if (bo.rOp.isReduciable) bo.ComposedOperMemberReduce(bo.lOp, bo.rOp.reductionStep)
        else (bo.lOp,bo.rOp) match {
          case (l:T, r:T) => this.ReduceToUnar
          case (_,_) => ErrorExpr("Type missmatch")
        }
      case Var(s: String) => if (env.contains(s)) env(s) else ErrorExpr("UndefinedVariable")
      case _ => println("ERROR: failed to infer kind of expr" + this.show)
        ErrorExpr("UndefExpr")
    }


      def eval[T]: Option[Any] = this match {

        //TODO move down
        case Var(s: String) => if (env.contains(s)) Some[Any](env(s).eval.getOrElse(None)) else None
        case co: ComposedOperation => (co, co.lOp, co.rOp) match {
          case (_, l: Var, _) => co.reductionStep.eval
          case (_, _, r: Var) => co.reductionStep.eval
          case (_, _, _) => (co, co.lOp.eval, co.rOp.eval) match {

            case (Sum(l: NumExpr, r: NumExpr), lval: Some[Int], rval: Some[Int]) =>
              Some[Int](lval.get + rval.get)
            case (Prod(l: NumExpr, r: NumExpr), lval: Some[Int], rval: Some[Int]) =>
              Some[Int](lval.get * rval.get)
            case (Less(l: NumExpr, r: NumExpr), lval: Some[Int], rval: Some[Int]) =>
              Some[Boolean](lval.get < rval.get)
            case (IfElse(cond, lOp, rOp), lval: Expr, rval: Expr) => Some[Expr](this.reductionStep) //already produces error
            case (_, _, _) => None
          }
        }
          case uo: UnarOper => uo match {
            case Number(n) => Some[Int](n)
            case Bool(b: Boolean) => Some[Boolean](b)
            case _ => None
          }
      }



      def show: String = this match {
        case un:UnarOper => un.value.toString
        case IfElse(cond, l, r) => "{ (" + cond.show + ")_?_(" + l.show + ")_:_(" + r.show + ") }"
        case dj:Disj => dj.lOp.show + dj.sign + dj.rOp.show
        case cj:Conj => (cj.lOp, cj.rOp) match  {
          case (ldj:Disj, rdj:Disj)  => "(" + cj.lOp.show + ")" + cj.sign + "(" + cj.rOp.show +")"
          case (ldj:Disj, _ )        => "(" + cj.lOp.show + ")" + cj.sign +  cj.rOp.show
          case (_ , rdj:Disj )       =>  cj.lOp.show  + cj.sign + "(" + cj.rOp.show +")"
          case (_, _ )               =>  cj.lOp.show + cj.sign +  cj.rOp.show
        }
      }

    }




  final class Machine {

    def run(expr: Expr): Expr = {
      println(expr.show)

      if (expr.isReduciable)
        run(expr.reductionStep)
      else
        expr
    }
  }




  def main(args: Array[String]): Unit = {
/*
    println(eval(Lit(-1)))
    println(eval(Succ(Lit(-1))))
    println(eval(IsZero(Succ(Lit(-1)))))
    println()


    println(Sum(Number(5), Number(4)).eval)

    println(Sum(Number(5), Number(4)).eval)

    println( Prod(Number(5),
      Sum(Var("a"), Number(-3))).show)

    println( Prod(Sum(Var("a"), Number(-3)) ,Number(5) ).show)

    println( Prod( Sum(Var("a"), Number(-3)) , Sum(Var("a"), Number(-3)) ).show)

    println( Prod(Number(5),
      Prod(Var("a"), Number(-3))).show)

    println( Prod(Number(5),
      Var("a")).show)

    println( Sum( Sum(Var("a"), Number(-3)) , Sum(Var("a"), Number(-3)) ).show)

    println( Prod(Number(5),
      Sum(Var("a"), Number(-3))).eval)

    println( Less(Number(5),
      Sum(Var("a"), Number(-3))).eval)
*/


    println(Var("a").eval)

    println(Var("Z").eval)

    println(Less(Number(2), Bool(false)).eval)
    println(IfElse(Less(Number(2), Bool(false)), Number(-1) ,  Number(-1) ).eval)

    println("ASSUME a + -3 + a + -3")
    new Machine().run( Sum( Sum(Var("a"), Number(-3)) , Sum(Var("a"), Number(-3)) ))

    println(Var("a").eval)

    println(Sum(Number(1), Number(2)).eval)

    println(Sum(Number(1), Bool(false)).eval)

    println(Sum(Var("a"), Number(2)).eval)

    println(Sum(Var("z"), Number(2)).eval)

    println(Sum(Var("k"), Number(2)).eval)


    println("ASSUME 5 * (a + -3) = " + Prod(Number(5), Sum(Var("a"), Number(-3))).eval )

    new Machine().run(Prod(Number(5),
      Sum(Var("a"), Number(-3))) )

    println("ASSUME 5 < (a + -3)")
    new Machine().run(Less(Number(5),
      Sum(Var("a"), Number(-3))) )

    println("ASSUME false < (a + -3)")
    new Machine().run(Less(Bool(false),
      Sum(Var("a"), Number(-3))) )

    println("ASSUME false < (a + -3) < 2")
    new Machine().run(Less(Less(Bool(false),
      Sum(Var("a"), Number(-3))), Number(2) ) )


    println("ASSUME false + (a + -3) < 2")
    new Machine().run(Sum(Sum(Bool(false),
      Sum(Var("a"), Number(-3))), Number(2) ) )


    new Machine().run( IfElse(Less(Number(2), Bool(false)), Number(-1) ,  Number(-1) ) )


  }

}






