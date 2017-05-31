package HT_3_ITSELF

import IntSet.lec_6_IntegerSetTree.Empty

import scala.runtime.Nothing$


object TinyLangV1Attempt2 {

  var env =  Map("a" -> Number(2), "b" -> Number(3))



  /**
    * Created by d1md1m on 25.05.17.
    */

  trait UnarOper extends Expr{
    def value:Any
  }

  trait BinOper extends Expr{
    def lOp: Expr
    def rOp: Expr
    def sign: String


    def BinOpMemberReduce(l:Expr, r:Expr): Expr = this match {
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


    def chooseReduction[T<:UnarOper](l: Expr, r: Expr):Expr = {
        if (l.isReduciable) this.BinOpMemberReduce(l.reductionStep, r)
        else  if (r.isReduciable) this.BinOpMemberReduce(l, r.reductionStep)
        else (l,r) match {
          case (l:T, r:T) => this.reduceByLevel
          case (_,_) => ErrorExpr("Type missmatch")
        }
    }

    def reduceByLevel:Expr = this match {
      case lg:LogExpr => Bool(lg.eval.get)
      case ne:NumExpr => Number(ne.eval) // TODO OPTIONAL RETURN HANDLING
      case IfElse(cond, l, r) => cond.reductionStep match {
        case rez:Bool => if (rez.value) l else  r
        case _ => ErrorExpr("ErrorIfElseInferrence")
      }
      case _ => println("Unknown expression" + this.show)
        ErrorExpr("Reduction fail")
    }

  } // Binary operations
  trait Conj extends BinOper // Conjunctive operations
  trait Disj extends BinOper // Disjunctive operations

  trait NumExpr extends Expr{
    override def eval: Int = this match {
      case Number(n) => n
      case Sum(lOp: NumExpr, rOp: NumExpr) => lOp.eval + rOp.eval
      case Prod(lOp: NumExpr, rOp: NumExpr) => lOp.eval * rOp.eval
      case _ => new Machine().run(this) match {
        case Number(n:Int) => n
        case _ => throw new Exception("Unexpected type (NOT INT) " + this.show )
      }
    }
  }

  trait LogExpr extends Expr{
    override def eval: Option[Boolean] = this match {
      case Less(lOp: NumExpr, rOp: NumExpr) => Some[Boolean](lOp.eval < rOp.eval)
      case Bool(b:Boolean) => Some[Boolean](b)
      case _ => new Machine().run(this) match {
        case Bool(b:Boolean) => Some[Boolean](b)
        case _ => None //throw new Exception("Unexpected type (NOT BOOL) " + this.show )
      }
    }
  }

  case class Number(value: Int) extends NumExpr with UnarOper{
  }

  case class Var(value: String) extends Expr with UnarOper{
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
      case bo:BinOper=> true
      case Var(s: String) => true
      case _ => false
    }



    def reductionStep : Expr = this match {

      case bo:BinOper => bo.chooseReduction(bo.lOp, bo.rOp)
      case Var(s: String) => env(s)
      case _ => println("ERROR: failed to infer kind of expr" + this.show)
        ErrorExpr("UndefExpr")
    }


      def eval: Any = this match {
        case IfElse(cond, lOp, rOp) => this.reductionStep
        case Var(s: String) => env(s)
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

    println("ASSUME a + -3 + a + -3")
    new Machine().run( Sum( Sum(Var("a"), Number(-3)) , Sum(Var("a"), Number(-3)) ))


    println("ASSUME 5 * (a + -3)")
    new Machine().run(Prod(Number(5),
      Sum(Var("a"), Number(-3))) )

    println("ASSUME 5 < (a + -3)")
    new Machine().run(Less(Number(5),
      Sum(Var("a"), Number(-3))) )

    println("ASSUME false < (a + -3)")
    new Machine().run(Less(Bool(false),
      Sum(Var("a"), Number(-3))) )

    new Machine().run(Less(Less(Bool(false),
      Sum(Var("a"), Number(-3))), Number(2) ) )

    new Machine().run( IfElse(Less(Number(2), Bool(false)), Number(-1) ,  Number(-1) ) )


  }

}






