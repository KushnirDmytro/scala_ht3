package HT_3_ITSELF

//Func oriented decomposition session

/**
  * Created by d1md1m on 25.05.17.
  */


object TinyLangV1 {

  var env =  Map("a" -> 2, "b" -> 3)



  trait Expr {


    def isReduciable :Boolean = this match {
      case bo:BinaryOper => true
      case Var(s: String) => true
      case _ => false
    }

/*
    def reductionStep : Expr = this match {
      case Sum(lOp: Expr, rOp: Expr)   => lOp.eval + rOp.eval
      case Prod(lOp: Expr, rOp: Expr) => lOp.eval * rOp.eval
      case Var(s: String) => env(s)
    }
*/

    def eval: Int = this match {
      case Number(n) => n
      case Sum(lOp: Expr, rOp: Expr) => lOp.eval + rOp.eval
      case Prod(lOp: Expr, rOp: Expr) => lOp.eval * rOp.eval
      case Var(s: String) => env(s)
    }

    def show: String = this match {
      case Number(n) => n.toString
      case Sum(lOp: Expr, rOp: Expr) => lOp.show +" + "+ rOp.show
      case Prod(lOp: Expr, rOp: Expr) => (lOp, rOp) match  {
        case (Sum(_, _), Sum(_, _))  => "(" + lOp.show + ")" + " * " + "(" + rOp.show +")"
        case (Sum(_, _), _ ) => "(" + lOp.show + ")" + " * " +  rOp.show
        case (_, Sum(_, _) ) =>  lOp.show  + " * " + "(" + rOp.show +")"
        case (_, _ ) =>  lOp.show  + " * " +   rOp.show
      }
      case Var(s: String) => s
    }

  }

  trait BinaryOper extends Expr

  case class Number(n: Int) extends Expr {
  }
  case class Bool(b: Boolean) extends Expr {
  }
  case class Var(s: String) extends Expr {
  }

  case class Less(lOp: Expr, rOp: Expr) extends Expr {
  }

  case class Prod (lOp:Expr, rOp:Expr) extends BinaryOper{}

  case class Sum (lOp:Expr, rOp:Expr) extends BinaryOper{}

  case class Error() extends Expr {
  }

  case class IfElse(IfOp: Expr, ElseOp: Expr, cond:Boolean) extends Expr {
  }

  final class Machine {

    def run(expr: Expr):Expr = {
      println(expr)

      if (expr.isReduciable)
        expr
      //    run (expr.reductionStep)
      else
        expr
    }

  }


  def main(args: Array[String]): Unit = {

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
  }

}






