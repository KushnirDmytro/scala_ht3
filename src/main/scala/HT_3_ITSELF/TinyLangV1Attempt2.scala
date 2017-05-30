package HT_3_ITSELF



object TinyLangV1Attempt2 {

  var env =  Map("a" -> Number(2), "b" -> Number(3))



  /**
    * Created by d1md1m on 25.05.17.
    */

  trait NumExpr extends Expr{
    override def eval: Int = this match {
      case Number(n) => n
      case Sum(lOp: NumExpr, rOp: NumExpr) => lOp.eval + rOp.eval
      case Prod(lOp: NumExpr, rOp: NumExpr) => lOp.eval * rOp.eval
    }
  }

  trait LogExpr extends Expr{
    override def eval: Boolean = this match {
      case Less(lOp: NumExpr, rOp: NumExpr) => lOp.eval < rOp.eval
      case Bool(b:Boolean) => b
    }
  }

  case class Number(n: Int) extends NumExpr {
  }

  case class Var(s: String) extends Expr {
  }

  case class Bool(b:Boolean) extends LogExpr{
  }

  case class Prod(lOp: Expr, rOp: Expr) extends NumExpr {
  }

  case class Sum(lOp: Expr, rOp: Expr) extends NumExpr {
  }

  case class Less(lOp: Expr, rOp: Expr) extends LogExpr {
  }

  case class ErrorExpr(msg:String) extends Expr

  trait Expr {

    def isReduciable :Boolean = this match {
      case Prod(_,_) => true
      case Sum(_,_) => true
      case Less(_,_) => true
      case Var(s: String) => true
      case _ => false
    }



    def reductionStep : Expr = this match {
      case ne:NumExpr=> ne match {
        case Prod(lOp: Expr, rOp: Expr) =>
          if (lOp.isReduciable)  Prod(lOp.reductionStep, rOp)
          else if (rOp.isReduciable ) Prod(lOp,rOp.reductionStep)
          else Number(ne.eval)
        case Sum(lOp: Expr, rOp: Expr) =>
          if (lOp.isReduciable)  Sum(lOp.reductionStep, rOp)
          else if (rOp.isReduciable ) Sum(lOp,rOp.reductionStep)
          else Number(ne.eval)

      }
      case le:LogExpr=> le match {
        case Less(lOp: Expr, rOp: Expr) =>
          if (lOp.isReduciable)  Less(lOp.reductionStep, rOp)
        else if (rOp.isReduciable ) Less(lOp,rOp.reductionStep)
        else (lOp, rOp) match {
            case (lOp:NumExpr, rOp:NumExpr) => Bool(le.eval)
            case _ =>
              println("LogicalExpr [" +  le.show + "] failed to simplify")
              ErrorExpr("Err[" + le.show + "]")
          }


      }
      case Var(s: String) => env(s)
    }



      def eval: Any = this match {
        case Var(s: String) => env(s)
      }


      def show: String = this match {
        case Number(n) => n.toString
        case Bool(n) => n.toString
        case Sum(lOp: Expr, rOp: Expr) => lOp.show +" + "+ rOp.show
        case Less(lOp: Expr, rOp: Expr) => lOp.show +" < "+ rOp.show
        case ErrorExpr(msg:String) => msg
        case Prod(lOp: Expr, rOp: Expr) => (lOp, rOp) match  {
          case (Sum(_, _), Sum(_, _))  => "(" + lOp.show + ")" + " * " + "(" + rOp.show +")"
          case (Sum(_, _), _ ) => "(" + lOp.show + ")" + " * " +  rOp.show
          case (_, Sum(_, _) ) =>  lOp.show  + " * " + "(" + rOp.show +")"
          case (_, _ ) =>  lOp.show  + " * " +   rOp.show
        }
        case Var(s: String) => s
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







  trait Term[T]
  case class Lit(x: Int) extends Term[Int]
  case class Succ(t: Term[Int]) extends Term[Int]
  case class IsZero(t: Term[Int]) extends Term[Boolean]
  case class If[T](c: Term[Boolean],
                   t1: Term[T],
                   t2: Term[T]) extends Term[T]

  def eval[T](t: Term[T]): T = t match {
    case Lit(n)        => n
    case Succ(u)       => eval(u) + 1
    case IsZero(u)     => eval(u) == 0
    case If(c, u1, u2) => eval(if (eval(c)) u1 else u2)
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

    new Machine().run( Sum( Sum(Var("a"), Number(-3)) , Sum(Var("a"), Number(-3)) ))

    new Machine().run(Prod(Number(5),
      Sum(Var("a"), Number(-3))) )

    new Machine().run(Less(Number(5),
      Sum(Var("a"), Number(-3))) )

    new Machine().run(Less(Bool(false),
      Sum(Var("a"), Number(-3))) )


    new Machine().run(Less(Less(Bool(false),
      Sum(Var("a"), Number(-3))), Number(2) ) )


  }


}






