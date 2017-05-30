package HT_3_ITSELF



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
  } // Binary operations
  trait Conj extends BinOper // Conjunctive operations
  trait Disj extends BinOper // Disjunctive operations

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


  trait Expr {

    def isReduciable :Boolean = this match {
      case bo:BinOper=> true
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
        case un:UnarOper => un.value.toString
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






