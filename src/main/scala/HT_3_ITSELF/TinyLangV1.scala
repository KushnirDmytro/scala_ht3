package HT_3_ITSELF


//Func oriented decomposition session

/**
  * Created by d1md1m on 25.05.17.
  */


object TinyLangV1 {

  var env =  Map("a" -> 2, "b" -> 3)



  trait Expr {


    def isReduciable :Boolean = this match {
      case bo:BinaryOper[Boolean] => true
      case Var(s: String) => true
      case _ => false
    }



    def reductionStep : Expr = this match {
      case bo:BinaryOper[_] =>
        if (bo.lOp.isReduciable)
          bo.sign match{
            case " * "=> Prod(bo.lOp.reductionStep, bo.rOp)
            case " + "=> Sum(bo.lOp.reductionStep, bo.rOp)
            case " < "=> Less(bo.lOp.reductionStep, bo.rOp)
          }
        else if (bo.rOp.isReduciable)
          bo.sign match{
            case " * "=> Prod(bo.lOp,bo.rOp.reductionStep)
            case " + "=> Sum(bo.lOp,bo.rOp.reductionStep)
            case " < "=> Less(bo.lOp,bo.rOp.reductionStep)
          }
        else
          Number(this.eval)

      case Var(s: String) => Number(env(s))
    }


    def eval:Int  = this match {
      case Number(n) => n
      case bo:BinaryOper[Int] => bo.oper(bo.lOp.eval,bo.rOp.eval)
      case Var(s: String) => env(s)
    }

    def show: String = this match {
      case Number(n) => n.toString
      case dj:Disj[_] => dj.lOp.show + dj.sign + dj.rOp.show
      case cj:Conj[_] => (cj.lOp, cj.rOp) match  {
        case (djl:Disj[_], djr:Disj[_])  => "(" + cj.lOp.show + ")" + cj.sign + "(" + cj.rOp.show +")"
        case (djl:Disj[_], _ ) => "(" + cj.lOp.show + ")" + cj.sign +  cj.rOp.show
        case (_, djr:Disj[_] ) =>  cj.lOp.show  + cj.sign + "(" + cj.rOp.show +")"
        case (_, _ ) =>  cj.lOp.show  + cj.sign + cj.rOp.show
      }
      case Var(s: String) => s
    }

  }

  trait BinaryOper[T] extends Expr {
    def lOp:Expr
    def rOp:Expr
    def oper(a:T, b:T):T
    def sign:String
  }

  trait Conj[T] extends BinaryOper[T]
  trait Disj[T] extends BinaryOper[T]

  trait Unar[T] extends Expr{
    def Unit:T
    def Nihil:T
  }

  case class Number(n: Int) extends Unar[Int] {
    override def Unit:Int = 1
    override def Nihil:Int = 0
  }

  case class Bool(b: Boolean) extends Unar[Boolean] {
    override def Unit:Boolean = true
    override def Nihil:Boolean = false
  }

  case class Var(s: String) extends Unar[Any] {
    override def Unit: Unit = AnyRef
    override def Nihil: Unit = None
  }

  case class Less(lOp: Expr, rOp: Expr) extends Expr {
  }

  case class Prod (lOp:Expr, rOp:Expr) extends Conj[Int]{
    def oper(a: Int, b: Int): Int = a * b
    def sign: String = " * "
  }

  case class Sum (lOp:Expr, rOp:Expr) extends Disj[Int]{
    def oper(a: Int, b: Int): Int = a + b
    def sign: String = " + "
  }


  case class Error() extends Expr {
  }

  case class IfElse(IfOp: Expr, ElseOp: Expr, cond:Boolean) extends Expr {
  }

  final class Machine {

    def run(expr: Expr):Expr = {
      println(expr.show)

      if (expr.isReduciable)
        run (expr.reductionStep)
      else
        expr
    }

  }


  def main(args: Array[String]): Unit = {
/*
    println(Sum(Number(5), Number(4)).show)

    println(Sum(Number(5), Number(4)).eval)

    println( Prod(Number(5),
      Sum(Var("a"), Number(-3))).show)

    println( Prod(Number(5),
      Sum(Var("a"), Number(-3))).eval)

    println( Prod(Sum(Var("a"), Number(-3)) ,Number(5) ).show)

    println( Prod( Sum(Var("a"), Number(-3)) , Sum(Var("a"), Number(-3)) ).show)

    println( Prod(Number(5),
      Prod(Var("a"), Number(-3))).show)

    println( Prod(Number(5),
      Var("a")).show)

    println( Sum( Sum(Var("a"), Number(-3)) , Sum(Var("a"), Number(-3)) ).show)

    println( Prod(Number(5),
      Sum(Var("a"), Number(-3))).eval)
*/

    new Machine().run( Sum( Sum(Var("a"), Number(-3)) , Sum(Var("a"), Number(-3)) ))

    new Machine().run(Prod(Number(5),
      Sum(Var("a"), Number(-3))) )
  }

}






