package HT_3_ITSELF


object TinyLangV2_FINAL_ {

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

  trait NumExpr extends Expr
  trait LogExpr extends Expr

  case class Number(value: Int) extends NumExpr with UnarOper
  case class Var(value: String) extends NumExpr with LogExpr with UnarOper
  case class Bool(value:Boolean) extends LogExpr with UnarOper
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
  case class IfElse(cond:Expr, lOp:Expr, rOp: Expr) extends Conj{
    def sign = " : "
  }


  trait Expr {

    def isReduciable :Boolean = this match {
      case DoNothing() => false
      case e:END => false
      case st:Stat => true
      case bo:ComposedOperation=> true
      case Var(s: String) => true
      case _ => false
    }

    val mat = new Machine()

    def eval[T](env:Map[String, Any]): Option[Any] = this match {
      case Var(s: String) => if (env.contains(s))
        if (env(s) == s) {
          println("LoopInVarReduction")
          None
        }
        else Some[Any](env(s))
      else None
      case IfElse(cond:Expr, lOp, rOp) => cond.eval(env) match {
        case (sm:Some[Any]) => sm.get match {
          case rez:Boolean => if (rez) lOp.eval(env) else rOp.eval(env)
          case _ => None
        }
        case (_) => None
      }
      case co: ComposedOperation => (co, co.lOp, co.rOp) match {
        case (_, l: Var, _) => mat.reductionStep(co, env).eval(env)
        case (_, _, r: Var) => mat.reductionStep(co, env).eval(env)
        case (_,_,_) => (co.lOp.eval(env), co.rOp.eval(env)) match {
          case (l:Some[Any], r:Some[Any]) => (co, l.get, r.get) match {
            case (Sum(l: NumExpr, r: NumExpr), lval: Int, rval: Int) =>
              Some[Int](lval + rval)
            case (Prod(l: NumExpr, r: NumExpr), lval: Int, rval: Int) =>
              Some[Int](lval * rval)
            case (Less(l: NumExpr, r: NumExpr), lval: Int, rval: Int) =>
              Some[Boolean](lval < rval)
            case (_, _, _) => None
          }
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
        case cj:Conj => (cj, cj.lOp, cj.rOp) match  {
          case (lg:LogExpr, l:UnarOper, r:UnarOper)  => cj.lOp.show  + cj.sign  + cj.rOp.show
          case (lg:LogExpr, l:UnarOper, _)  =>  cj.lOp.show + cj.sign + "(" + cj.rOp.show +")"
          case (lg:LogExpr, _, r:UnarOper)  => "(" + cj.lOp.show + ")" + cj.sign +  cj.rOp.show
          case (lg:LogExpr, _, _)  => "(" + cj.lOp.show + ")" + cj.sign + "(" + cj.rOp.show +")"

          case (_, ldj:Disj, rdj:Disj)  => "(" + cj.lOp.show + ")" + cj.sign + "(" + cj.rOp.show +")"
          case (_,ldj:Disj, _ )        => "(" + cj.lOp.show + ")" + cj.sign +  cj.rOp.show
          case (_,_ , rdj:Disj )       =>  cj.lOp.show  + cj.sign + "(" + cj.rOp.show +")"
          case (_,_, _ )               =>  cj.lOp.show + cj.sign +  cj.rOp.show
        }
        case (st:Stat) => st match{
          case DoNothing() => "DoNothing "
        }
        case _ => ErrorExpr("Unknown_to_string transition [" + this.toString + "]").show
      }

  }



  // ===========================STATS=============================

  trait Stat extends Expr

  case class DoNothing() extends Stat

  case class END(expr: Any = None) extends Stat

  case class Assign(key:String, expr:Expr) extends Stat

  case class If (Cond:Expr, thenStat:Expr, elseStat:Expr) extends Stat

  case class Seq(DoSeq:Stat*) extends Stat

  case class While(Cond:Expr, Doseq:Seq)extends Stat

  // ===========================STATS=============================


  final class Machine {


    def returnEnv(env:Map[String, Any]): Map[String, Any] ={
      if (env.contains("OldState"))
        env- "OldState"
      else env
    }

    def run(stat: Stat, OldEnv:Map[String, Any]): Map[String, Any] = {
      //println(stat.show)
     if (stat.isReduciable){

        (stat,OldEnv + ("OldState" -> stat.toString)) match {
          case (_, newEnv) if newEnv.contains("__error") => newEnv //ALLREADY WITTH ERROR
          case (expr: END, newEnv) => newEnv
          case (expr, newEnv)
            if OldEnv.contains("OldState") && (OldEnv("OldState") == stat.toString)
          => newEnv + ("__error" -> "LiveLoopDangerTermination")

          case (st: Stat, _) => (st, OldEnv) match {

            case (While(cond: Expr, thenStat: Seq), e) => cond.eval(e) match {
              case rez: Some[Any] => rez.get match {
                case r: Boolean => if (r)
                   run( stat, e ++ run(thenStat, e))
                else returnEnv(e)
                case _ => returnEnv(e) + ("__error" -> "ConditionSeemsNOTBOOL")
              }
              case _ => returnEnv(e) + ("__error" -> "WhileCondError")
            }

            case (Seq(head, tail@_*), e) => (head, tail) match {
              case (h: Stat, _) =>
                run(Seq(tail: _*), returnEnv(e) ++ run(head, e))
              case (h: Stat) => returnEnv(e) ++ run(head, e)
            }
            case (Seq(), e) => /*Assume empty*/ returnEnv(e)


            case (If(cond, thenStat: Stat, elseStat: Stat), e) =>
              cond.eval(e) match {
                case s: Some[Any] => s.get match {
                  case (b: Boolean) => if (b)
                    run(thenStat, e)
                  else
                    run(elseStat, e)
                  case _ =>  returnEnv(e) + ("__error" -> "IfElseConditionReducitonWrongType")
                }
                case _ => returnEnv(e) + ("__error" -> "IfElseConditionReducitonToNone")
              }


            case (Assign(s: String, v: Expr), e) =>
              (s, v) match {
                case (_, v: Stat) => returnEnv(e) + (s -> v) //"funcion" assignment. Let it be
                case (_, Var(key: String)) => returnEnv(e) + (s -> key)
                case (_, v: Expr) => v.eval(e) match {
                  case rez: Some[Any] => returnEnv(e) + (s -> rez.get)
                  case _ => returnEnv(e) + ("__error" -> "AssignmentOfErrorAttempt")
                }
                case (_, _) => returnEnv(e) + ("__error" -> "UnknownValueToAssign")
              }
          }
        }
        }
      else returnEnv(OldEnv)
    }

    def ReduceToUnar(expr: Expr, env:Map[String, Any]):Expr = expr.eval(env) match {
      case rez:Some[Any] => rez.get match {
        case n:Int => Number(n)
        case b:Boolean => Bool(b)
        case _=> ErrorExpr("UndefinedReturnType")
      }
      case _ => ErrorExpr("ReductionResultInNONE")
    }


    def reductionStep[T<:UnarOper](expr: Expr, env:Map[String, Any]) : Expr = expr match {
      case IfElse(cond:LogExpr, l, r) => cond match {
        //not in reduce toUnar to create "Call By Name" style
          // If "CBV" => replace there
        case cd:Bool => if (cd.value) l else  r
        case _ if cond.isReduciable =>  IfElse( reductionStep(cond, env), l ,r)
        case _ => ErrorExpr("ErrorIfElseInferrence")
      }
      case bo:ComposedOperation =>
        if (bo.lOp.isReduciable) bo.ComposedOperMemberReduce(reductionStep(bo.lOp, env), bo.rOp)
        else  if (bo.rOp.isReduciable) bo.ComposedOperMemberReduce(bo.lOp, reductionStep(bo.rOp, env))
        else (bo.lOp,bo.rOp) match {
          case (l:T, r:T) => ReduceToUnar(bo, env)
          case (_,_) => println("ERROR: failed to infer kind of expr" + bo.show)
            ErrorExpr("Type missmatch")
        }
      case Var(s: String) => if (env.contains(s))
        env(s) match {
          case i:Int => Number(i)
          case b:Boolean => Bool(b)
          case v:String if s == "__error" => ErrorExpr(v)
          case v:String if v == s => ErrorExpr("VariableSelfLooping")
          case v:String => Var(v)
          case _ => ErrorExpr("Unknown type of variable")
        }
      else ErrorExpr("UndefinedVariable")
      case _ => println("ERROR: failed to infer kind of expr" + expr.show)
        ErrorExpr("UndefExpr")
    }

    def reduce(expr: Expr, env:Map[String, Any]): Expr = {
     // println(expr.show)
      if (expr.isReduciable) reductionStep(expr, env) match {
        case a:Expr if a== expr => ErrorExpr("ReductionLoop")
        case a:Expr => reduce(a, env)
      }
      else
        expr
    }




  }




  def main(args: Array[String]): Unit = {


/*  LOCAL TESTING*/

    var env =  Map("a" -> 2, "b" -> 3, "k" -> false, "var_a" -> "a")

    var red = new Machine()
    var mach = new Machine()

    val preperationEnv = mach.run(Assign("decr", Number(2)), env)
    val testEnv = mach.run(Assign("chk", Number(0)), preperationEnv)
    val test1 = mach.run(
      While( Less(Number(0), Var("decr"))
        ,
        Seq(
          Assign("decr", Sum( Var("decr"), Number(-1) )), //decrementation
          Assign("chk", Sum( Var("chk"), Number(1) ))
        )
      ),
      testEnv)
    println(test1)


    println(red.run(
      Seq(
        Assign("s", Number(2)), Assign("s", Sum(Number(2), Var("s")) )
      )
      , env)
    )

    println(red.run(
      Seq(
        //Assign("s", Number(2)), Assign("s", Sum(Number(2), Var("s")) )
      )
      , env)
    )

    println( IfElse(Less(Number(1), Number(2)), Number(1), Number(2)).eval(env).get )

    println(IfElse(Bool(true), Number(1), Number(2)).eval(env)) //.eval(env) == (if (b) k else n)
    println(IfElse(Number(2), Number(1), Number(2)).eval(env))

    println(red.reductionStep(IfElse(Less(Number(1), Number(2)), Number(1), Number(2)),  env))


    println("ASSUME a + -3 + a + -3")
    new Machine().reduce( Sum( Sum(Var("a"), Number(-3)) , Sum(Var("a"), Number(-3)) ) , env)


    new Machine().reduce( IfElse(Less(Number(2), Bool(false)), Number(-1) ,  Number(-1) ) ,env)


  }

}






