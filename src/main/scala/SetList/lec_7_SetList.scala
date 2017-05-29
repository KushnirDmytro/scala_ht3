package SetList



/**
  * Created by d1md1m on 25.05.17.
  */


object lec_7_SetList {

  sealed trait List[+A]{

    def ++[B>:A](ys:List[B]):List[B] = {
      foldRight(this,ys)(Cons(_,_) )
    }
  }


  def dropWhile[A](xs:List[A])(f: A => Boolean):List[A] = {
    xs match {
      case Cons(h,t) if f(h) => dropWhile(t)(f)
      case _ => xs
    }
  }


  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]{
    override def toString: String = head match {
      case _: List[_] => "[" + head + "]" + " \u279D " + tail
      case _=> head + " \u279D " + tail
    }
  }

def nth[A](n:Int, xs:List[A]):Option[A] = xs match {
  case Cons(h,t) if n > 0 => nth(n-1, t)
  case Cons(h,t) if n == 0 => Some(h)
  case _ => None
}

  def  foldRight[A,B](xs:List[A], neutrallElement:B)
                     (f: (A,B) => B):B = xs match {
    case Nil => neutrallElement
    case Cons(h,t) => f(h, foldRight(t, neutrallElement)(f))
  }

 // def fncTransp[A,B](f: (A,B) => B):((B,A) => B)

  def  foldRightViaFoldLeft[A,B](xs:List[A], neutrallElement:B)
                     (f: (A,B) => B):B = xs match {
    case Nil => neutrallElement
    case Cons(h,t) =>  foldLeft(xs, (b:B) => b)((g,a) => b => g(f(a,b)))(neutrallElement)
  }


  @annotation.tailrec
  def foldLeft[A,B](xs:List[A], neutrall:B)(f:(B, A) => B):B = xs match {
    case Nil => neutrall
    case Cons(h,t) => foldLeft(t, f(neutrall, h))(f)
  }

  def sum (xs:List[Int]):Int =
    foldRight(xs, 0)(_+_)

  def leftSum (xs:List[Int]):Int =
    foldLeft(xs, 0)(_+_)

  def reverse[A](xs:List[A]):List[A] =
  foldLeft(xs, Nil:List[A])( (acc, h) => Cons(h, acc) )

  def prod (xs:List[Int]):Int =
    foldRight(xs, 1)(_*_)

  def length[A](xs: List[A]):Int =
    foldRight(xs,0)((xs, buf) => buf+1)


  def main(args: Array[String]): Unit = {
    val list123 = Cons(1, Cons(Cons(2, Nil), Cons(3, Nil)))
    println(list123)

    println(list123.head)

    println(list123.tail)

    val list456 = Cons(4, Cons(5, Cons(6, Nil)))
    println(list456)

    println(list123 ++ list456)

    println(dropWhile(list456)(_ <= 5))

    println ( 5 + nth(2, list456).get)

    println(length(list456))

    println(sum(list456))
    println(leftSum(list456))
    println(reverse(list456))
    //println(foldRightViaFoldLeft(list123, 0))

  }
}





