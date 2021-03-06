import java.util.concurrent.{ForkJoinPool, ForkJoinWorkerThread, RecursiveTask}

import HW_5.{task,parallel}

import scala.util.Random


trait Monoid[A]{
  def op(x:A, y:A):A
  def zero:A
}

val stringMonoid = new Monoid[String] {
  def op(x:String, y:String): String = x+y
  def zero = ""
}

def listMonoid[A] = new Monoid[List[A]] {
  def op(x:List[A], y:List[A]): List[A] = x++y
  def zero = Nil
}

val intUnderAddition = new Monoid[Int] {
  def op(x:Int, y:Int): Int = x+y
  def zero = 0
}
val intUnderMultiplication = new Monoid[Int] {
  def op(x:Int, y:Int): Int = x*y
  def zero = 1
}
val booleanUnderConjunction = new Monoid[Boolean] {
  def op(x:Boolean, y:Boolean): Boolean = x&&y
  def zero = true
}
val booleanUnderDisjunction = new Monoid[Boolean] {
  def op(x:Boolean, y:Boolean): Boolean = x||y
  def zero = false
}

def endofunMonoid[A] = new Monoid[A=>A]  {
  def op(f:A=>A, g:A=>A):A=>A = (x: A) => f(g(x):A):A
  def zero: (A) => A = (x:A)=>x //identity mapping
}

val words = List ("Hello" , "Monoidal" , "Folding")

val rightSentance = words.foldRight(stringMonoid.zero)(stringMonoid.op)

val leftSentance = words.foldLeft(stringMonoid.zero)(stringMonoid.op)

//Create a generic function for concatenating list of elements of any
//type that forms a monoid

def concat [A] ( xs : List [A] , m: Monoid [A ] ) : A =
  xs . foldLeft(m. zero) (m. op )

val sentance = concat(words, stringMonoid)

def foldMap [ A, B ] ( xs : List [A] , m: Monoid [ B ] ) ( f : A => B ) : B =
xs . foldLeft (m. zero ) ( ( b , a ) => m. op ( b , f ( a ) ) )

val w1 = "first"
val w2 = "second"
val w3 = "third"


def foldMapBalanced [ A, B ] ( xs : IndexedSeq [A] , m: Monoid [ B ] )
                             ( f : A => B ) : B =
  if ( xs . length == 0 )
m. zero
  else if ( xs . length == 1 )
f ( xs ( 0 ) )
else {
  val ( l , r ) = xs . splitAt ( xs . length/2 )
  m. op ( foldMapBalanced ( l , m) ( f ) ,
    foldMapBalanced ( r , m) ( f ) )
}

//Use foldMap or foldMapBalanced to determine if an
//instance of IndexedSeq[Int] is ordered (ascending or
//  descending).


val isAccendingMonoid = new Monoid[(Int, Int, Boolean)] {
  def op( x:(Int, Int, Boolean), y:(Int, Int, Boolean) ):(Int, Int, Boolean) =
  (math.min(x._1, y._1), math.max(x._2, y._2), x._2 < y._1 )
  def zero: (Int, Int, Boolean) = (Int.MaxValue, Int.MinValue, true)
}


def isOrderedAscending (arg: IndexedSeq[Int]) = {
  foldMapBalanced(arg, isAccendingMonoid)( (el:Int) => (el, el , true)  )
}

val numList1 = IndexedSeq(-1,2,3, 23)
val numList2 = IndexedSeq(5,2,4,7)

val rez1 = isOrderedAscending(numList1)
val rez2 = isOrderedAscending(numList2)

// =================================== PARALLEL FOLDING ==============



def foldPar [A] ( xs : IndexedSeq [A] ,
from : Int , to : Int , m: Monoid [A] )
( implicit threshholdSize : Int ) : A =
  if ( to - from < threshholdSize)
foldSegment ( xs , from , to , m)
else {
  val middle = from + ( to -from ) / 2
  val ( l , r ) = parallel(
    foldPar ( xs , from , middle , m) ( threshholdSize ) ,
    foldPar ( xs , middle , to , m) ( threshholdSize ) )
  m. op ( l , r )
}

def foldSegment [A ] ( xs : IndexedSeq [A] ,
from : Int , to : Int , m: Monoid [A ] ) : A = {
  var res = xs ( from )
  var index = from + 1
  while ( index < to ) {
    res = m. op ( res , xs ( index ) )
    index = index + 1
  }
  res
}


def foldMapPar [ A, B ] ( xs : IndexedSeq [A] ,
                          from : Int , to : Int , m: Monoid [B] )
                        ( f : A => B)
                        ( implicit threshholdSize : Int ) : B =
  if ( to - from <= threshholdSize)
    foldMapSegment ( xs , from , to , m)(f)
  else {
    val middle = from + ( to -from ) / 2
    val ( l , r ) = parallel(
      foldMapPar ( xs , from , middle , m)(f) ( threshholdSize ) ,
      foldMapPar ( xs , middle , to , m)(f) ( threshholdSize ) )
    m. op ( l , r )
  }


def foldMapSegment [ A, B ] ( xs : IndexedSeq [A] ,
                              from : Int , to : Int , m: Monoid [B] )
                            ( f : A => B ): B =
{
  var res =  f ( xs ( from ) )
  var index = from + 1
  while ( index < to ) {
    res = m. op ( res , f(xs ( index )) )
    index = index + 1
  }
  res
}


def power (base:Int, power:Int):Int = {

  def powerStep(rez:Int, powerLeft:Int):Int = {
    if (powerLeft >= 1) {
      powerStep(rez*base, powerLeft-1)
    }else{
      rez
    }
  }

  powerStep(1,power)
}

val rnd = new Random
val length = 1000000
val source = ( 0 until length ) .
  map( _ * rnd.nextInt( ) ).toVector
  implicit val threshhold = 1000
val monoid = new Monoid [ Int ] {
  def op ( x : Int , y : Int ): Int = x + y
  def zero = 0
}
foldMapPar ( source ,
  0 , source . length ,
  monoid ) ( power ( _ , 2 ) )

