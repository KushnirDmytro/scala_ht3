package HW_5

import HT_4.HT_4_Object.integralOptimalThreadsNumber
import com.sun.javaws.exceptions.InvalidArgumentException
import org.scalameter._

import scala.io.Source
import scala.util.Random

/**
  * Created by d1md1m on 19.05.17.
  */
object HT_5 {

  trait Monoid[A] {
    def op(x: A, y: A): A

    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    def op(x: String, y: String): String = x + y

    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(x: List[A], y: List[A]): List[A] = x ++ y

    def zero = Nil
  }

  val intUnderAddition = new Monoid[Int] {
    def op(x: Int, y: Int): Int = x + y

    def zero = 0
  }
  val intUnderMultiplication = new Monoid[Int] {
    def op(x: Int, y: Int): Int = x * y

    def zero = 1
  }
  val booleanUnderConjunction = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x && y

    def zero = true
  }
  val booleanUnderDisjunction = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x || y

    def zero = false
  }

  def endofunMonoid[A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = (x: A) => f(g(x): A): A

    def zero: (A) => A = (x: A) => x //identity mapping
  }


  //TODO ask how to save signature
  //
  //  def endofunMonoid[A] :  Monoid[A=>A] = {
  //    def op(f:A=>A, g:A=>A):A=>A = (x: A) => f(g(x):A):A
  //    def zero: (A) => A = (x:A)=>x //identity mapping
  //  }


  def foldMapBalanced[A, B](xs: IndexedSeq[A], m: Monoid[B])
                           (f: A => B): B =
    if (xs.length == 0)
      m.zero
    else if (xs.length == 1)
      f(xs(0))
    else {
      val (l, r) = xs.splitAt(xs.length / 2)
      m.op(foldMapBalanced(l, m)(f),
        foldMapBalanced(r, m)(f))
    }

  //Use foldMap or foldMapBalanced to determine if an
  //instance of IndexedSeq[Int] is ordered (ascending or
  //  descending).


  val isAccendingMonoid = new Monoid[(Int, Int, Boolean)] {
    def op(x: (Int, Int, Boolean), y: (Int, Int, Boolean)): (Int, Int, Boolean) =
      (math.min(x._1, y._1), math.max(x._2, y._2), x._2 < y._1)

    def zero: (Int, Int, Boolean) = (Int.MaxValue, Int.MinValue, true)
  }


  def isOrderedAscending(arg: IndexedSeq[Int]) = {
    foldMapBalanced(arg, isAccendingMonoid)((el: Int) => (el, el, true))
  }

  val numList1 = IndexedSeq(-1, 2, 3, 23)
  val numList2 = IndexedSeq(5, 2, 4, 7)

  val rez1 = isOrderedAscending(numList1)
  val rez2 = isOrderedAscending(numList2)

  // =================================== PARALLEL FOLDING ==============


  def foldPar[A](xs: IndexedSeq[A],
                 from: Int, to: Int, m: Monoid[A])
                (implicit threshholdSize: Int): A =
    if (to - from < threshholdSize)
      foldSegment(xs, from, to, m)
    else {
      val middle = from + (to - from) / 2
      val (l, r) = parallel(
        foldPar(xs, from, middle, m)(threshholdSize),
        foldPar(xs, middle, to, m)(threshholdSize))
      m.op(l, r)
    }

  def foldSegment[A](xs: IndexedSeq[A],
                     from: Int, to: Int, m: Monoid[A]): A = {
    var res = xs(from)
    var index = from + 1
    while (index < to) {
      res = m.op(res, xs(index))
      index = index + 1
    }
    res
  }


  def foldMapPar[A, B](xs: IndexedSeq[A],
                       from: Int, to: Int, m: Monoid[B])
                      (f: A => B)
                      (implicit threshholdSize: Int): B =
    if (to - from <= threshholdSize)
      foldMapSegment(xs, from, to, m)(f)
    else {
      val middle = from + (to - from) / 2
      val (l, r) = parallel(
        foldMapPar(xs, from, middle, m)(f)(threshholdSize),
        foldMapPar(xs, middle, to, m)(f)(threshholdSize))
      m.op(l, r)
    }


  def foldMapSegment[A, B](xs: IndexedSeq[A],
                           from: Int, to: Int, m: Monoid[B])
                          (f: A => B): B = {
    var res = f(xs(from))
    var index = from + 1
    while (index < to) {
      res = m.op(res, f(xs(index)))
      index = index + 1
    }
    res
  }


  def power(base: Int, power: Int): Int = {

    def powerStep(rez: Int, powerLeft: Int): Int = {
      if (powerLeft >= 1) {
        powerStep(rez * base, powerLeft - 1)
      } else {
        rez
      }
    }

    powerStep(1, power)
  }


  def main(args: Array[String]): Unit = {


    case class wordCountMon(pref: Boolean, body: Int, postf: Boolean)

    type wordCountMonScalar = (Int, Int, Int)


    val WordsCoutMono = new Monoid[wordCountMon] {
      def op(left: wordCountMon, right: wordCountMon): wordCountMon =
      if ( (left.postf && right.pref) || (left.postf && right.pref && right.body > 0))
        wordCountMon(left.pref, left.body + right.body + 1, right.postf)
      else
        wordCountMon(left.pref, left.body + right.body, right.postf)
      def zero: wordCountMon = wordCountMon(pref = false, 0, postf = false) //identity mapping
    }

    def stringToMono(st: String): wordCountMon = {
      if (st.isEmpty) {
        WordsCoutMono.zero
      }
      else {
        val wordEndPattern = "[A-Za-z][^A-Za-z]".r
        wordCountMon(st.head.isLetter,
          wordEndPattern.findAllIn(st).length,
          st.last.isLetter
        )
      }
    }

    def unpack_monoid(mon: wordCountMon): Int = {
      if (mon.postf)
        mon.body+1
      else
        mon.body
    }

 val lines = Source.fromFile("/home/d1md1m/SCALA_FP/Lec8/HT_3_andFriends_Kushnir_D/src/main/scala/HW_5/lines50.txt").getLines.toArray
    //val lines = Source.fromFile("/home/d1md1m/SCALA_FP/Lec8/HT_3_andFriends_Kushnir_D/src/main/scala/HW_5/big.txt").getLines.toArray
    //val lines = Source.fromFile("/home/d1md1m/SCALA_FP/Lec8/HT_3_andFriends_Kushnir_D/src/main/scala/HW_5/text.txt").getLines.toArray


    def rez = foldMapSegment(lines, 0, lines.length, WordsCoutMono)(stringToMono)

    println(unpack_monoid(rez))

    def rez_par = foldMapPar(lines, 0, lines.length, WordsCoutMono)(stringToMono)(200)

    println(unpack_monoid(rez_par))



        val standartConfig = config(
          Key.exec.minWarmupRuns -> 5,
          Key.exec.maxWarmupRuns -> 10,
          Key.exec.benchRuns -> 10,
          Key.verbose -> true
        ) withWarmer(new Warmer.Default)


        val foldMapSequentialRezArr = standartConfig measure {
          foldMapSegment(lines, 0, lines.length, WordsCoutMono)(stringToMono)
        }

        val foldMapParrallelArr = standartConfig measure {
          foldMapPar(lines, 0, lines.length, WordsCoutMono)(stringToMono)(20)
        }


        println(s"foldmapReztWithVector $foldMapSequentialRezArr")

        println(s"foldmapReztWithArray $foldMapParrallelArr")

    println(s"seq / par ${foldMapSequentialRezArr.value/foldMapParrallelArr.value}")


  }
}
