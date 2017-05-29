import lec_6_IntegerSetTree.{Empty, NonEmpty}
import sun.invoke.empty.Empty

/**
  * Created by d1md1m on 25.05.17.
  */


object lec_6_IntegerSetTree {



  sealed trait IntSet {
    def rebalance() : IntSet
    def contains(x: Int): Boolean

    def include(x: Int): IntSet

    def union (other : IntSet) : IntSet

    def exclude(x:Int) : IntSet


    def <=(other:IntSet):Boolean=
      size(this) <= size(other)


    def >=(other:IntSet):Boolean=
      size(this) >= size(other)

    def isBalanced : Boolean = this match {
      case Empty =>
        true
      case NonEmpty(el, left, right) =>
        math.abs(height(left) - height(right)) <= 1
    }


    def map (f:Int => Int):IntSet = this match {
      case Empty =>
        this
      case NonEmpty(el, left, right) =>
        NonEmpty(el, left.map(f), right.map(f)) exclude el include f(el)
    }


    def higherSon: IntSet = this match {
      case Empty =>
        throw new Exception("Empty NOD has sons")
      case NonEmpty (el, left, right)=>
        if ( height(left) >= height(right) )
          left
        else
          right
    } //attention, can return EMPTY!


    def getRightSon: IntSet = this match {
      case Empty =>
        Empty
      case NonEmpty(el, left, right) =>
        right
    }


    def getLeftSon: IntSet = this match {
      case Empty =>
        Empty
      case NonEmpty(el, left, right) =>
        left
    }

    def getRootElement: Int = this match {
      case Empty =>
        throw new Exception("Empty has no root")
      case NonEmpty(el, left, right) =>
        el
    }



    def rotate: IntSet = this match {
      case Empty =>
        this
      case NonEmpty (el, left, right) =>
        if (!this.isBalanced){
          if (height(left) > height(right))
            rotationClockwise
          else
            rotationCounterClockwise
        }
        else
          this
    }


    def rebalanceStep: IntSet = this match {
      case Empty =>
        this
      case NonEmpty (el, left, right) =>
        if (isBalanced)
          this
        else
          this rotate
    }




    def rotationClockwise: IntSet = this match { //left is highter
      case Empty =>
        Empty
      case NonEmpty(el, left, right) =>
        NonEmpty(
          left.getRootElement,
          left.getLeftSon.rebalanceStep,
          NonEmpty(el, this.getLeftSon.getRightSon, this.getRightSon).rebalanceStep )

    }

    def rotationCounterClockwise: IntSet = this match {//right is highter
      case Empty =>
        Empty
      case NonEmpty(el, left, right) =>
        NonEmpty(
          right.getRootElement,
          NonEmpty(el, this.getLeftSon, this.getRightSon.getLeftSon).rebalanceStep,
          right.getRightSon).rebalanceStep
    }


    def isValid:Boolean = this match {
      case Empty => true
      case NonEmpty (el, left, right) =>
        (left, right) match {
          case (Empty, Empty) =>
            true
          case (NonEmpty (_, _, _), NonEmpty (_, _, _)) =>
            ((el > left.getRootElement)
              &&
              (el < right.getRootElement))
          case (NonEmpty (_, _, _), Empty) =>
            el > left.getRootElement
          case (Empty, NonEmpty (_, _, _)) =>
            el < right.getRootElement
        }
    }


    def treeLine(H:Int, s:String):String = this match {
      case Empty => ""
      case NonEmpty (el, left, right) =>
        "\t" * H + s +el + "\n"
    }

    def treePrint(H:Int, s:String):String = this match {
      case Empty =>
        ""
      case NonEmpty (el, left, right) =>
        left.treePrint(H + 1, "/") + this.treeLine(H, s)  + right.treePrint(H + 1, "\\")
    }





  }



  case object Empty extends IntSet{
    def contains(x: Int): Boolean =
      false

    def rebalance():IntSet = this

    def exclude(x:Int) : IntSet = this

    def union (other : IntSet) : IntSet = other

    def include(x: Int): IntSet =
      NonEmpty(x, Empty, Empty)

    override def toString: String = "."
  }

  case class NonEmpty(el: Int,
                      left: IntSet,
                      right: IntSet) extends IntSet {

    def contains(x:Int): Boolean =
      if (x<el) left contains x
      else if (x>el) right contains x
      else true


    def rebalance() : IntSet ={
      val tr = this.rebalanceStep
      if (tr.isBalanced)
        tr
      else
        tr.rebalance()
    }


    def exclude(x:Int) : IntSet = {
      if (x < el) NonEmpty (el, left exclude x, right)
      else if (x > el) NonEmpty (el, left, right exclude x)
      else left union right
    }



    def ~(arg:IntSet) : IntSet = {
      if (size(arg) > 0)
        arg
      else
        Empty
    }

    def union (other : IntSet) : IntSet ={
      if (this <= other)
        other include el union left union right
      else
        other union this
    }


    def include (x:Int) : IntSet  = {
      if (x<el) NonEmpty(el, left include  x, right)
      else if (x>el) NonEmpty(el, left , right include  x)
      else this
    }



    override def toString: String =
      "{" + left + el + right + "}"
  }



  def height(set : IntSet): Int = set match {
    case Empty =>
      0
    case NonEmpty (_, left, right) =>
      1 + Math.max(height(left), height(right))
  }

  /*
  def bubbleSortInsert(noob: Int, set: IntSet):IntSet = set match {
    case Empty =>
      set include noob
    case NonEmpty =>

  }
  */



  def size(set : IntSet): Int = set match {
    case Empty =>
      0
    case NonEmpty (_, left, right) =>
      1 + size(left) + size(right)
  }





  def main(args: Array[String]): Unit = {

    /*
    val set = NonEmpty(7, Empty, Empty)

    val set2 = set include 5 include 12

    val set3 = set include 3 include 17 include 5

    println(set)
    println(set2)
    println(set3)


    println(size(set))
    println(size(set2))
      println(size(set3))

      println(height(set))
      println(height(set2))
      println(height(set3))


    val setX = Empty union Empty
    println(setX)
    println( Empty union set )

    println(set union Empty )

    println(set2 union set )

    println(set2 union set3 )

    println(set3)
    println(set3 exclude 7)
    println(set3 exclude 7 include -8)
    println("map -15 :")
    println( set3 map(x => 1 ))
//    println("map *-1 :")

    println(set3 map(x => x * -1    ))

    println(set3.isBalanced)
    val  set5 = set3 include 10 include 11 include 12
    println(set5)
    print(set5.treePrint( 0, ""))
    println(set5.isBalanced)
    val set4 = set5.rotate
    print(set4.treePrint(0, ""))
    println(set4.isBalanced)
    println(set4.isValid)
*/
    val set1 = NonEmpty(5, Empty, Empty)
    val set2 = set1 include 8 include 7 include 6 include 4 include 3 include 2 include 1 include 0 include -1 include -2 include -3 include -4 include -5
    print(set2.treePrint(0, ""))
    println("balanced " + set2.isBalanced)
    println("valid " +set2.isValid)
    val set3 = set2.rebalance()

    print(set3.treePrint(0, ""))
    println("balanced " + set3.isBalanced)
    println("valid " +set3.isValid)

    val set4 = set3 map(x => 1)
    print(set4.treePrint(0, ""))
    println("balanced " + set4.isBalanced)
    println("valid " +set4.isValid)
  }

}





