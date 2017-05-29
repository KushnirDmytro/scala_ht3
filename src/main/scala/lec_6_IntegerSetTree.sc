/**
  * Created by d1md1m on 25.05.17.
  */



  sealed trait IntSet {
    def contains(x: Int): Boolean

    def include(x: Int): IntSet

  def union (other : IntSet) : IntSet
  }


  case object Empty extends IntSet{
    def contains(x: Int): Boolean =
      false


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


    def union (other : IntSet) : IntSet ={

      other include el union left union right

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


def size(set : IntSet): Int = set match {
  case Empty =>
    0
  case NonEmpty (_, left, right) =>
    1 + size(left) + size(right)
}


val set = NonEmpty(7, Empty, Empty)

val set2 = set include 5 include 12

val set3 = set include 3 include 17 include 5

size(set)
size(set2)
size(set3)

height(set)
height(set2)
height(set3)


val set4 = set union set2


//val set2And3 = set2 union set3


//def toStr(r:Rational) = numer(r) + "/" + denom(r)





