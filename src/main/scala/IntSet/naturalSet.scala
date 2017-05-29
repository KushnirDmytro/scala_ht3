package IntSet

/**
  * Created by d1md1m on 25.05.17.
  */


object naturalSet {

  /*
  class naturalSet(steps:Int) {
    val innerSet : Set[Int] = null

   def contain(el :Int) :Boolean ={
      innerSet.contains(el)
   }


    override def toString: String =  innerSet.toString()
  }
*/

  //def toStr(r:Rational) = numer(r) + "/" + denom(r)



def naturalSet(steps:Int, set: Set[Int]) : Set[Int]={


  def innerCycle(hostSet: Set[Int], iter: Iterator[Int] ): Set[Int] = {
    if ( !iter.hasNext )
    hostSet
    else {
      val adder:Int = iter.next()
      println(hostSet)
      hostSet.map(el => el + adder) ++ innerCycle( hostSet  , iter)
    }

  }

  if (steps == 0){
    println(set.toString())
    set
  }

  else
    {
      naturalSet(steps-1, innerCycle(set, set.iterator))
    }
}


  def main(args: Array[String]): Unit = {

    val set :Set[Int]  = Set(0,1)
    naturalSet (3,  Set(0,1)  )

  }
}
