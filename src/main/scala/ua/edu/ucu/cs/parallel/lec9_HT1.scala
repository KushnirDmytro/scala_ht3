package ua.edu.ucu.cs.parallel

import org.scalameter

import scala.util.Random
import org.scalameter._

/**
  * Created by d1md1m on 19.05.17.
  */
object lec9_HT1 {



  def power(x: Int, p: Double): Int =
    math.exp(p * math.log(math.abs(x))).toInt



  def sumSegment(a: Array[Int], p: Double,
                 from: Int, to: Int): Int = {
    def iter(sum: Int, index: Int): Int =
      if (index >= to) sum
      else iter(sum + power(a(index), p), index + 1)

    iter(0, from)
  }


  var treshhold = 1000
  def sumSegmentPar(a: Array[Int], p: Double,
                    from: Int, to: Int): Int = {
    if (to - from < treshhold)
      sumSegment(a,p, from, to)
    else{
      val middle = from + (to - from )/2
      val (sum1, sum2) = parallel(sumSegmentPar(a,p, from, middle), sumSegmentPar(a,p, middle, to) )
      sum1 + sum2
    }

  }


  def pNorm(a: Array[Int], p: Double): Int = power(sumSegment(a, p, 0, a.length), 1/p ) ;

  def pNormParallel(a: Array[Int], p: Double): Int = power(sumSegmentPar(a, p, 0, a.length), 1/p ) ;

  def main(args: Array[String]): Unit = {
    val rnd = new Random
    val length = 10000000
    val input = (0 until length).map(_ * rnd.nextInt()).toArray

    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 500,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ) withWarmer(new Warmer.Default)


    def countSum (a: Array[Int], s: Int, pos:Int, to : Int):Int = {
      if (pos >= to) {
       // println(s"Sum is $s ")
        s
      }
      else
        countSum(a, s + a(pos), pos + 1, to)
    }

    def countAvg (a: Array[Int], s: Int, n:Int ,pos:Int, to:Int):Double = {
      if (pos >= to) {
      //  println(s"Avg is ${s / n}")
        s / n
      }
      else
        countAvg(a, s + a(pos), n+1 , pos + 1, to)
    }

    val mid = input.length / 2

    val paralellTimeSeparate = standartConfig.measure {
      parallel(countSum(input,0,0, mid), countSum(input,0,mid, input.length))
      parallel(countAvg(input,0,0,0, mid), countAvg(input,0,0,mid, input.length))
    }


    val paralellTimeSimultanious = standartConfig.measure {
      parallel(countSum(input,0,0,input.length), countAvg(input,0,0,0, input.length) )
    }


    println(s"paralellTimeSimultanious1Run $paralellTimeSimultanious")

    println(s"paralellTimeSeparate2runs $paralellTimeSeparate")

    println(s"speedRatio ${paralellTimeSimultanious.value / paralellTimeSeparate.value}")
    println(s"number of elements $length")

  }

}
