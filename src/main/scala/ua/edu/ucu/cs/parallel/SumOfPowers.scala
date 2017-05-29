package ua.edu.ucu.cs.parallel

import org.scalameter

import scala.util.Random
import org.scalameter._

/**
  * Created by d1md1m on 19.05.17.
  */
object SumOfPowers {



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
    val length = 1000000
    val input = (0 until length).map(_ * rnd.nextInt()).toArray

    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ) withWarmer(new Warmer.Default)

    val seqTime = standartConfig.measure {
      pNorm(input, 2)
    }


    val paralellTime = standartConfig.measure {
      pNormParallel(input, 2)
    }

    println(s"SeqTime $seqTime ms")


    println(s"ParalelTime $paralellTime ms")

    println(s"speedUp ${seqTime.value / paralellTime.value}")

  }

}
