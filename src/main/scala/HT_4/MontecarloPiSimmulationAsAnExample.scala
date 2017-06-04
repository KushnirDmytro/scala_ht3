package HT_4

import org.scalameter._

import scala.util.Random

/**
  * Created by d1md1m on 19.05.17.
  */
object MontecarloPiSimmulationAsAnExample {

  var treshhold = 1000

  def pi (totalNumberOfPoints:Int):Double = 4.0* countPointsInsideCircle(totalNumberOfPoints)/totalNumberOfPoints


  def countPointsInsideCircle(totalNumberOfPoints:Int):Int ={
    val rndX = new Random
    val rndY = new Random

    def simulation(hits:Int, pointsGenerated:Int):Int ={

      if (pointsGenerated >= totalNumberOfPoints)
        hits
      else {
        val x = rndX.nextDouble()
        val y = rndY.nextDouble()

        simulation(hits + (if (x*x + y*y <= 1) 1 else 0), pointsGenerated+1)
      }
    }
    simulation(0,0)
  }


  def piPar(totalNumberOfPoints:Int) = {


    val ((pi1, pi2), (pi3, pi4)) =

      parallel(
        parallel(
          countPointsInsideCircle(totalNumberOfPoints/4), countPointsInsideCircle(totalNumberOfPoints/4)
        ),
        parallel(
          countPointsInsideCircle(totalNumberOfPoints/4), countPointsInsideCircle(totalNumberOfPoints/4)
        )
      )

    4.0 * (pi1 + pi2 + pi3 + pi4) /totalNumberOfPoints
  }


  def piParOptimalThreadsNumber(totalNumberOfPoints:Int):Double = {

    val optimalTaskSize = totalNumberOfPoints / Runtime.getRuntime.availableProcessors()

    def splitTaskSize(taskSize:Int):Int ={
      if (taskSize <= optimalTaskSize)
        countPointsInsideCircle(taskSize)
      else {
        val (r1, r2) = parallel(
          splitTaskSize(taskSize/ 2),
          splitTaskSize(taskSize / 2)
        )
        r1 + r2
      }
    }

    splitTaskSize(totalNumberOfPoints) * 4.0 / totalNumberOfPoints
  }


//  def pNorm(a: Array[Int], p: Double): Int = power(sumSegment(a, p, 0, a.length), 1/p ) ;

 // def pNormParallel(a: Array[Int], p: Double): Int = power(sumSegmentPar(a, p, 0, a.length), 1/p ) ;

  def main(args: Array[String]): Unit = {

    val totalNumberOfPoints = 1000000

    println(pi(totalNumberOfPoints))
    println(piPar(totalNumberOfPoints))
    println(piParOptimalThreadsNumber(totalNumberOfPoints))



    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 500,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ) withWarmer(new Warmer.Default)



    val seqPi = standartConfig measure {
      pi(totalNumberOfPoints)
    }


    val ParPi = standartConfig measure {
      piPar(totalNumberOfPoints)
    }

    val ParPiOptThreadsN = standartConfig measure {
      piParOptimalThreadsNumber(totalNumberOfPoints)
    }

    println(s"PiSecCount $seqPi")
    println(s"PiCountPar $ParPi")
    println(s"PiCountPar $ParPiOptThreadsN")

    println(s"speedRatio1vs2 ${seqPi.value/ParPi.value}")
    println(s"speedRatio2vs3 ${ParPi.value/ParPiOptThreadsN.value}")


  }

}
