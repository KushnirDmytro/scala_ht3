
package HT_4

import HT_4.HT_4_Object.{countPointsUnderIntegral, getSizeOfBox, integralOptimalThreadsNumber}
import com.sun.javaws.exceptions.InvalidArgumentException
import org.scalameter._

import scala.util.Random

/**
  * Created by d1md1m on 19.05.17.
  */
object HT_4_ObjectAutoBorders{


  def sequentialIntegral(func: (Seq[Double]) => Double,
                          totalNumberOfPoints:Int,
                         tuple2: Tuple2[Double, Double]*): Double = {

    val (empiricMin, empiricMax) = HeatMinMax(func,
      totalNumberOfPoints,
    tuple2)

    //println(empiricMin, empiricMax)

    val hits = countPointsUnderIntegral(func,
      totalNumberOfPoints,Tuple2(empiricMin, empiricMax), tuple2: _*)

    val argsBoxSize = getSizeOfSampleSpace(tuple2: _* ) //rem

    hits.toDouble / totalNumberOfPoints * argsBoxSize * (empiricMax - empiricMin)

  }

  def getSizeOfSampleSpace(tuple2: (Double, Double)*):Double = {
    tuple2.foldLeft(1.0)((acc, elem) => acc * (elem._2 - elem._1))
  }

  def argumentsAreValid(tuple2: (Double, Double)*):Boolean = {
    val sizeCheck = tuple2.nonEmpty
    val pairwiseCheck = tuple2.foldLeft(true)( (acc, el) => acc && (el._2 > el._1) )
    sizeCheck && pairwiseCheck
  }

  def toSizesArray(
                    tuple2:(Double, Double)*
                  ) : Seq[Double] = {
    tuple2.map ( el => el._2 - el._1 )
  }

  def toStartOffsetsArr(
    tuple2:(Double, Double)* ) : Seq[Double] = {
    tuple2.map ( el => el._1 )
  }



  def generateArgs(random: Random, tuple2: (Double, Double)*):Seq[(Double)] = {
    tuple2.map(el => random.nextDouble() * (el._2 - el._1) + el._1 )
  }


  def HeatMinMax(func: (Seq[Double]) => Double,
                               totalNumberOfPoints:Int,
                               tuple2: Seq[(Double, Double)]
                              ):(Double, Double) ={


    if( !argumentsAreValid(tuple2: _ *) )
      throw new InvalidArgumentException(Array("Integration limits fail"))
    else {

      val rndX = new Random

      val presision = totalNumberOfPoints / 10 // Direct limitation By Amdal's Law
      def simulation(
                     pointsGenerated: Int,
                     detectedMin:Double,
                     detectedMax:Double,
                    lastDetectedMin:Int,
                     lastDetectedMax:Int
                    ): (Double, Double) = {

        if ( ((pointsGenerated -lastDetectedMin > presision) &&
          (pointsGenerated - lastDetectedMax > presision)) ||
          (pointsGenerated >= totalNumberOfPoints) )
        {

        //  println(s"HEat run $pointsGenerated")
          (detectedMin, detectedMax)
        }

        else {

          val x = generateArgs(rndX, tuple2: _*)

          val fv /*function value*/ = func(x)

          if (fv < detectedMin)
            simulation(
              pointsGenerated + 1,
              fv,
              detectedMax,
              pointsGenerated + 1,
              lastDetectedMax
            )
            else
          if (fv> detectedMax)
            simulation(
              pointsGenerated + 1,
              detectedMin,
              fv,
              lastDetectedMax,
              pointsGenerated + 1
            )
            else
          simulation(
            pointsGenerated + 1,
            detectedMin,
            detectedMax,
            lastDetectedMin,
            lastDetectedMax
          )

        }
      }

      simulation(0, Double.MaxValue, Double.MinValue, 0, 0)
    }

  }


  def countPointsUnderIntegral(func: (Seq[Double]) => Double,
                               totalNumberOfPoints:Int,
                               empiricBorders:Tuple2[Double, Double],
                               tuple2: (Double, Double)*
                               ):(Int) ={



      val rndY = new Random
    val rndX = new Random

      def simulation(hits: Int,
                     pointsGenerated: Int
                    ): Int = {

          if (pointsGenerated >= totalNumberOfPoints)
            hits

          else {

            val y = generateArgs(rndY, (empiricBorders._1, empiricBorders._2)).head
            val x = generateArgs(rndX, tuple2: _*)


            val fv = func(x)

            simulation(
              hits +
                (if (y > 0 && y < fv ) 1
              else
                if (y < 0 && y > fv ) -1
                else 0
                ),
              pointsGenerated + 1
            )

          }
        }

        val hitsCounted = simulation(0, 0)
      hitsCounted
  }


  def integralOptimalThreadsNumber(func: (Seq[Double]) => Double,
                                   totalNumberOfPoints:Int,
                                   tuple2: Tuple2[Double, Double]*):Double = {

    val optimalTaskSize = totalNumberOfPoints / Runtime.getRuntime.availableProcessors()

    val (empMin:Double, empMax:Double) = HeatMinMax(func, totalNumberOfPoints, tuple2)

    def splitTaskSize(taskSize:Int):Int ={
      if (taskSize <= optimalTaskSize)
        countPointsUnderIntegral(func, taskSize,(empMin, empMax), tuple2:_* )
      else {
        val (r1, r2) = parallel(
          splitTaskSize(taskSize/ 2),
          splitTaskSize(taskSize / 2)
        )
        r1 + r2
      }
    }


    val rez_size = getSizeOfBox(tuple2:_*) * (empMax - empMin)

    splitTaskSize(totalNumberOfPoints).toDouble / totalNumberOfPoints.toDouble * rez_size
  }



  def sinFunc(arg:Double*):Double = math.sin(arg.head)

  def main(args: Array[String]): Unit = {

    val totalNumberOfPoints = 100000


    val fSin:  (Seq[Double]) => Double = (x) => {math.sin(x.head)}
    val fCos:  (Seq[Double]) => Double = (x) => {math.cos(x.head)}

    val fSumofTwo: (Seq[Double]) => Double = (x) => {x.head + x.tail.head}
    val fprodOfTwo: (Seq[Double]) => Double = (x) => {x.head * x.tail.head}


    println( sequentialIntegral( fSumofTwo , totalNumberOfPoints,  (0 , 1), (0, 1)) )

    println( sequentialIntegral( fprodOfTwo , totalNumberOfPoints,  (0 , 1), (0, 1)) )




    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 500,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ) withWarmer(new Warmer.Default)

    val seqIntegral = standartConfig measure {
      sequentialIntegral( fSin, totalNumberOfPoints,(0 , math.Pi/2))
    }


    println(s"sinConsCount $seqIntegral")
    val ParIntegral = standartConfig measure {
      integralOptimalThreadsNumber( fSin, totalNumberOfPoints, (0 , math.Pi/2))
    }


    println(s"PiSecCount $seqIntegral")
    println(s"PiCountPar $ParIntegral")

    println(s"speedRatio1vs2 ${seqIntegral.value/ParIntegral.value}")

  }

}
