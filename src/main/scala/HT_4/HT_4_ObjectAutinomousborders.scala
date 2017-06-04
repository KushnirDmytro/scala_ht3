
package HT_4

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

    val (hits, empiricMin, empiricMax) = countPointsUnderIntegral(func,
      totalNumberOfPoints,
    tuple2)

    println(s"hits $hits")

    val coef = hits.toDouble / totalNumberOfPoints

    println(s"coef $coef")

    val argsBoxSize = getSizeOfSampleSpace(tuple2.tail: _* ) //rem
    println(s"ArgsBoxSize: $argsBoxSize")

    val rez = coef * argsBoxSize * (empiricMax - empiricMin)
    println(s"rez $rez")
    rez
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


  def countPointsUnderIntegral(func: (Seq[Double]) => Double,
                               totalNumberOfPoints:Int,
                               tuple2: Seq[(Double, Double)]
                               ):(Int, Double, Double) ={


    if( !argumentsAreValid(tuple2: _ *) )
      throw new InvalidArgumentException(Array("Integration limits fail"))
    else {

        val rndX = new Random
        val rndY = new Random


      def simulation(hits: Int,
                     pointsGenerated: Int,
                    detectedMin:Double,
                    detectedMax:Double
                    ): (Int, Double, Double) = {

          if (pointsGenerated >= totalNumberOfPoints)
            (hits, detectedMin, detectedMax)

          else {

            val x = generateArgs(rndX, tuple2.tail: _*)
            val y = generateArgs(rndX, tuple2.head).head

            val fv /*function value*/ = func(x)

            simulation(
              hits +
                (if (y > 0 && y < fv ) 1
              else
                if (y < 0 && y > fv ) -1
                else 0
                ),
              pointsGenerated + 1,
              math.min(fv, detectedMin),
              math.max(fv, detectedMax)
            )

          }
        }

        val hitsCounted = simulation(0, 0, Double.MaxValue, Double.MinValue)
      (hitsCounted._1, hitsCounted._2, hitsCounted._3)
      }

  }



/*
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
*/

  //  def pNorm(a: Array[Int], p: Double): Int = power(sumSegment(a, p, 0, a.length), 1/p ) ;

  // def pNormParallel(a: Array[Int], p: Double): Int = power(sumSegmentPar(a, p, 0, a.length), 1/p ) ;

  def sinFunc(arg:Double*):Double = math.sin(arg.head)

  def main(args: Array[String]): Unit = {

    val totalNumberOfPoints = 1000000


    val boxSize = getSizeOfSampleSpace((0 , math.Pi/2), (-1, 1), (0, 2))
    println(boxSize)

    val sizeArray = toSizesArray((0.0 , math.Pi/2), (-1.0, 1.0), (0.0, 2.0))

    println(sizeArray)

    val check = argumentsAreValid((0 , math.Pi/2), (-1, 1), (0, 2))

    println(check)

    val check2 = argumentsAreValid((0 , math.Pi/2), (-1, -31), (0, 2))

    println(check2)

    val begArr = toStartOffsetsArr((0 , math.Pi/2), (-1, -31), (0, 2))

    println(begArr)

    val fSin:  (Seq[Double]) => Double = (x) => {math.sin(x.head)}
    val fCos:  (Seq[Double]) => Double = (x) => {math.cos(x.head)}

    val fSumofTwo: (Seq[Double]) => Double = (x) => {x.head + x.tail.head}
    val fprodOfTwo: (Seq[Double]) => Double = (x) => {x.head * x.tail.head}


    println( sequentialIntegral( fSin, totalNumberOfPoints, (-1.0, 1.0),(0 , math.Pi/2)))

    println( sequentialIntegral( fSin, totalNumberOfPoints, (-1.0, 1.0),(-math.Pi/2 , math.Pi/2)) )

    println( sequentialIntegral( fSin , totalNumberOfPoints, (-1.0, 1.0), (-math.Pi/2 , 0)) )

    println( sequentialIntegral( fCos, totalNumberOfPoints, (-1.0, 1.0),(0 , math.Pi/2)))

    println( sequentialIntegral( fCos, totalNumberOfPoints, (-1.0, 1.0),(-math.Pi/2 , math.Pi/2)) )

    println( sequentialIntegral( fCos , totalNumberOfPoints, (-1.0, 1.0), (-math.Pi/2 , 0)) )

    println( sequentialIntegral( fSumofTwo , totalNumberOfPoints, (0, 2.0), (0 , 1), (0, 1)) )

    println( sequentialIntegral( fprodOfTwo , totalNumberOfPoints, (0, 1.0), (0 , 1), (0, 1)) )

    // println( sequentialIntegral( fSin , totalNumberOfPoints, (-1.0, 1.0), (-math.Pi/2 , 0)) )


    /*
    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 500,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ) withWarmer(new Warmer.Default)

    val seqSin = standartConfig measure {
      sinSeq(-math.Pi/2 , math.Pi/2, -1.0, 1.0, totalNumberOfPoints)
    }


    println(s"sinConsCount $seqSin")
    //println(s"PiCountPar $ParPi")


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

    */
  }

}
