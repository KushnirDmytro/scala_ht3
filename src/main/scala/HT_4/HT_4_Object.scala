package HT_4

import com.sun.javaws.exceptions.InvalidArgumentException
import org.scalameter._

import scala.util.Random

/**
  * Created by d1md1m on 19.05.17.
  */
object HT_4_Object{


  def sequentialIntegral(func: (Seq[Double]) => Double,
                          totalNumberOfPoints:Int,
                         tuple2: Tuple2[Double, Double]*): Double = {
    val boxSize = getSizeOfBox(tuple2:_*)

    val hits = countPointsUnderIntegral(func,
      totalNumberOfPoints,
    tuple2)

    hits.toDouble / totalNumberOfPoints * boxSize
  }

  def getSizeOfBox(tuple2: (Double, Double)*):Double = {
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
                               ):Int ={


    if( !argumentsAreValid(tuple2: _ *) )
      throw new InvalidArgumentException(Array("Integration limits fail"))
    else {

        val rndX = new Random
        val rndY = new Random

      def simulation(hits: Int,
                       pointsGenerated: Int): Int = {

          if (pointsGenerated >= totalNumberOfPoints)
            hits

          else {

            val x = generateArgs(rndX, tuple2.tail: _*)
            val y = generateArgs(rndX, tuple2.head).head

            val fv /*function value*/ = func(x)

            simulation(
              hits +
                (if
              (y > 0 && y < fv )
                1
              else if
              (y < 0 && y > fv )
                -1
                else
                0
                ),
              pointsGenerated + 1
            )

          }
        }

        val hitsCounted = simulation(0, 0)
      hitsCounted
      }

  }



  def integralOptimalThreadsNumber(func: (Seq[Double]) => Double,
                                      totalNumberOfPoints:Int,
                                      tuple2: Tuple2[Double, Double]*):Double = {

    val optimalTaskSize = totalNumberOfPoints / Runtime.getRuntime.availableProcessors()

    def splitTaskSize(taskSize:Int):Int ={
      if (taskSize <= optimalTaskSize)
        countPointsUnderIntegral(func, taskSize ,tuple2)
      else {
        val (r1, r2) = parallel(
          splitTaskSize(taskSize/ 2),
          splitTaskSize(taskSize / 2)
        )
        r1 + r2
      }
    }

    val rez = splitTaskSize(totalNumberOfPoints)
   // println(s"parralel hits $rez")

    val rez_size = getSizeOfBox(tuple2:_*)
    //println(s"parralel rezSize $rez_size")


    rez.toDouble / totalNumberOfPoints.toDouble * rez_size
  }



  def sinFunc(arg:Double*):Double = math.sin(arg.head)

  def main(args: Array[String]): Unit = {

    val totalNumberOfPoints = 100000



    val fSin:  (Seq[Double]) => Double = (x) => {math.sin(x.head)}
    val fCos:  (Seq[Double]) => Double = (x) => {math.cos(x.head)}

    val fSumofTwo: (Seq[Double]) => Double = (x) => {x.head + x.tail.head}
    val fprodOfTwo: (Seq[Double]) => Double = (x) => {x.head * x.tail.head}


    println("seq")
    println( sequentialIntegral( fSin, totalNumberOfPoints, (-1.0, 1.0),(0 , math.Pi/2)))

    println("parall")
    println( integralOptimalThreadsNumber( fSin, totalNumberOfPoints, (-1.0, 1.0),(0 , math.Pi/2)))
/*
    println( sequentialIntegral( fSin, totalNumberOfPoints, (-1.0, 1.0),(-math.Pi/2 , math.Pi/2)) )

    println( sequentialIntegral( fSin , totalNumberOfPoints, (-1.0, 1.0), (-math.Pi/2 , 0)) )

    println( sequentialIntegral( fCos, totalNumberOfPoints, (-1.0, 1.0),(0 , math.Pi/2)))

    println( sequentialIntegral( fCos, totalNumberOfPoints, (-1.0, 1.0),(-math.Pi/2 , math.Pi/2)) )

    println( sequentialIntegral( fCos , totalNumberOfPoints, (-1.0, 1.0), (-math.Pi/2 , 0)) )

    println( sequentialIntegral( fSumofTwo , totalNumberOfPoints, (0, 2.0), (0 , 1), (0, 1)) )

    println( sequentialIntegral( fprodOfTwo , totalNumberOfPoints, (0, 1.0), (0 , 1), (0, 1)) )

    // println( sequentialIntegral( fSin , totalNumberOfPoints, (-1.0, 1.0), (-math.Pi/2 , 0)) )
*/

    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 500,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ) withWarmer(new Warmer.Default)

    val seqIntegral = standartConfig measure {
      sequentialIntegral( fSin, totalNumberOfPoints, (-1.0, 1.0),(0 , math.Pi/2))
    }

    val ParIntegral = standartConfig measure {
      integralOptimalThreadsNumber( fSin, totalNumberOfPoints, (-1.0, 1.0),(0 , math.Pi/2))
    }


        println(s"PiSecCount $seqIntegral")
        println(s"PiCountPar $ParIntegral")

        println(s"speedRatio1vs2 ${seqIntegral.value/ParIntegral.value}")


  }

}
