package ua.edu.ucu.cs.parallel

/**
  * Created by d1md1m on 19.05.17.
  */
object thred_training {

  class GdayThread extends Thread{
    override def run(): Unit ={
      println("G'day")
      println("World!")
    }
  }

  def main(args: Array[String]): Unit = {



    for (i <- 1 to 20) {
      val thread1 = new GdayThread
      val thread2 = new GdayThread
      thread1.start()
      thread2.start()

      thread1.join()
      thread2.join()
    }

  }

}
