import scala.collection.GenSeq
import scala.util._
import math._
import scala.annotation.tailrec
import org.scalameter._

object KMeans {

type Point =  (Double, Double)

 private val standardConfig = config(
      Key.exec.minWarmupRuns -> 5,
      Key.exec.maxWarmupRuns -> 50
    ) withWarmer new Warmer.Default

def interval(a: Point, b: Point): Double = {
    sqrt(pow(a._1 - b._1, 2) + pow(a._2 - b._2, 2))
  }

def generatePoints(k: Int, num: Int): Seq[Point] = {
    val x = Random
    for {i <- 0 until k}
	 yield (x.nextInt() + x.nextDouble(), x.nextInt() + x.nextDouble())
}

def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    Random.shuffle(points).take(k)
}

def findClosest(p: Point, means: GenSeq[Point]): Point = {
    var x = means.head
    var min = interval(p, means.head)
    for (i <- means) {
      val n = interval(p, i)
      if (n < min) 
      {
        min = n
        x = i
      }
    }
   x
}
 
def classify(points: GenSeq[Point], means: GenSeq[Point]): GenSeq[(Point, GenSeq[Point])] = {
    val x = (-50.0, -50.0)
    val a = means.map(c => (c, points.map(p =>
      if (findClosest(p, means) == c) p else x ).filter(_ != x)))
    a
}

def help(x: GenSeq[Point]): Point = (x.map(_._1).sum / x.length, x.map(_._2).sum / x.length)

def findAverage(oldMean: Point, points: GenSeq[Point]): Point = {
    if (points.isEmpty)
      oldMean
    else 
      help(points)
}

def update(classified: GenSeq[(Point, GenSeq[Point])]): GenSeq[Point] = {
    classified.map(x => findAverage(x._1, x._2))
 }
 
def converged(eta : Double)(oldMeans : GenSeq[Point], newMeans : GenSeq[Point]) : Boolean = {
    oldMeans.zip(newMeans).forall({
      case (old_mean, new_mean) => interval(old_mean, new_mean) < eta
    })
  }



@tailrec
 final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    val follow = update(classify(points, means))
    if (converged(eta)(means, follow))
      follow
    else
      kMeans(points, follow, eta)
 }




def kMeansTime() = {
    val cl = 100
    val num = 500
    val eta = 0.1

    println("K_MEANS")
    val points = generatePoints(cl, num)
    val startingMeans = initializeMeans(cl, points)

    val l = kMeans(points, startingMeans, eta)

    val seqtime = standardConfig measure {
      kMeans(points, startingMeans, eta)
   }

    val partime = standardConfig measure {
      kMeans(points.par, startingMeans.par, eta)
   }

    println("seqtime = " + seqtime)
    println("partime: " + partime)
    println("acceleration: " + seqtime.value / partime.value)
  }

def main(args: Array[String]): Unit = {
    kMeansTime()
  }





}