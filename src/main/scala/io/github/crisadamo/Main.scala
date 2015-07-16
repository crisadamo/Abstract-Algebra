package io.github.crisadamo

import io.github.crisadamo.algebra._
import io.github.crisadamo.container._
import io.github.crisadamo.examples.Statistics


object Main extends App {

  val data = {
    val rnd = new scala.util.Random
    (1 to 100).map { _ => rnd.nextInt(1000).toDouble }.toSeq
  }

  println("Stats Median: " + Statistics.mean(data.toVector))

  implicit val monoid = new CummulativeMovingAverageMonoid[Double]()

  data.zipWithIndex.scanLeft(Monoid.zero[AverageValue[Double]]) { (previous, data) =>
    val (value, time) = data
    val cma = Monoid.plus(previous, AverageValue(value))
    println("At %d: decayed=%f".format(time, cma.value))
    cma
  }
}
