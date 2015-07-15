package io.github.crisadamo

import io.github.crisadamo.algebra._
import io.github.crisadamo.container._
import io.github.crisadamo.examples.{StatisticsMean, MedianMonoid, Statistics}


object Main extends App {

  val numbers = Vector[Double](13, 23.0, 42, 45, 61, 73, 96, 100, 199, 420, 900, 3839)
  println("Stats Median: " + Statistics.mean(numbers))

  val monoid = new MedianMonoid[Double]
  val num = numbers

  val s1 = StatisticsMean(numbers.take(3))
  val s2 = StatisticsMean(numbers.drop(3).take(3))
  val s3 = StatisticsMean(numbers.drop(6).take(3))
  val s4 = StatisticsMean(numbers.drop(9).take(3))

  val sm1 = monoid.plus(s1, s2)
  val sm2 = monoid.plus(sm1, s3)
  val sm = monoid.plus(sm2, s4)

  println("Median: " + sm.median + " l: " + sm.length + " s:" + sm.sum + " cl: " + sm.calculatedLength)
}
