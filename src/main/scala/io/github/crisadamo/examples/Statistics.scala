package io.github.crisadamo.examples

import io.github.crisadamo.algebra._

import scala.util.Try


object Statistics {
  def median[T: Field](xs: Vector[T]): T = xs(xs.size / 2)

  def quartiles[T: Field](xs: Vector[T]): (T, T, T) =
    ( xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3) )

  def iqr[T](xs: Vector[T])(implicit fld: Field[T]): T = quartiles(xs) match {
    case (lowerQuartile, _, upperQuartile) => fld.minus(upperQuartile, lowerQuartile)
  }

  def mean[T](xs: Vector[T])(implicit fld: Field[T]): T =
    fld.divByInt(xs.reduce(fld.plus(_, _)), xs.size)
}


case class StatisticsMean[T](values: TraversableOnce[T])(implicit fld: Field[T]) {
  self =>

  val calculatedLength = 0
  val sum = fld.sum(values)

  def median = fld.divByInt(sum, length)

  def length = Try(values.size).toOption.getOrElse(0) + calculatedLength

  def +(that: StatisticsMean[T]): StatisticsMean[T] = new StatisticsMean[T](Seq[T]()) {
    override val calculatedLength = self.length + that.length
    override val sum = fld.plus(fld.sum(self.values), fld.sum(that.values))
  }
}

class MedianMonoid[T](implicit fld: Field[T]) extends Monoid[StatisticsMean[T]] {
  override def zero: StatisticsMean[T] = new StatisticsMean(Seq[T]())
  override def plus(l: StatisticsMean[T], r: StatisticsMean[T]): StatisticsMean[T] = l + r
}