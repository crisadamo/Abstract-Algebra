package io.github.crisadamo.examples

import io.github.crisadamo.algebra._


object Statistics {
  def median[T: Field](xs: IndexedSeq[T]): T = xs(xs.size / 2)
  def quartiles[T: Field](xs: IndexedSeq[T]): (T, T, T) =
    ( xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3) )

  def iqr[T](xs: IndexedSeq[T])(implicit fld: Field[T]): T = quartiles(xs) match {
    case (lowerQuartile, _, upperQuartile) => fld.minus(upperQuartile, lowerQuartile)
  }

  def mean[T](xs: IndexedSeq[T])(implicit fld: Field[T]): T =
    fld.divBy[Int](xs.reduce(fld.plus(_, _)), xs.size)
}

