package io.github.crisadamo.algebra


object AverageValue {
  def apply[T](item: T): AverageValue[T] = apply(Seq(item))
  def apply[T](items: TraversableOnce[T], cnt: Long, avg: Double) =
    new AverageValue[T](items) {
      override val count = cnt
      override val value = avg
    }
}

case class AverageValue[T](items: TraversableOnce[T]) {
  val count: Long = 0L
  val value: Double = 0.0
}

/**
 *  An = Average New
 *  Ao = Average Old
 *  Co = Count Old
 *  Cn = Count New
 *  Vn = New Value
 *  Sn = New Size
 *
 *  An = (Vn + Co * Ao) / Cn
 *
 */
class CummulativeMovingAverageMonoid[T](implicit fld: Field[T]) extends Monoid[AverageValue[T]] {
  override def zero: AverageValue[T] = AverageValue(Seq[T]())
  override def plus(l: AverageValue[T], r: AverageValue[T]) = {
    val (avgNew, avgOld) = if (r.count == 0L) (r, l) else (l, r)
    val newCnt = avgNew.count + avgNew.items.size.toLong
    val avg = cumAvg(fld.sum(avgNew.items), newCnt, avgOld.count, avgOld.value)

    AverageValue(zero.items, newCnt + avgOld.count, avg)
  }

  private def cumAvg(sum: T, newCnt: Long, oldCnt: Long, oldAvg: Double) =
    fld.divByAs[Double](fld.plusBy(sum, oldCnt * oldAvg), newCnt + oldCnt)
}


object CummulativeMovingAverageMonoid {
  def zero[T](implicit fld: Field[T]) = new CummulativeMovingAverageMonoid[T].zero
  def from[T](v: AverageValue[T])(implicit fld: Field[T]) =
    new CummulativeMovingAverageMonoid[T].plus(zero, v)
}