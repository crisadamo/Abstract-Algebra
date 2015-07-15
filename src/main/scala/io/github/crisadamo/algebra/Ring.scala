package io.github.crisadamo.algebra


trait Ring[T] extends Group[T] {
  def one: T
  def times(l: T, r: T): T
}


object IntRing extends Ring[Int] {
  override def zero: Int = 0
  override def one: Int = 1
  override def inverse(v: Int) = -v
  override def plus(l: Int, r: Int) = l + r
  override def minus(l: Int, r: Int) = l - r
  override def times(l: Int, r: Int) = l * r
}


object LongRing extends Ring[Long] {
  override def zero: Long = 0L
  override def one: Long = 1L
  override def inverse(v: Long) = -v
  override def plus(l: Long, r: Long) = l + r
  override def minus(l: Long, r: Long) = l - r
  override def times(l: Long, r: Long) = l * r
}


object DoubleRing extends Ring[Double] {
  override def zero: Double = 0.0
  override def one: Double = 1.0
  override def inverse(v: Double) = -v
  override def plus(l: Double, r: Double) = l + r
  override def minus(l: Double, r: Double) = l - r
  override def times(l: Double, r: Double) = l * r
}


object FloatRing extends Ring[Float] {
  override def zero: Float = 0.0f
  override def one: Float = 1.0f
  override def inverse(v: Float) = -v
  override def plus(l: Float, r: Float) = l + r
  override def minus(l: Float, r: Float) = l - r
  override def times(l: Float, r: Float) = l * r
}


object BooleanRing extends Ring[Boolean] {
  override def zero = false
  override def one = true
  override def inverse(v: Boolean) = v
  override def plus(l: Boolean, r: Boolean) = l ^ r
  override def minus(l: Boolean, r: Boolean) = l ^ r
  override def times(l: Boolean, r: Boolean) = l && r
}
