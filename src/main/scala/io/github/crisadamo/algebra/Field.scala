package io.github.crisadamo.algebra

import annotation.implicitNotFound


@implicitNotFound("No member of type class Field in scope for ${T}")
trait Field[@specialized(Int, Long, Float, Double) T] extends Ring[T] {
  override def inverse(v: T): T = {
    div(one, v)
  }

  def div(l: T, r: T): T = {
    times(l, inverse(r))
  }

  def divByInt(l: T, r: Int): T = div(l, r.asInstanceOf[T])
  def divAsInt(l: T, r: Int): Int = divByInt(l, r).asInstanceOf[Int]
}


object Field {
  def div[T](l: T, r: T)(implicit field: Field[T]) = field.div(l, r)

  implicit val intField: Field[Int] = IntField
  implicit val longField: Field[Long] = LongField
  implicit val floatField: Field[Float] = FloatField
  implicit val doubleField: Field[Double] = DoubleField
}


object IntField extends Field[Int] {
  override def zero: Int = 0
  override def one: Int = 1
  override def inverse(v: Int) = -v
  override def plus(l: Int, r: Int) = l + r
  override def minus(l: Int, r: Int) = l - r
  override def times(l: Int, r: Int) = l * r
  override def div(l: Int, r: Int) = l / r
}

object LongField extends Field[Long] {
  override def zero: Long = 0L
  override def one: Long = 1L
  override def inverse(v: Long) = -v
  override def plus(l: Long, r: Long) = l + r
  override def minus(l: Long, r: Long) = l - r
  override def times(l: Long, r: Long) = l * r
  override def div(l: Long, r: Long) = l / r
  //def div(l: Long, r: Int): Int = (l / r).toInt
}

object DoubleField extends Field[Double] {
  override def zero: Double = 0.0
  override def one: Double = 1.0
  override def inverse(v: Double) = -v
  override def plus(l: Double, r: Double) = l + r
  override def minus(l: Double, r: Double) = l - r
  override def times(l: Double, r: Double) = l * r
  override def div(l: Double, r: Double) = l / r
  //def div(l: Double, r: Int): Int = (l / r).toInt
}

object FloatField extends Field[Float] {
  override def zero: Float = 0.0f
  override def one: Float = 1.0f
  override def inverse(v: Float) = -v
  override def plus(l: Float, r: Float) = l + r
  override def minus(l: Float, r: Float) = l - r
  override def times(l: Float, r: Float) = l * r
  override def div(l: Float, r: Float) = l / r
  //def div(l: Float, r: Int): Int = (l / r).toInt
}