package io.github.crisadamo.algebra

import annotation.implicitNotFound


@implicitNotFound("No member of type class Ring in scope for ${T}")
trait Ring[@specialized(Int, Long, Float, Double) T] extends Group[T] {
  def one: T
  def times(l: T, r: T): T
}

object Ring {
  def times[T](l: T, r: T)(implicit rng: Ring[T]) = rng.times(l, r)

  implicit val booleanRing: Ring[Boolean] = BooleanRing
}


object BooleanRing extends Ring[Boolean] {
  override def zero = false
  override def one = true
  override def inverse(v: Boolean) = v
  override def plus(l: Boolean, r: Boolean) = l ^ r
  override def minus(l: Boolean, r: Boolean) = l ^ r
  override def times(l: Boolean, r: Boolean) = l && r
}
