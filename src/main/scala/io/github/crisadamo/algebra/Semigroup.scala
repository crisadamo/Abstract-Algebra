package io.github.crisadamo.algebra

import annotation.implicitNotFound


@implicitNotFound("No member of type class Semigroup in scope for ${T}")
trait Semigroup[@specialized(Int, Long, Float, Double) T] extends java.io.Serializable {
  def plus(l: T, r: T): T
  def sumOption(iter: TraversableOnce[T]): Option[T] =
    iter.reduceLeftOption { plus(_, _) }
}


object Semigroup {
  def from[T](f: (T, T) => T): Semigroup[T] = new Semigroup[T] {
    def plus(l: T, r: T): T = f(l, r)
  }

  def plus[T](l: T, r: T)(implicit semi: Semigroup[T]) = semi.plus(l, r)
}
