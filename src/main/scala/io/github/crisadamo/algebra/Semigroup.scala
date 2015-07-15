package io.github.crisadamo.algebra

trait Semigroup[T] extends java.io.Serializable {
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
