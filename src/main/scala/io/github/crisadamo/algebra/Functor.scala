package io.github.crisadamo.algebra


trait Functor[M[_]] {
  def map[T, U](m: M[T])(fn: (T) => U): M[U]
}
