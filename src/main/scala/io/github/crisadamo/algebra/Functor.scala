package io.github.crisadamo.algebra

import annotation.implicitNotFound


@implicitNotFound("No member of type class Functor in scope for ${M}")
trait Functor[M[_]] {
  def map[T, U](m: M[T])(fn: (T) => U): M[U]
}
