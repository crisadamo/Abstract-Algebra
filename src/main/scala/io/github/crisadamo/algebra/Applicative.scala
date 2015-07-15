package io.github.crisadamo.algebra

import annotation.implicitNotFound


@implicitNotFound("No member of type class Applicative in scope for ${M}")
trait Applicative[M[_]] extends Functor[M] {
  def apply[T](v: T): M[T]
  def join[T, U](mt: M[T], mu: M[U]): M[(T, U)]
  def joinWith[T, U, V](mt: M[T], mu: M[U])(fn: (T, U) => V): M[V] =
    map(join(mt, mu)) { case (t, u) => fn(t, u) }
}
