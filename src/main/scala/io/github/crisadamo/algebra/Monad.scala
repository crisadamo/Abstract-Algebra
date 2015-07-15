package io.github.crisadamo.algebra

import annotation.implicitNotFound


@implicitNotFound("No member of type class Monad in scope for ${M}")
trait Monad[M[_]] extends Applicative[M] {
  def flatMap[T, U](m: M[T])(fn: (T) => M[U]): M[U]
  override def map[T, U](m: M[T])(fn: (T) => U): M[U] = flatMap(m)((t: T) => apply(fn(t)))
  override def join[T, U](mt: M[T], mu: M[U]): M[(T, U)] = flatMap(mt) { (t: T) =>
    map(mu) { (u: U) => (t, u) }
  }
}
