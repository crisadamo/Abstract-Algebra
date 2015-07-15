package io.github.crisadamo.algebra

import io.github.crisadamo.algebra.Monoid

trait Group[T] extends Monoid[T] {
  def inverse(v: T): T = minus(zero, v)
  def minus(l: T, r: T): T = plus(l, inverse(r))
}


object Group {
  def inverse[T](v: T)(implicit grp: Group[T]): T = grp.inverse(v)
  def minus[T](l: T, r: T)(implicit grp: Group[T]): T = grp.minus(l, r)
}


class OptionGroup[T](implicit group: Group[T]) extends OptionMonoid[T] with Group[Option[T]] {
  override def isNonZero(opt: Option[T]): Boolean =
    opt.exists{ group.isNonZero(_) }

  override def inverse(opt: Option[T]) =
    opt.map{ v => group.inverse(v) }
}