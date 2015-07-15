package io.github.crisadamo.algebra

import annotation.implicitNotFound


@implicitNotFound("No member of type class Monoid in scope for ${T}")
trait Monoid[@specialized(Int, Long, Float, Double) T] extends Semigroup[T] {
  def zero: T
  def sum(vs: TraversableOnce[T]): T = sumOption(vs).getOrElse(zero)
  def isNonZero(v: T): Boolean = (v != zero)
  def nonZeroOption(v: T): Option[T] = {
    if (isNonZero(v)) Some(v)
    else None
  }
}


object Monoid {
  def zero[T](implicit mon: Monoid[T]) = mon.zero
  def plus[T](l: T, r: T)(implicit mon: Monoid[T]): T = mon.plus(l, r)
  def from[T](z: => T)(f: (T, T) => T): Monoid[T] = new Monoid[T] {
    def zero: T = z
    def plus(l: T, r: T): T = f(l, r)
  }

  implicit def optionMonoid[T: Semigroup]: Monoid[Option[T]] = new OptionMonoid[T]
  implicit def listMonoid[T]: Monoid[List[T]] = new ListMonoid[T]
  implicit def seqMonoid[T]: Monoid[Seq[T]] = new SeqMonoid[T]
  implicit def setMonoid[T]: Monoid[Set[T]] = new SetMonoid[T]
  implicit val stringMonoid = new StringMonoid
}


class OptionMonoid[T](implicit semi: Semigroup[T]) extends Monoid[Option[T]] {
  override def zero = None
  override def plus(l: Option[T], r: Option[T]): Option[T] = (l, r) match {
    case (None, rh) => rh
    case (lh, None) => lh
    case (lh, rh) => Some(semi.plus(lh.get, rh.get))
  }

  override def sumOption(iter: TraversableOnce[Option[T]]): Option[Option[T]] =
    if (iter.isEmpty) None
    else Some(semi.sumOption(iter.filter(_.isDefined).map { _.get }))
}


class ListMonoid[T] extends Monoid[List[T]] {
  override def zero = List[T]()
  override def plus(l: List[T], r: List[T]): List[T] = r ++ l
  override def sumOption(iter: TraversableOnce[List[T]]): Option[List[T]] =
    if (iter.isEmpty) None
    else {
      val builder = List.newBuilder[T]
      iter.foreach { builder ++= _ }
      Some(builder.result())
    }
}


class SeqMonoid[T] extends Monoid[Seq[T]] {
  override def zero = Seq[T]()
  override def plus(l: Seq[T], r: Seq[T]): Seq[T] = l ++ r
  override def sumOption(iter: TraversableOnce[Seq[T]]): Option[Seq[T]] =
    if (iter.isEmpty) None
    else {
      val builder = Seq.newBuilder[T]
      iter.foreach { builder ++= _ }
      Some(builder.result())
    }
}


class SetMonoid[T] extends Monoid[Set[T]] {
  override def zero = Set[T]()
  override def plus(l: Set[T], r: Set[T]): Set[T] = l ++ r
  override def sumOption(iter: TraversableOnce[Set[T]]): Option[Set[T]] =
    if (iter.isEmpty) None
    else {
      val mutable = scala.collection.mutable.Set[T]()
      iter.foreach { s => mutable ++= s }
      Some(mutable.toSet)
    }
}


class StringMonoid extends Monoid[String] {
  override def zero = ""
  override def plus(l: String, r: String): String = l + r
  override def sumOption(iter: TraversableOnce[String]): Option[String] =
    if (iter.isEmpty) None
    else Some(iter.mkString(""))
}