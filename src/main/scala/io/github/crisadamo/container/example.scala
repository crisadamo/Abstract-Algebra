package io.github.crisadamo.container

import io.github.crisadamo.algebra.OptionMonoid


sealed trait CreditCardContractVar[T] extends Var[T] {
  override def header = "ctdc_" + name
}

object CreditCardContractVar {
  implicit val behaviorScoreMonoid = new OptionMonoid[Float]
}

case class BehaviorScore(v: Option[Float]) extends CreditCardContractVar[Option[Float]] {
  override val name: String = "CAL_TRIAD_AVG"
  override lazy val value: Option[Float] = v
  override val columnName: String = "CAL_TRIAD"
  override val tableIndex: Int = 1
}


