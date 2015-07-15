package io.github.crisadamo.container

import io.github.crisadamo.algebra.OptionMonoid

/*
sealed trait CreditCardContractVar[T] extends Var[T] {
  override def header = "ctdc_" + name
}

object CreditCardContractVar {
  //implicit val behaviorScoreMonoid = new OptionMonoid[Float]
}

case class BehaviorScore(override val value: Option[Float]) extends CreditCardContractVar[Option[Float]] {
  val name: String = "CAL_TRIAD_AVG"
  val columnName: String = "CAL_TRIAD"
  val tableIndex: Int = 1
  val varType: VarType = MonthlyType
}

case class AcquisitionChannel(override val value: Option[String]) extends CreditCardContractVar[String] {
  val tableIndex = 1
  val name = "CAL_TRIAD"
  val columnName: String = "CAL_TRIAD"
  val varType = MonthlyType
}

case class FinalBalanceExtract(override val value: Option[Float]) extends CreditCardContractVar[Float] {
  val tableIndex = 1
  val name = "CAL_TRIAD"
  val varType = MonthlyType
  val columnName: String = "CAL_TRIAD"
}

case class CreditLimit(override val value: Option[Int]) extends CreditCardContractVar[Int] {
  val tableIndex = 1
  val name = "CAL_TRIAD"
  val varType = MonthlyType
  val columnName: String = "CAL_TRIAD"
}
*/
