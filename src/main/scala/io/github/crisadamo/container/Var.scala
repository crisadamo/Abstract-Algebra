package io.github.crisadamo.container


trait Var[+T] {
  val name: String
  val value: T
  val varType: VarType
  val tableIndex: Int
  val columnName: String
  def header = name
}

sealed trait VarType
case object YearlyType extends VarType
case object MonthlyType extends VarType

object Var { self =>

  def apply[T](v: T, _name: String, colName: String, tIndex: Int, vType: VarType) = new Var[T] {
    override val name: String = _name
    override lazy val value: T = v
    override val columnName: String = colName
    override val tableIndex: Int = tIndex
    override val varType: VarType = vType
  }
}



