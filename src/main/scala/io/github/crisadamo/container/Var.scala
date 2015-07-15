package io.github.crisadamo.container


trait Var[+T] {
  val name: String
  val value: T
  val tableIndex: Int
  val columnName: String
  def header = name
}

object Var {
  def apply[T](v: T, name: String, colName: String, tIndex: Int) = new Var[T] {
    override val name: String = name
    override lazy val value: T = v
    override val columnName: String = colName
    override val tableIndex: Int = tIndex
  }
}



