package main.scala.core

trait PureExpression {
  val free: Set[String]
  val body: PureExpression
}

case class PureLambda (free: Set[String], body: PureExpression) extends PureExpression {
  
}

case class PureApplication (free: Set[String], body: PureExpression) extends PureExpression {
  
}

case class PureAtom (free: Set[String]) extends PureExpression {
  val body = null
}

