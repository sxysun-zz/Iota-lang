package main.scala.core

/**
 * literals like boolean, string, integer, and double in lisp
 */
sealed trait Atom

case class LiteralBoolean (value: Boolean) extends Atom

case class LiteralInt (value: Int) extends Atom

case class LiteralDouble (value: Double) extends Atom

case class LiteralString (value: String) extends Atom