package main.scala.core

/**
 * literals like boolean, string, integer, and double in lisp
 */
sealed trait Atom {
  
}

case class AtomBoolean (value: Boolean) extends Atom

case class AtomInt (value: Int) extends Atom

case class AtomDouble (value: Double) extends Atom

case class AtomString (value: String) extends Atom