package main.scala.core

/**
 * literals like boolean, string, integer, and double in lisp
 */
sealed trait Atom 

import langType._

/**
 * represented by 真 and 假
 */
case class AtomBoolean (value: Boolean) extends Atom {
  val typeInfo = boolean
}

case class AtomInt (value: Int) extends Atom {
  val typeInfo = int
}

case class AtomDouble (value: Double) extends Atom {
  val typeInfo = double
}

case class AtomString (value: String) extends Atom {
  val typeInfo = string
}