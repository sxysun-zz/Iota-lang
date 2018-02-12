package main.scala.core

object dfaState extends Enumeration {
    type dfaState = Value
    val START, END, 
    SPACE,
    DEFINE,
    STRING, INT, DOUBLE, BOOLEAN, IDENTIFIER, LAMBDA,
    IF,
    LPAREN, RPAREN, 
    LCOMMENT, BCOMMENT, 
    ARITHOPERATOR, BOOLOPERATOR, COMPOPERATOR, LOGICOPERATOR = Value
}

import dfaState._
case class Token (kind: dfaState, value: String, property: MetaData){
  override def toString() = s"$value: $kind"
}