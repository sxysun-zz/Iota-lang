package main.scala.core

object dfaState extends Enumeration {
    type dfaState = Value
    val START, END, STRING, INT, DOUBLE, BOOLEAN, IDENTIFIER, 
    LPAREN, RPAREN, LCOMMENT, BCOMMENT, BINOPERATOR, SPACE, BOOLOPERATOR = Value
}

import dfaState._
case class Token (kind: dfaState, value: String, property: MetaData){
  
}