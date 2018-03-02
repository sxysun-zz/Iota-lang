package main.scala.core

import main.scala.util._

case class MetaData (lineNumber: Int, colNumber: Int){
  
  override def toString = {
    "line number : " + lineNumber + " | absolute position : " + colNumber
  }
}