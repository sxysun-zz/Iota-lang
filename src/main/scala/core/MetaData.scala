package main.scala.core

case class MetaData (lineNumber: Int, colNumber: Int){
  override def toString = {
    "line number : " + lineNumber + " | absolute position : " + colNumber
  }
}