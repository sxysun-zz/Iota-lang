package main.scala.core

object Operations {
  
  def ddplus = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value + r.value)
  def ddminus = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value - r.value)
  def dddivide = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value / r.value)
  def ddmulti = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value * r.value)
  def ddmod = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value % r.value)
  
  def iiplus = (l: AtomInt, r: AtomInt) => AtomInt(l.value + r.value)
  def iiminus = (l: AtomInt, r: AtomInt) => AtomInt(l.value - r.value)
  def iidivide = (l: AtomInt, r: AtomInt) => AtomInt(l.value / r.value)
  def iimulti = (l: AtomInt, r: AtomInt) => AtomInt(l.value * r.value)
  def iimod = (l: AtomInt, r: AtomInt) => AtomInt(l.value % r.value)
}