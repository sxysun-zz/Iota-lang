package main.scala.core

object Operations {
  
  private val epsilon = 0.0001
  
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
  
  def ddnumEquiv = (l: AtomDouble, r: AtomDouble) => AtomBoolean(Math.abs(l.value - r.value) < epsilon)
  def ddnumInEquiv = (l: AtomDouble, r: AtomDouble) => AtomBoolean(Math.abs(l.value - r.value) > epsilon)
  def ddGe = (l: AtomDouble, r: AtomDouble) => AtomBoolean(l.value > r.value)
  def ddSm = (l: AtomDouble, r: AtomDouble) => AtomBoolean(l.value < r.value)
  
  def or = (l: AtomBoolean, r: AtomBoolean) => AtomBoolean(l.value || r.value)
  def and = (l: AtomBoolean, r: AtomBoolean) => AtomBoolean(l.value && r.value)
  def equiv = (l: AtomBoolean, r: AtomBoolean) => AtomBoolean((l.value && r.value) || (!l.value && !r.value))
  def xor = (l: AtomBoolean, r: AtomBoolean) => AtomBoolean((l.value && !r.value) || (!l.value && r.value))
}