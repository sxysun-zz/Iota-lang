package main.scala.core

object Operations {
  def ddplus = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value + r.value)
  def ddminus = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value - r.value)
  def dddivide = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value / r.value)
  def ddmulti = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value * r.value)
  def ddmod = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value % r.value)
  
  def diplus = (l: AtomDouble, r: AtomInt) => AtomDouble(l.value + r.value)
  def diminus = (l: AtomDouble, r: AtomInt) => AtomDouble(l.value - r.value)
  def didivide = (l: AtomDouble, r: AtomInt) => AtomDouble(l.value / r.value)
  def dimulti = (l: AtomDouble, r: AtomInt) => AtomDouble(l.value * r.value)
  def dimod = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value % r.value)
  
  def idplus = (l: AtomInt, r: AtomDouble) => AtomDouble(l.value + r.value)
  def idminus = (l: AtomInt, r: AtomDouble) => AtomDouble(l.value - r.value)
  def iddivide = (l: AtomInt, r: AtomDouble) => AtomDouble(l.value / r.value)
  def idmulti = (l: AtomInt, r: AtomDouble) => AtomDouble(l.value * r.value)
  def idmod = (l: AtomDouble, r: AtomDouble) => AtomDouble(l.value % r.value)
  
  def iiplus = (l: AtomInt, r: AtomInt) => AtomInt(l.value + r.value)
  def iiminus = (l: AtomInt, r: AtomInt) => AtomInt(l.value - r.value)
  def iidivide = (l: AtomInt, r: AtomInt) => AtomInt(l.value / r.value)
  def iimulti = (l: AtomInt, r: AtomInt) => AtomInt(l.value * r.value)
  def iimod = (l: AtomInt, r: AtomInt) => AtomInt(l.value % r.value)
  
  /*
  implicit def ddtoAtom(f: (AtomDouble, AtomDouble) => AtomDouble): (Atom, Atom) => Atom = {
    
  }
  * 
  */
}