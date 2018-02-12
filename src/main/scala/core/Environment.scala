package main.scala.core

sealed trait SymbolTable {}

case class TypeEnvironment () extends SymbolTable{
  import langType._
  private var m: Map[String, langType] = Map(
      "#f" -> boolean,
      "#t" -> boolean,
      "INF" -> int
  )
  
  def copyExternal(env: Environment) = {env.table.map(x => {
      m = m + (x._1 -> x._2.inferType(env))
    }); this}
  
  def copyExternal(env: TypeEnvironment) = {env.m.map(x => {
      this.m = this.m + (x._1 -> x._2)
    }); this}
  
  def extendEnvironment(name: String, attribute: langType): TypeEnvironment = {
    if(!m.contains(name)) m = m + (name -> attribute)
    else if (m(name) != attribute) m = m.updated(name, attribute); this
  }
  
  def lookUp(name: String): Either[String, langType] = {
    if(m.contains(name)) Right(m(name))
    else Left(s"the identifier $name is not found")
  }
  
  def size = m.size
}

/**
 * ``IMPORTANT`` the `Scala` map behaves like a stack
 */
case class Environment () extends SymbolTable {
  var table: Map[String, Atom] = Map(
      "#f" -> AtomBoolean(false),
      "#t" -> AtomBoolean(true),
      "INF" -> AtomInt(Int.MaxValue)
  )
  
  var functionTable: Map[String, LambdaExpression] = Map()
  
  def extendEnvironment(name: String, attribute: Atom): Environment = {
    if(table.contains(name)) table = table.updated(name, attribute)
    else table = table + (name -> attribute)
    this
  }
  
  def copyExternal(env: Environment) = {env.table.map(x => {
      this.table = this.table + (x._1 -> x._2)
    }); this}
  
  def lookUp(name: String): Either[String, Atom] = {
    if(table.contains(name)) Right(table(name))
    else Left(s"the identifier $name is not found")
  }
  
  def size = table.size
  
  import langType._
  def getType(name: String): Either[String, langType] = {
    if(table.contains(name)) Right(table(name).inferType(this))
    else Left(s"the identifier $name is not found")
  }
  
  def replPrint = {
    println("symbol table: ")
    println("-----")
    this.table.map(x => println(x._1+": "+x._2.inferType(this)+" -> "+x._2))
    println("-----")
  }
}

object typeReflex {
  import dfaState._
  import langType._
  val correspondingType: Map[dfaState, langType] = Map(
      dfaState.BOOLEAN -> langType.boolean,
      dfaState.INT -> langType.int,
      dfaState.STRING -> langType.string,
      dfaState.DOUBLE -> langType.double
  )
}