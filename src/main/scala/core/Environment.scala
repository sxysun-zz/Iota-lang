package main.scala.core

case class TypeEnvironment () {
  import langType._
  private var m: Map[String, langType] = Map(
      "#f" -> boolean,
      "#t" -> boolean,
      "INF" -> int
  )
  
  def copyExternal(env: Environment) = {env.table.map(x => {
      m = m + (x._1 -> x._2.inferType(env))
    }); this}
  
  def extendEnvironment(name: String, attribute: langType): TypeEnvironment = {
    if(!m.contains(name)){
      m = m + (name -> attribute)
    } else if (m(name) != attribute) {
      //m(name) = attribute
      m = m + (name -> attribute)
    } else {}
    
    this
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
case class Environment () {
  var table: Map[String, Atom] = Map(
      "#f" -> AtomBoolean(false),
      "#t" -> AtomBoolean(true),
      "INF" -> AtomInt(0xffff)
  )
  
  def extendEnvironment(name: String, attribute: Atom) = {
    table = table + (name -> attribute)
  }
  
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
}