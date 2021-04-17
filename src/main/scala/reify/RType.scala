package reify


case class RType(name: String, args: List[RType]) {
  val typeName: String = args match {
    case Nil  => name
    case many => s"$name[${many.mkString(", ")}]"
  }

  final override def toString: String = typeName
}

object RType {
  def of[A: Reify]: RType = Reify.of[A].rtype
  
  def create[A: Reify](name: String): RType = apply(name, of[A])
  def create[A: Reify, B: Reify](name: String): RType = apply(name, of[A], of[B])
  def create[A: Reify, B: Reify, C: Reify](name: String): RType = apply(name, of[A], of[B], of[C])

  def apply(name: String, args: RType*): RType = RType(name, args.toList)
}
