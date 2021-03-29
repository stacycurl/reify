package reify


sealed trait RType {
  def typeName: String

  final override def toString: String = typeName
}

object RType {
  def of[A: Reify]: RType = Reify.of[A].rtype
  
  def apply[A: Reify](name: String): RType = TC1[A](name, of[A])
  def apply[A: Reify, B: Reify](name: String): RType = TC2[A, B](name, of[A], of[B])
  def apply[A: Reify, B: Reify, C: Reify](name: String): RType = TC3[A, B, C](name, of[A], of[B], of[C])

  case class TC0(typeName: String) extends RType

  case class TC1[A: Reify](name: String, arg1: RType) extends RType {
    def typeName: String = s"$name[$arg1]"

    def reify1: Reify[A] = Reify.of[A]
  }

  case class TC2[A: Reify, B: Reify](name: String, arg1: RType, arg2: RType) extends RType {
    def typeName: String = s"$name[$arg1, $arg2]"

    def reify1: Reify[A] = Reify.of[A]
    def reify2: Reify[B] = Reify.of[B]
  }

  case class TC3[A: Reify, B: Reify, C: Reify](name: String, arg1: RType, arg2: RType, arg3: RType) extends RType {
    def typeName: String = s"$name[$arg1, $arg2, $arg3]"

    def reify1: Reify[A] = Reify.of[A]
    def reify2: Reify[B] = Reify.of[B]
    def reify3: Reify[C] = Reify.of[C]
  }
}
