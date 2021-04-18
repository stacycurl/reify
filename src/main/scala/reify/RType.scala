package reify

import reify.Reified.{RCaseClass, RString}
import reify.Token.TType


case class RType(name: String, args: List[RType]) {
  def tokenize: TType = TType(name, args.map(_.tokenize))
  
  val typeName: String = args match {
    case Nil  => name
    case many => s"$name[${many.mkString(", ")}]"
  }

  final override def toString: String = typeName
}

object RType extends Reify.Companion[RType] {
  object Typed {
    def apply[A](implicit A: Typed[A]): RType = A.value

    implicit def typedRTypeFromReify[A](implicit A: Reify[A]): Typed[A] = Typed[A](A.rtype)
  }
  
  case class Typed[A](value: RType)
  
  def fromTType(ttype: TType): RType =
    RType(ttype.name, ttype.args.map(fromTType))
  
  implicit val reifyRType: Reify[RType] = Reify.defer[RType] {
    Reify.apply[RType](
      RType("RType"), 
      {
        case RType(name, args) => RCaseClass(RType("RType", Nil), RString(name) :: args.map(reifyRType.reify(_)))
      }, 
      {
        case RCaseClass(RType("RType", Nil), RString(name) :: args) => {
          val rtypes = args.flatMap(reifyRType.reflect(_))
          
          if (rtypes.length == args.length) Some(RType(name, rtypes)) else None  
        } 
      }
    )
  }
  
  def of[A: Reify]: RType = Reify.of[A].rtype
  
  def create[A: Reify](name: String): RType = apply(name, of[A])
  def create[A: Reify, B: Reify](name: String): RType = apply(name, of[A], of[B])
  def create[A: Reify, B: Reify, C: Reify](name: String): RType = apply(name, of[A], of[B], of[C])
  def create[A: Reify, B: Reify, C: Reify, D: Reify](name: String): RType = apply(name, of[A], of[B], of[C], of[D])

  def apply(name: String, args: RType*): RType = RType(name, args.toList)
}
