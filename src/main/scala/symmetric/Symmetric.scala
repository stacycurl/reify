package symmetric

import scala.language.postfixOps

import reify.internal.prelude.RListSyntax
import reify.{Reified, Reify}
import symmetric.{SmartJoiner => <#>, Symmetric => <->}

import scala.reflect.ClassTag


trait Symmetric[A, B] {
  val extractTo: Extractor[A, B] = Extractor.from[A].apply(to)
  val extractFrom: Extractor[B, A] = Extractor.from[B].apply(from)

  def to(a: A): Option[B]
  def from(b: B): Option[A]
  
  final def lhsTo[Base: ClassTag](implicit CT: ClassTag[A]): Base <-> B = {
    val IsA:    Extractor[Any, A]    = Extractor.isA[A]
    val IsBase: Extractor[Any, Base] = Extractor.isA[Base]
    
    val baseOptB: Base => Option[B] = {
      case IsA(a) => to(a)
      case _      => None
    } 
    
    val bOptBase: B => Option[Base] = b => from(b) match {
      case Some(IsA(IsBase(base))) => Some(base) // Doing this instead of 'Base :> A' to make calling easier
      case _                       => None
    }
    
    Symmetric.fromOpt[Base, B](baseOptB).toOpt(bOptBase)
  }
  
  final def flip: B <-> A = Symmetric.Flipped(this)

  final def |||(other: A <-> B): A <-> B = Symmetric.Or(this, other)

  final def ***[C](other: A <-> C): A <-> (B, C) = Symmetric.Forked(this, other)

  final def &:[C, D](other: C <-> D): (C, A) <-> (D, B) = Symmetric.And(this, other)

  final def <->[C](other: B <-> C): A <-> C = Symmetric.Joined(this, other)
  
  final def first[C]: (A, C) <-> (B, C) = this &: Symmetric.identity[C]
  
  final def second[C]: (C, A) <-> (C, B) = Symmetric.identity[C] &: this

  final def smartJoin[C, D, Out](other: C <-> D)(implicit SJ: SmartJoiner.Aux[A <-> B, C <-> D, Out]): Out =
    SJ.smartJoin(this, other)

  final def xmap[C](bc: B => C)(cb: C => B): A <-> C = Symmetric.XMapped[A, B, C](this, bc, cb)
}

object Symmetric {
  trait Companion[A, Decomposed] extends (A <-> Decomposed) {
    def apply[B](rhs: Decomposed <-> B)(implicit J: (A <-> Decomposed) <#> (Decomposed <-> B)): J.Out = J.smartJoin(this, rhs)
  }

  val string: String <-> String = identity[String]

  implicit class SymmetricTupleSyntax[A, B, C](private val self: A <-> (B, C)) extends AnyVal {
    def joinFirst[D](bd: B <-> D): A <-> (D, C) = self <-> bd.first[C]
    def joinSecond[D](cd: C <-> D): A <-> (B, D) = self <-> cd.second[B]
  }
  
  implicit class SymmetricTuple1stUnitSyntax[A, B](private val self: A <-> (Unit, B)) extends AnyVal {
    def _2: A <-> B = self.xmap[B](_._2)(() -> _)
  }

  implicit class SymmetricTuple2ndUnitSyntax[A, B](private val self: A <-> (B, Unit)) extends AnyVal {
    def _1: A <-> B = self.xmap[B](_._1)(_ -> ())
  }

  implicit class SymmetricToListSyntax[A, B](private val self: A <-> List[B]) extends AnyVal {
    def map[C](bc: B <-> C): A <-> List[C] =
      fromOpt[A, List[C]](a => self.to(a).map(bs => bs.flatMap(bc.to)))
        .toOpt(cs => self.from(cs.flatMap(bc.from)))
  }

  trait FromExtractor[A, B] extends (A <-> B) with Extractor[A, B] {
    final def to(a: A): Option[B]   = unapply(a)
    final def from(b: B): Option[A] = Some(apply(b))

    def apply(b: B): A
  }

  def ignore[A](value: A): A <-> Unit = equiv[A, Unit](value, ())

  def equiv[A, B](a: A, b: B): A <-> B = Equiv(a, b)

  def identity[A]: A <-> A = new Identity[A]

  case class from[A, B](pf: PartialFunction[A, B]) {
    def to(f: PartialFunction[B, A]): A <-> B = FromFunctions[A, B](pf.lift, f.lift)
  }

  case class fromOpt[A, B](f: A => Option[B]) {
    def toOpt(g: B => Option[A]): A <-> B = FromFunctions[A, B](f, g)
  }

  case class iso[A, B](f: A => B) {
    def to(g: B => A): A <-> B = FromIso[A, B](f, g)
  }
  
  def fromReify[A](implicit A: Reify[A]): A <-> Reified =
    Symmetric.fromOpt[A, Reified](a => Some(A.reify(a))).toOpt(A.reflect)
  
  def forSealedTrait[A, B](abs: (A <-> B)*): A <-> B = {
    val tos:   List[A => Option[B]] = abs.toList.map(ab => ab.to(_))
    val froms: List[B => Option[A]] = abs.toList.map(ab => ab.from(_))
     
    Symmetric.fromOpt[A, B](a => {
      tos.flatMapFirst(f => f(a))
    }).toOpt(b => {
      froms.flatMapFirst(f => f(b))
    })
  }
  

  private case class FromIso[A, B](f: A => B, g: B => A) extends (A <-> B) {
    def to(a: A):   Option[B] = Some(f(a))
    def from(b: B): Option[A] = Some(g(b))
  }

  private case class FromFunctions[A, B](ab: A => Option[B], ba: B => Option[A]) extends (A <-> B) {
    def to(a: A):   Option[B] = ab(a)
    def from(b: B): Option[A] = ba(b)
  }

  private class Identity[A] extends (A <-> A) {
    def to(a: A):   Option[A] = Some(a)
    def from(a: A): Option[A] = Some(a)
  }

  private case class Equiv[A, B](a: A, b: B) extends (A <-> B) {
    def to(a: A):   Option[B] = Some(b).filter(_ => a == this.a)
    def from(b: B): Option[A] = Some(a).filter(_ => b == this.b)
  }

  private case class XMapped[A, B, C](from: A <-> B, bc: B => C, cb: C => B) extends (A <-> C) {
    def to(a: A):   Option[C] = from.to(a).map(bc)
    def from(c: C): Option[A] = from.from(cb(c))
  }

  private case class Or[A, B](fst: A <-> B, snd: A <-> B) extends (A <-> B) {
    def to(a: A):   Option[B] = fst.to(a).orElse(snd.to(a))
    def from(b: B): Option[A] = fst.from(b).orElse(snd.from(b))
  }

  case class And[A, B, C, D](ab: A <-> B, cd: C <-> D) extends ((C, A) <-> (D, B)) {
    def to(ac: (C, A)): Option[(D, B)] = for {
      b <- ab.to(ac._2)
      d <- cd.to(ac._1)
    } yield (d, b)

    def from(bd: (D, B)): Option[(C, A)] = for {
      a <- ab.from(bd._2)
      c <- cd.from(bd._1)
    } yield (c, a)
  }

  private case class Joined[A, B, C](ab: A <-> B, bc: B <-> C) extends (A <-> C) {
    def to(a: A): Option[C] = for {
      b <- ab.to(a)
      c <- bc.to(b)
    } yield c

    def from(c: C): Option[A] = for {
      b <- bc.from(c)
      a <- ab.from(b)
    } yield a
  }

  private case class Forked[A, B, C](ab: A <-> B, ac: A <-> C) extends (A <-> (B, C)) {
    def to(a: A): Option[(B, C)] = for {
      b <- ab.to(a)
      c <- ac.to(a)
    } yield (b, c)

    def from(bc: (B, C)): Option[A] = for {
      a1 <- ab.from(bc._1)
      a2 <- ac.from(bc._2)
      if a1 == a2
    } yield a1
  }

  private case class Flipped[A, B](ab: A <-> B) extends (B <-> A) {
    def to(b: B): Option[A]   = ab.from(b)
    def from(a: A): Option[B] = ab.to(a)
  }
}


trait SmartJoiner[LHS <: Symmetric[_, _], RHS <: Symmetric[_, _]] {
  type Out

  def smartJoin(lhs: LHS, rhs: RHS): Out
}

object SmartJoiner extends SmartJoiner0 {
  implicit def joinPairWithOneWithUnit1[A, B, C, D]: Aux[A <-> (B, C), (B, C) <-> (D, Unit), A <-> D] = {
    new ((A <-> (B, C)) <#> ((B, C) <-> (D, Unit))) {
      type Out = A <-> D

      def smartJoin(a_bc: A <-> (B, C), bc_dunit: (B, C) <-> (D, Unit)): A <-> D = (a_bc <-> bc_dunit) _1
    }
  }

  implicit def joinPairWithOneWithUnit2[A, B, C, D]: Aux[A <-> (B, C), (B, C) <-> (Unit, D), A <-> D] = {
    new ((A <-> (B, C)) <#> ((B, C) <-> (Unit, D))) {
      type Out = A <-> D

      def smartJoin(a_bc: A <-> (B, C), bc_dunit: (B, C) <-> (Unit, D)): A <-> D = (a_bc <-> bc_dunit) _2
    }
  }

  implicit def lift[From, A, D, E, X](
    implicit aux: Aux[From <-> X, X <-> (E, Unit), From <-> E]
  ): Aux[
    From   <-> (A, X),
    (A, X) <-> (D, (E, Unit)),
    From   <-> (D, E)
  ] = {
    new <#>[
      From   <-> (A, X),
      (A, X) <-> (D, (E, Unit))
    ] {
      type Out = From <-> (D, E)

      def smartJoin(
        lhs: From <-> (A, X),
        rhs: (A, X) <-> (D, (E, Unit))
      ): From <-> (D, E) = 
        lhs <-> rhs.joinSecond(Symmetric.identity[(E, Unit)]._1)
    }
  }
}

trait SmartJoiner0 {
  type Aux[LHS <: Symmetric[_, _], RHS <: Symmetric[_, _], Out0] = SmartJoiner[LHS, RHS] {
    type Out = Out0
  }

  implicit def simple[A, B, C]: Aux[A <-> B, B <-> C, A <-> C] = {
    new ((A <-> B) <#> (B <-> C)) {
      type Out = A <-> C

      def smartJoin(ab: A <-> B, bc: B <-> C): A <-> C = ab <-> bc
    }
  }
}