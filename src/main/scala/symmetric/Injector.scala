package symmetric

import scala.collection.immutable.{Nil => LNil}
import scala.util.Try


trait Injector[A, B] extends Extractor[B, A] { self =>
  def inject(a: A): B
  
  final def andThen[C](next: Injector[B, C]): Injector[A, C] = Injector.AndThen(this, next)
  
  final def &:[C, D](cd: Injector[C, D]): Injector[(C, A), (D, B)] = Injector.And(this, cd)

  final def withValue[C](value: C): Injector[A, (C, B)] = xmap(b => (value, b)).pf {
    case (`value`, b) => b
  }

  case class xmap[C](bc: B => C) {
    def pf(cb: PartialFunction[C, B]): Injector[A, C] = apply(cb.lift)
    def apply(cb: C => Option[B]): Injector[A, C] = Injector.XMapped(self, bc, cb)
  }
}

object Injector {
  object Nil extends Injector[Unit, List[Nothing]] {
    def inject(u: Unit): List[Nothing] = LNil
    def unapply(l: List[Nothing]): Option[Unit] = if (l.isEmpty) Some(()) else None
    
    def ::[A, E](head: Injector[A, E]): Injector[A, List[E]] = head.xmap[List[E]](head => head  :: LNil).pf {
      case head :: LNil => head
    }
  } 
  
  val Boolean: Injector[Boolean, String] = from[Boolean].attempt[String](_.toString)(_.toBoolean)
  val Int:     Injector[Int, String]     = from[Int].attempt[String](_.toString)(_.toInt)
  val Long:    Injector[Long, String]    = from[Long].attempt[String](value => s"${value}L")(_.toLong)
  
  implicit class InjectorSyntax[A, B](private val self: Injector[A, B]) extends AnyVal {
    def apply(value: A): B = self.inject(value)
  }

  implicit class InjectorFromListSyntax[E, A](private val self: Injector[List[E], A]) extends AnyVal {
    def apply(values: E*): A = self.inject(values.toList)

    def flatMap[C](ec: Injector[C, E]): Injector[List[C], A] = {
      from[List[C]].apply[A](
        inject = cs => self.inject(cs.map(ec.inject))
      )(
        extract = b => {
          for {
            as <- self.unapply(b)
            cs = as.flatMap(ec.unapply)
            if cs.length == as.length
          } yield cs
        }
      )
    }
  }
  
  implicit class InjectorToListSyntax[A, E](private val self: Injector[A, List[E]]) extends AnyVal {
    def ::[B](head: Injector[B, E]): Injector[(B, A), List[E]] = {
      (head &: self).xmap[List[E]] {
        case (head, tail) => head :: tail
      }.pf {
        case head :: tail => (head, tail)
      }
    }
  }
  
  def id[A]: Injector[A, A] = from[A].apply[A](identity[A] _)(Some(_))

  def from[A]: From[A] = new From[A]

  class From[A] {
    def attempt[B](inject: A => B)(extract: B => A): Injector[A, B] =
      apply(inject)(b => Try(extract(b)).toOption)

    def pf[B](inject: A => B)(extract: PartialFunction[B, A]): Injector[A, B] =
      apply(inject)(extract.lift)
    
    def apply[B](inject: A => B)(extract: B => Option[A]): Injector[A, B] =
      FromFunctions[A, B](inject, extract)
  }

  private case class FromFunctions[A, B](inject0: A => B, extract: B => Option[A]) extends Injector[A, B] {
    def inject(a: A): B = inject0(a)
    def unapply(b: B): Option[A] = extract(b)
  }
  
  private case class XMapped[A, B, C](from: Injector[A, B], bc: B => C, cb: C => Option[B]) extends Injector[A, C] {
    def inject(a: A): C = bc(from.inject(a))
    def unapply(c: C): Option[A] = cb(c).flatMap(from.unapply)
  }
  
  private case class AndThen[A, B, C](lhs: Injector[A, B], rhs: Injector[B, C]) extends Injector[A, C] {
    def inject(a: A): C = rhs.inject(lhs.inject(a))

    def unapply(c: C): Option[A] = for {
      b <- rhs.unapply(c)
      a <- lhs.unapply(b)
    } yield a
  }
  
  private case class And[A, B, C, D](ab: Injector[A, B], cd: Injector[C, D]) extends Injector[(C, A), (D, B)] {
    def inject(ca: (C, A)): (D, B) = {
      val (c, a) = ca
      
      (cd.inject(c), ab.inject(a))
    }

    def unapply(db: (D, B)): Option[(C, A)] = {
      val (d, b) = db
      
      for {
        a <- ab.unapply(b)
        c <- cd.unapply(d)
      } yield (c, a)
    }
  }
}
