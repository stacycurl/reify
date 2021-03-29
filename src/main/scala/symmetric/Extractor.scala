package symmetric

import scala.language.implicitConversions

import scala.reflect.ClassTag
import scala.util.Try


trait Extractor[-A, +B] {
  def unapply(a: A): Option[B]

  def all: Extractor[List[A], List[B]] = Extractor.all(this)

  final def when(p: B => Boolean): Extractor[A, B] =
    Extractor.When(this, p)

  final def map[C](f: B => C): Extractor[A, C] =
    Extractor.Mapped(this, f)

  final def attempt[C](f: B => C): Extractor[A, C] =
    Extractor.AndThen(this, (b: B) => Try(f(b)).toOption)

  final def andThen[C](bc: Extractor[B, C]): Extractor[A, C] =
    Extractor.AndThen(this, bc)

  final def contramap[Z](za: Z => A): Extractor[Z, B] =
    Extractor.Contramapped(this, za)
}

object Extractor {
  implicit class ExtractorSyntax[A, B](private val self: Extractor[A, B]) extends AnyVal {
    def log(name: String): Extractor[A, B] = tap(
      around = a => optB => println(s"$name: $a => $optB")
    )

    def tap(
      before: A => Unit = _ => {},
      after: Option[B] => Unit = _ => {},
      around: A => Option[B] => Unit = _ => _ => {}
    ): Extractor[A, B] =
      Tapped(self, before, after, around)

    def flatMap[C](f: B => Extractor[A, C]): Extractor[A, C] =
      FlatMapped(self, f)

    def orElse(rhs: Extractor[A, B]): Extractor[A, B] =
      OrElse(self, rhs)

    def toOption: Extractor[Option[A], Option[B]] =
      from[Option[A]].property(optA => optA.flatMap(self.unapply))
  }

  implicit class ExtractorFromListSyntax[E, B](private val self: Extractor[List[E], B]) extends AnyVal {
    def ^:[A](head: Extractor[E, A]): Extractor[List[E], (A, B)] = Cons[A, B, E](head, self)
  }

  private case class Cons[A, B, E](head: Extractor[E, A], tail: Extractor[List[E], B]) extends Extractor[List[E], (A, B)] {
    def unapply(es: List[E]): Option[(A, B)] = PartialFunction.condOpt(es) {
      case head(a) :: tail(b) => (a, b)
    }
  }

  implicit def fromFn[A, B](fn: A => Option[B]): Extractor[A, B] =
    from[A].apply(fn)

  def identity[A]: Extractor[A, A] =
    from[A].property(Predef.identity[A])
  
  def from[A]: From[A] = new From[A]

  def fromMap[K, V](map: Map[K, V]): Extractor[K, V] = from[K](map.get)

  def isA[A: ClassTag]: Extractor[Any, A] =
    new FromClassTag[A]

  private def all[A, B](ab: Extractor[A, B]): Extractor[List[A], List[B]] = from[List[A]].apply(as => {
    val optBs: Option[List[B]] = as.foldLeft(Option(Nil: List[B])) {
      case (Some(acc), a) => ab.unapply(a).map(b => b :: acc)
      case (None, _)      => None
    }.map(_.reverse)

    optBs
  })

  def defer[A, B](value: => Extractor[A, B]): Extractor[A, B] =
    new Extractor.Deferred(value)

  val Boolean: Extractor[String, Boolean] = from[String].attempt(_.toBoolean)
  val Int: Extractor[String, Int] = from[String].attempt(_.toInt)

  object && {
    def unapply[A](a: A): Option[(A, A)] = Some((a, a))
  }

  object == {
    def unapply[A](pair: (A, A)): Option[(A, A)] = pair match {
      case (lhs, rhs) => if (lhs == rhs) Some(pair) else None
    }
  }

  object != {
    def unapply[A](pair: (A, A)): Option[(A, A)] = pair match {
      case (lhs, rhs) => if (lhs != rhs) Some(pair) else None
    }
  }

  object < {
    def unapply[A](pair: (A, A))(implicit A: Ordering[A]): Option[(A, A)] = pair match {
      case (lhs, rhs) => if (A.lt(lhs, rhs)) Some(pair) else None
    }
  }

  object <= {
    def unapply[A](pair: (A, A))(implicit A: Ordering[A]): Option[(A, A)] = pair match {
      case (lhs, rhs) => if (A.lteq(lhs, rhs)) Some(pair) else None
    }
  }

  object > {
    def unapply[A](pair: (A, A))(implicit A: Ordering[A]): Option[(A, A)] = pair match {
      case (lhs, rhs) => if (A.gt(lhs, rhs)) Some(pair) else None
    }
  }

  object >= {
    def unapply[A](pair: (A, A))(implicit A: Ordering[A]): Option[(A, A)] = pair match {
      case (lhs, rhs) => if (A.gteq(lhs, rhs)) Some(pair) else None
    }
  }

  object -=- {
    def unapply[A: Subtract](pair: (A, A)): Option[(A, A)] = {
      val (lhs, rhs) = pair

      val lhsOnly = Subtract.of[A].apply(lhs, rhs)
      val rhsOnly = Subtract.of[A].apply(rhs, lhs)

      Some((lhsOnly, rhsOnly))
    }
  }

  object -= {
    def unapply[A: Subtract](pair: (A, A)): Option[(A, A)] = {
      val (lhs, rhs) = pair

      val rhsOnly = Subtract.of[A].apply(rhs, lhs)

      Some((lhs, rhsOnly))
    }
  }

  object =- {
    def unapply[A: Subtract](pair: (A, A)): Option[(A, A)] = {
      val (lhs, rhs) = pair

      val lhsOnly = Subtract.of[A].apply(lhs, rhs)

      Some((lhsOnly, rhs))
    }
  }

  class From[A] {

    def collect[B](pf: PartialFunction[A, B]): Extractor[A, B] =
      apply(pf.lift)

    def when(p: A => Boolean): Extractor[A, A] =
      apply(a => Some(a).filter(p))

    def tuple2[T1, T2](t1: A => T1, t2: A => T2): Extractor[A, (T1, T2)] =
      property[(T1, T2)](a => (t1(a), t2(a)))

    def tuple3[T1, T2, T3](t1: A => T1, t2: A => T2, t3: A => T3): Extractor[A, (T1, T2, T3)] =
      property[(T1, T2, T3)](a => (t1(a), t2(a), t3(a)))

    def property[B](f: A => B): Extractor[A, B] =
      apply(a => Some(f(a)))

    def attempt[B](f: A => B): Extractor[A, B] =
      apply(a => Try(f(a)).toOption)

    def apply[B](f: A => Option[B]): Extractor[A, B] =
      FromFn(f)

  }

  private case class FromFn[A, B](f: A => Option[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = f(a)
  }

  private class FromClassTag[A: ClassTag] extends Extractor[Any, A] {

    def unapply(value: Any): Option[A] = {
      if (value.getClass == implicitly[ClassTag[A]].runtimeClass) {
        Some(value.asInstanceOf[A])
      } else None
    }
  }

  private case class AndThen[A, B, C](ab: Extractor[A, B], bc: Extractor[B, C]) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = ab.unapply(a).flatMap(bc.unapply)
  }

  private case class OrElse[A, B](lhs: Extractor[A, B], rhs: Extractor[A, B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = lhs.unapply(a).orElse(rhs.unapply(a))
  }

  private case class When[A, B](ab: Extractor[A, B], p: B => Boolean) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = ab.unapply(a).filter(p)
  }

  private case class Mapped[A, B, C](ab: Extractor[A, B], bc: B => C) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = ab.unapply(a).map(bc)
  }

  private case class FlatMapped[A, B, C](ab: Extractor[A, B], bc: B => Extractor[A, C]) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = ab.unapply(a).flatMap(b => bc(b).unapply(a))
  }

  private case class Contramapped[Z, A, B](ab: Extractor[A, B], za: Z => A) extends Extractor[Z, B] {
    def unapply(z: Z): Option[B] = ab.unapply(za(z))
  }

  private class Deferred[A, B](deferred0: => Extractor[A, B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = deferred.unapply(a)

    private lazy val deferred: Extractor[A, B] = deferred0
  }

  private case class Tapped[A, B](
    ab: Extractor[A, B],
    before: A => Unit,
    after: Option[B] => Unit,
    around: A => Option[B] => Unit
  ) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = {
      before(a)

      val result = ab.unapply(a)
      after(result)
      around(a)(result)
      
      result
    }
  }

  sealed trait Subtract[A] {
    def apply(lhs: A, rhs: A): A
  }

  object Subtract {
    def of[A](implicit subtract: Subtract[A]): Subtract[A] = subtract

    implicit def listSubtract[A]: Subtract[List[A]] = new Subtract[List[A]] {
      def apply(lhs: List[A], rhs: List[A]): List[A] = lhs.filterNot(rhs.contains)
    }

    implicit def optionSubtract[A: Subtract]: Subtract[Option[A]] = new Subtract[Option[A]] {
      def apply(lhs: Option[A], rhs: Option[A]): Option[A] = (lhs, rhs) match {
        case (Some(l), Some(r)) => Some(Subtract.of[A].apply(l, r))
        case (Some(l), None) => Some(l)
        case (None, Some(r)) => Some(r)
        case (None, None)    => None
      }
    }
  }
}



