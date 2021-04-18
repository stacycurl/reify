package reify

import java.io.File
import reify.Reified.{RBoolean, REither, RFile, RInt, RList, RLong, RMap, ROption, RSet, RString, RTuple2, RTuple3, RTuple4}
import symmetric.Injector


trait ReifyInstances {
  implicit lazy val reifyString: Reify[String] =
    instance[String](RType("String"), RString(_)) { case RString(value) => value }

  implicit lazy val reifyBoolean: Reify[Boolean] =
    fromInjector[Boolean](RType("Boolean"), RBoolean)

  implicit lazy val reifyInt: Reify[Int] =
    fromInjector[Int](RType("Int"), RInt)

  implicit lazy val reifyLong: Reify[Long] =
    fromInjector[Long](RType("Long"), RLong)

  implicit lazy val reifyFile: Reify[File] = 
    fromInjector[File](RType("File"), RFile)

  
  // TODO [16 Jan 2012]: Change this to A -> B
  implicit def reifyTuple2[A, B](implicit A: Reify[A], B: Reify[B]): Reify[(A, B)] =
    instance[(A, B)](RType.create[A, B]("Tuple2"), RTuple2.create) {
      case RTuple2(A(a), B(b)) => (a, b)
    }

  implicit def reifyTuple3[A, B, C](implicit A: Reify[A], B: Reify[B], C: Reify[C]): Reify[(A, B, C)] =
    instance[(A, B, C)](RType.create[A, B, C]("Tuple3"), RTuple3.create) {
      case RTuple3(A(a), B(b), C(c)) => (a, b, c)
    }

  implicit def reifyTuple4[A, B, C, D](implicit A: Reify[A], B: Reify[B], C: Reify[C], D: Reify[D]): Reify[(A, B, C, D)] =
    instance[(A, B, C, D)](RType.create[A, B, C, D]("Tuple4"), RTuple4.create) {
      case RTuple4(A(a), B(b), C(c), D(d)) => (a, b, c, d)
    }

  implicit def reifyOption[A: Reify]: Reify[Option[A]] =
    instance[Option[A]](RType.create[A]("Option"), ROption.create) {
      case ROption(optReified) => optReified.flatMap(Reify.reflect[A])
    }

  implicit def reifyList[A: Reify]: Reify[List[A]] =
    instance[List[A]](RType.create[A]("List"), RList.create) {
      case RList(values) => values.flatMap(Reify.of[A].reflect)
    }

  implicit def reifySet[A: Reify]: Reify[Set[A]] =
    instance[Set[A]](RType.create[A]("Set"), RSet.create) {
      case RSet(values) => values.flatMap(Reify.of[A].reflect)
    }

  implicit def reifyMap[K, V](implicit K: Reify[K], V: Reify[V]): Reify[Map[K, V]] =
    instance[Map[K, V]](RType.create[K, V]("Map"), RMap.create) {
      case RMap(values) => values.collect { case (K(k), V(v)) => k -> v }
    }

  implicit def reifyEither[L, R](implicit L: Reify[L], R: Reify[R]): Reify[Either[L, R]] =
    instance[Either[L, R]](RType.create[L, R]("Either"), REither.create) {
      case REither(Left(L(l))) => Left(l)
      case REither(Right(R(r))) => Right(r)
    }
    
  private def fromInjector[A](rtype: RType, injector: Injector[A, Reified]): Reify[A] =
    ReifyImplementations.FromFunctions[A](rtype, injector.inject, injector.unapply)

  private def instance[A](rtype: RType, reify: A => Reified)(reflect: PartialFunction[Reified, A]): Reify[A] =
    ReifyImplementations.FromFunctions[A](rtype, reify, reflect.lift)
}
