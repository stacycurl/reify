package reify

import reify.Reified.{RBoolean, REither, RInt, RList, RLong, RMap, ROption, RSet, RString}


trait ReifyInstances {
  implicit val reifyBoolean: Reify[Boolean] =
    instance[Boolean](RType.TC0("Boolean"), RBoolean) { case RBoolean(value) => value }

  implicit val reifyInt: Reify[Int] =
    instance[Int](RType.TC0("Int"), RInt) { case RInt(value) => value }

  implicit val reifyLong: Reify[Long] =
    instance[Long](RType.TC0("Long"), RLong) { case RLong(value) => value }

  implicit val reifyString: Reify[String] =
    instance[String](RType.TC0("String"), RString) { case RString(value) => value }



  implicit def reifyOption[A: Reify]: Reify[Option[A]] =
    instance[Option[A]](RType.tc1[A]("Option"), ROption.create) {
      case ROption(optReified) => optReified.flatMap(Reify.reflect[A])
    }

  implicit def reifyList[A: Reify]: Reify[List[A]] =
    instance[List[A]](RType.tc1[A]("List"), RList.create) {
      case RList(values) => values.flatMap(Reify.of[A].reflect)
    }

  implicit def reifySet[A: Reify]: Reify[Set[A]] =
    instance[Set[A]](RType.tc1[A]("Set"), RSet.create) {
      case RSet(values) => values.flatMap(Reify.of[A].reflect)
    }

  implicit def reifyMap[K, V](implicit K: Reify[K], V: Reify[V]): Reify[Map[K, V]] =
    instance[Map[K, V]](RType.tc2[K, V]("Map"), RMap.create) {
      case RMap(values) => values.collect { case (K(k), V(v)) => k -> v }
    }

  implicit def reifyEither[L, R](implicit L: Reify[L], R: Reify[R]): Reify[Either[L, R]] =
    instance[Either[L, R]](RType.tc2[L, R]("Either"), REither.create) {
      case REither(Left(L(l))) => Left(l)
      case REither(Right(R(r))) => Right(r)
    }

  private def instance[A](rtype: RType, reify: A => Reified)(reflect: PartialFunction[Reified, A]): Reify[A] =
    ReifyImplementations.FromFunctions[A](rtype, reify, reflect.lift)
}
