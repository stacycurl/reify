package reify

import scala.language.dynamics

import reify.Reified.{RCaseClass, RList, RMethod}
import reify.Token.{Arguments, Compound, Identifier, Infix, Method, Primitive, TString}
import reify.internal.prelude._
import reify.internal.util.{Around, Extractor}

sealed trait Reified {
  def apply(parameters: List[Reified]): Reified =
    method("", parameters)

  def apply[A: Reify](a: A): Reified =
    method("", List(Reify.reify(a)))

  def apply[A: Reify, B: Reify](a: A, b: B): Reified =
    method("", List(Reify.reify(a), Reify.reify(b)))

  def apply[A: Reify, B: Reify, C: Reify](a: A, b: B, c: C): Reified =
    method("", List(Reify.reify(a), Reify.reify(b), Reify.reify(c)))

  def method(name: String, parameters: List[Reified]): Reified =
    RMethod(this, name, parameters)

  def method(name: String): Reified =
    new RMethod(this, name, Nil)

  def method[A: Reify](name: String, a: A): Reified =
    new RMethod(this, name, a ^: Nil)

  def method[A: Reify, B: Reify](name: String, a: A, b: B): Reified =
    new RMethod(this, name, a ^: b ^: Nil)

  def method[A: Reify, B: Reify, C: Reify](name: String, a: A, b: B, c: C): Reified =
    new RMethod(this, name, a ^: b ^: c ^: Nil)

  def method[A: Reify, B: Reify, C: Reify, D: Reify](name: String, a: A, b: B, c: C, d: D): Reified =
    new RMethod(this, name, a ^: b ^: c ^: d ^: Nil)

  def method[A: Reify, B: Reify, C: Reify, D: Reify, E: Reify](name: String, a: A, b: B, c: C, d: D, e: E): Reified =
    new RMethod(this, name, a ^: b ^: c ^: d ^: e ^: Nil)

  final def escape: Reified = Reified.escape(this)

  final def tokenize: Token = Reified.tokenize(this)

  final def transform(pf: PartialFunction[Reified, Reified]): Reified = Reified.transform(pf, this)

  final def useAsArgumentsFor(name: String): Reified = this match {
    case RList(args)         => RCaseClass(name, args)
    case RCaseClass(_, args) => RCaseClass(name, args)
    case other               => other
  }
}

object Reified extends Reify.Companion[Reified] {
  object Arounds {
    val listVarArgs: Around = Around("listVarArgs", {
      case RList(value) => RVarArgs(value)
    }, {
      case RVarArgs(value) => RList(value)
    })
  }

  object parse {
    def unapply(token: Token): Option[Reified] = apply(token)

    def apply(token: Token): Option[Reified] = PartialFunction.condOpt(token) {
      case Primitive(Extractor.Boolean(value))       => RBoolean(value)
      case Primitive(Extractor.Int(value))           => RInt(value)
      case Primitive("None")                         => ROption(None)
      case Primitive(value)                          => RPrimitive(value)
      case Identifier(value)                         => RCaseObject(value)
      case TString(value)                            => RString(value)
      case Infix(parse(lhs), separator, parse(rhs))  => RInfix(lhs, separator, rhs)
      case Compound("Some", parse(value) :: Nil)     => ROption(Some(value))
      case Compound("List", List(params))            => RList(params)
      case Compound("Map", Entries(entries))         => RMap(entries)
      case Compound(name, List(params))              => RCaseClass(name, params)
      case Method(parse(target), name, List(params)) => RMethod(target, name, params)
    }

    private val List: Extractor[List[Token], List[Reified]] = Extractor.from[List[Token]].apply(tokens => {
      val result = tokens.foldLeft(Some(Nil): Option[List[Reified]]) {
        case (None, _)          => None
        case (Some(acc), token) => apply(token).map(reified => reified :: acc)
      }.map(_.reverse)

      result
    })

    private val Entries: Extractor[List[Token], Map[Reified, Reified]] =
      List.andThen(Extractor.from[List[Reified]].apply[Map[Reified, Reified]](reifieds => {
        val result = reifieds.foldLeft(Some(Map.empty[Reified, Reified]): Option[Map[Reified, Reified]]) {
          case (Some(acc), RInfix(key, " -> ", value)) => Some(acc + (key -> value))
          case _                                       => None
        }

        result
      }))
  }

  implicit val reifyReified: Reify[Reified] = {
    def reify(value: Reified): Reified = value match {
      case RBoolean(value)                   => RCaseClass("RBoolean", value)
      case RInt(value)                       => RCaseClass("RInt", value)
      case RLong(value)                      => RCaseClass("RLong", value)
      case RString(value)                    => RCaseClass("RString", value)
      case RPrimitive(value)                 => RCaseClass("RPrimitive", RString(value))
      case ROption(value)                    => RCaseClass("ROption", RROption(value))
      case REither(value)                    => RCaseClass("REither", RREither(value))
      case RList(list)                       => RCaseClass("RList", RRList(list))
      case RSet(set)                         => RCaseClass("RSet", RRSet(set))
      case RMap(map)                         => RCaseClass("RMap", RRMap(map))
      case RCaseClass(name, parameters)      => RCaseClass("RCaseClass", RString(name) :: RRList(parameters) :: Nil)
      case RCaseObject(name)                 => RCaseClass("RCaseObject", RString(name))
      case RInfix(lhs, name, rhs)            => RCaseClass("RInfix", List(lhs, RString(name), rhs))
      case RMethod(target, name, parameters) => RCaseClass("RMethod", List(reify(target), RString(name), RRList(parameters)))
      case RVarArgs(args)                    => RCaseClass("RVarArgs", RRList(args))
    }

    def reflect(reified: Reified): Option[Reified] = PartialFunction.condOpt(reified) {
      case RCaseClass("RBoolean", List(RBoolean(value)))                                        => RBoolean(value)
      case RCaseClass("RInt", List(RInt(value)))                                                => RInt(value)
      case RCaseClass("RLong", List(RLong(value)))                                              => RLong(value)
      case RCaseClass("RString", List(RString(value)))                                          => RString(value)
      case RCaseClass("RPrimitive", List(RString(value)))                                       => RPrimitive(value)
      case RCaseClass("ROption", List(RROption(value)))                                         => ROption(value)
      case RCaseClass("REither", List(RREither(value)))                                         => REither(value)
      case RCaseClass("RList", List(RRList(elements)))                                          => RList(elements)
      case RCaseClass("RMap", List(RRMap(map)))                                                 => RMap(map)
      case RCaseClass("RInfix", List(lhs, RString(name), rhs))                                  => RInfix(lhs, name, rhs)
      case RCaseClass("RCaseClass", RString(name) :: RRList(parameters) :: Nil)                 => RCaseClass(name, parameters)
      case RCaseClass("RCaseObject", List(RString(name)))                                       => RCaseObject(name)
      case RCaseClass("RMethod", Reified(target) :: RString(name) :: RRList(parameters) :: Nil) => RMethod(target, name, parameters)
      case RCaseClass("RVarArgs", List(RRList(args)))                                           => RVarArgs(args)
    }

    object RROption {
      def apply(opt: Option[Reified]): Reified = opt match {
        case None        => RCaseObject("None")
        case Some(value) => RCaseClass("Some", reify(value))
      }

      def unapply(reified: Reified): Option[Option[Reified]] = PartialFunction.condOpt(reified) {
        case RCaseObject("None")                      => None
        case RCaseClass("Some", List(Reified(value))) => Some(value)
      }
    }

    object RREither {
      def apply(either: Either[Reified, Reified]): Reified = either match {
        case Left(value)  => RCaseClass("Left", reify(value))
        case Right(value) => RCaseClass("Right", reify(value))
      }

      def unapply(reified: Reified): Option[Either[Reified, Reified]] = PartialFunction.condOpt(reified) {
        case RCaseClass("Left", List(Reified(value))) => Left(value)
        case RCaseClass("Right", List(Reified(value))) => Right(value)
      }
    }

    object RRMap {
      def apply(map: Map[Reified, Reified]): Reified = map match {
        case empty if empty.isEmpty => RCaseObject("Map").empty
        case nonEmpty               => RCaseClass("Map", nonEmpty.toList.map(kv => RInfix(reify(kv._1), " -> ", reify(kv._2))))
      }

      def unapply(reified: Reified): Option[Map[Reified, Reified]] = PartialFunction.condOpt(reified) {
        case RMethod(RCaseObject("Map"), "empty", Nil) => Map.empty
        case RCaseClass("Map", elements) => Map(elements collect {
          case RInfix(Reified(key), " -> ", Reified(value)) => key -> value
        }: _*)
      }
    }

    object RRList {
      def apply(values: List[Reified]): Reified = values match {
        case Nil    => RCaseObject("Nil")
        case nonNil => RCaseClass("List", nonNil.map(reify))
      }

      def unapply(reified: Reified): Option[List[Reified]] = PartialFunction.condOpt(reified) {
        case RCaseObject("Nil")           => Nil
        case RCaseClass("List", nonEmpty) => nonEmpty.flatMap(reflect)
      }
    }

    object RRSet {
      def apply(values: Set[Reified]): Reified =
        RCaseClass("Set", values.toList.map(reify))

      def unapply(reified: Reified): Option[Set[Reified]] = PartialFunction.condOpt(reified) {
        case RCaseClass("Set", values) => values.flatMap(reflect).toSet
      }
    }

    Reify[Reified](RType.TC0("Reified"), reify, reflect)
  }

  def tokenize(reified: Reified): Token = {
    def loop(reified: Reified): Token = reified match {
      case RBoolean(value)                   => Primitive(value.toString)
      case RInt(value)                       => Primitive(value.toString)
      case RLong(value)                      => Primitive(s"${value}L")
      case RString(value)                    => TString(value)
      case RPrimitive(value)                 => Primitive(value)
      case ROption(None)                     => Primitive("None")
      case ROption(Some(value))              => Compound("Some", List(loop(value)))
      case REither(Left(value))              => Compound("Left", List(loop(value)))
      case REither(Right(value))             => Compound("Right", List(loop(value)))
      case RList(value)                      => Compound("List", value.map(loop))
      case RSet(value)                       => Compound("Set", value.map(loop).toList)
      case RMap(value)                       => Compound("Map", value.toList.map(kv => Infix(loop(kv._1), " -> ", loop(kv._2))))
      case RCaseClass(name, parameters)      => Compound(name, parameters.map(loop))
      case RCaseObject(name)                 => Identifier(name)
      case RInfix(lhs, name, rhs)            => Infix(loop(lhs), name, loop(rhs))
      case RMethod(target, name, parameters) => Method(loop(target), name, parameters.map(loop))
      case RVarArgs(value)                   => Arguments(value.map(loop))
    }

    loop(reified)
  }

  def escape(reified: Reified): Reified = {
    def loop(reified: Reified): Reified = reified match {
      case RString(value)                    => RString(value.escape)
      case ROption(Some(value))              => ROption(Some(loop(value)))
      case REither(Left(value))              => REither(Left(loop(value)))
      case REither(Right(value))             => REither(Right(loop(value)))
      case RList(value)                      => RList(value.map(loop))
      case RMap(value)                       => RMap(value.map(kv => loop(kv._1) -> loop(kv._2)))
      case RCaseClass(name, parameters)      => RCaseClass(name, parameters.map(loop))
      case RInfix(lhs, name, rhs)            => RInfix(loop(lhs), name, loop(rhs))
      case RMethod(target, name, parameters) => RMethod(loop(target), name, parameters.map(loop))
      case RVarArgs(value)                   => RVarArgs(value.map(loop))
      case other                             => other
    }

    loop(reified)
  }

  final def transform(pf: PartialFunction[Reified, Reified], reified: Reified): Reified = {
    def loop(reified: Reified): Reified = pf.lift(reified) match {
      case Some(result) => result
      case None => reified match {
        case ROption(values)                   => ROption(values.map(loop))
        case REither(values)                   => REither(values.left.map(loop).right.map(loop))
        case RList(values)                     => RList(values.map(loop))
        case RMap(values)                      => RMap(values.map(kv => loop(kv._1) -> loop(kv._2)))
        case RInfix(lhs, name, rhs)            => RInfix(lhs.transform(pf), name, rhs.transform(pf))
        case RCaseClass(name, parameters)      => RCaseClass(name, parameters.map(loop))
        case RMethod(target, name, parameters) => RMethod(target.transform(pf), name, parameters.map(loop))
        case RVarArgs(args)                    => RVarArgs(args.map(loop))
        case other                             => other
      }
    }

    loop(reified)
  }


  case class RBoolean(value: Boolean) extends Reified

  case class RInt(value: Int) extends Reified

  case class RLong(value: Long) extends Reified

  object RStrings {
    object FromItem {
      def unapply(reified: Reified): Option[List[String]] = for {
        values  <- PartialFunction.condOpt(reified) { case RList(values) => values }
        strings <- unapplySeq(values)
      } yield strings
    }

    object FromItems {
      def unapply(reified: List[Reified]): Option[List[String]] = unapplySeq(reified)
    }

    def unapplySeq(reified: List[Reified]): Option[List[String]] = {
      val strings = reified.collect { case RString(string) => string }

      if (strings.length == reified.length) Some(strings) else None
    }
  }

  case class RString(value: String) extends Reified

  case class RPrimitive(value: String) extends Reified

  object ROption {
    val Int: Extractor[Reified, Option[Int]] = Extractor.from[Reified].collect {
      case ROption(None)              => None
      case ROption(Some(RInt(value))) => Some(value)
    }

    def create[A: Reify](value: Option[A]): Reified = ROption(value.map(Reify.reify[A]))
  }

  case class ROption(value: Option[Reified]) extends Reified

  object REither {
    def create[L: Reify, R: Reify](value: Either[L, R]): Reified =
      REither(value.left.map(Reify.reify[L]).right.map(Reify.reify[R]))
  }

  case class REither(value: Either[Reified, Reified]) extends Reified

  object RList {
    def create[A: Reify](value: List[A]): Reified = RList(value.map(Reify.reify[A]))
  }

  case class RList(value: List[Reified]) extends Reified

  object RSet {
    def create[A: Reify](value: Set[A]): Reified = RSet(value.map(Reify.reify[A]))
  }

  case class RSet(value: Set[Reified]) extends Reified

  object RMap {
    def create[K, V](value: Map[K, V])(implicit K: Reify[K], V: Reify[V]): Reified =
      RMap(value.map(kv => K.reify(kv._1) -> V.reify(kv._2)))
  }

  case class RMap(value: Map[Reified, Reified]) extends Reified

  object RCaseClass {
    def something[A: Reify](name: String, a: A): Reified =
      Reify.reify(a).useAsArgumentsFor(name)

    def apply[A: Reify](name: String, a: A): Reified =
      new RCaseClass(name, List(Reify.reify(a)))

    def apply[A: Reify, B: Reify](name: String, a: A, b: B): Reified =
      new RCaseClass(name, List(Reify.reify(a), Reify.reify(b)))

    def apply[A: Reify, B: Reify, C: Reify](name: String, a: A, b: B, c: C): Reified =
      new RCaseClass(name, List(Reify.reify(a), Reify.reify(b), Reify.reify(c)))

    def apply[A: Reify, B: Reify, C: Reify, D: Reify](name: String, a: A, b: B, c: C, d: D): Reified =
      new RCaseClass(name, List(Reify.reify(a), Reify.reify(b), Reify.reify(c), Reify.reify(d)))

    def apply(name: String, parameters: Reified*): Reified =
      new RCaseClass(name, parameters.toList)
  }

  case class RCaseClass(name: String, parameters: List[Reified]) extends Reified

  // TODO: Consider removing, as this cannot be differentiated from RPrimitive
  case class RCaseObject(name: String) extends Reified with Dynamic {
    def selectDynamic(name: String): Reified = method(name, Nil)
  }

  object RInfix {
    def apply[A: Reify, B: Reify](a: A, name: String, b: B): Reified =
      new RInfix(Reify.reify(a), name, Reify.reify(b))
  }

  case class RInfix(lhs: Reified, name: String, rhs: Reified) extends Reified

  case class RMethod(target: Reified, name: String, parameters: List[Reified]) extends Reified

  object RMethod {
    def create[T: Reify, A: Reify](target: T, name: String, a: A): Reified =
      new RMethod(Reify.reify(target), name, List(Reify.reify(a)))

    def apply[A: Reify](target: Reified, name: String, a: A): Reified =
      new RMethod(target, name, List(Reify.reify(a)))

    def apply[A: Reify, B: Reify](target: Reified, name: String, a: A, b: B): Reified =
      new RMethod(target, name, List(Reify.reify(a), Reify.reify(b)))
  }

  object method {
    def unapply(reified: Reified): Option[(Reified, String, List[Reified])] = PartialFunction.condOpt(reified) {
      case RMethod(target, name, parameters) => (target, name, parameters)
    }
  }

  object RVarArgs {
    def create[A: Reify](value: List[A]): Reified = RVarArgs(value.map(Reify.reify[A]))
  }

  case class RVarArgs(value: List[Reified]) extends Reified
}