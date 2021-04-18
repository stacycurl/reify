package reify

import scala.language.{dynamics, implicitConversions, postfixOps}

import java.io.File
import reify.Reified.{RInfix, RList, RMethod}
import reify.Reify.Reifys
import reify.Token.{Arguments, Compound, Function, Identifier, Infix, Method, Primitive, TString, TType}
import reify.internal.prelude._
import symmetric.{Extractor, Injector, Symmetric}


sealed trait Reified extends Dynamic with Product with Serializable {
  final def namedIf(condition: Boolean, name: String): Reified =
    if (condition) named(name) else this

  final def named(name: String): Reified =
    RInfix.named(name, this)

  final def method[P, A: RList.ToRList](parameters: P <-> A): P <-> Reified =
    parameters <-> RList.symmetric[A] <-> Invocation.create(this, "")

  final def method[P, A: RList.ToRList](methodName: String, parameters: P <-> A): P <-> Reified =
    parameters <-> RList.symmetric[A] <-> Invocation.create(this, methodName)

  def apply(parameters: List[Reified]): Reified =
    method("", parameters)

  def apply[A: Reify](a: A): Reified =
    method("", a ^: Nil)

  def apply[A: Reify, B: Reify](a: A, b: B): Reified =
    method("", a ^: b ^: Nil)

  def apply[A: Reify, B: Reify, C: Reify](a: A, b: B, c: C): Reified =
    method("", a ^: b ^: c ^: Nil)

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

  def applyDynamic(name: String)(args: Reifys*): Reified = args.toList match {
    case head :: Nil if name.forall(c => !c.isLetterOrDigit) => RInfix(this, s" $name ", head.value)
    case list                                                => method(name, list.map(_.value))
  }
  
  def infix[A: Reify](name: String, rhs: A): Reified =
    RInfix(this, name, Reify.reify(rhs))
  
  final def escape: Reified = Reified.escape(this)

  final def tokenize: Token = Reified.tokenize(this)

  final def transform(pf: PartialFunction[Reified, Reified]): Reified = Reified.transform(pf, this)
}

object Reified extends Reify.Companion[Reified] {
  object parse {
    def unapply(token: Token): Option[Reified] = apply(token)

    def apply(token: Token): Option[Reified] = PartialFunction.condOpt(token) {
      case Primitive(value)                                             => RPrimitive(value)
      case Identifier(value)                                            => RPrimitive(value)
      case TString(value)                                               => RString(value)
      case Infix(parse(lhs), separator, parse(rhs))                     => RInfix(lhs, separator, rhs)
      case Compound(TType(r"new $name", args), Arguments(List(params))) => RClass(RType.fromTType(TType(name, args)), params)
      case Compound(ttype, Arguments(List(params)))                     => RCaseClass(RType.fromTType(ttype), params)
      case Function(name, Arguments(List(params)))                      => RCaseClass(RType(name), params)
      case Method(parse(target), name, Arguments(List(params)))         => RMethod(target, name, params)
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
      case RBoolean(value)                   => RCaseClass(RType("RBoolean"), value)
      case RInt(value)                       => RCaseClass(RType("RInt"), value)
      case RLong(value)                      => RCaseClass(RType("RLong"), value)
      case RString(value)                    => RCaseClass(RType("RString"), value)
      case RPrimitive(value)                 => RCaseClass(RType("RPrimitive"), RString(value))
      case ROption(value)                    => RCaseClass(RType("ROption"), RROption(value))
      case REither(value)                    => RCaseClass(RType("REither"), RREither(value))
      case RList(list)                       => RCaseClass(RType("RList"), RRList(list))
      case RSet(set)                         => RCaseClass(RType("RSet"), RRSet(set))
      case RMap(map)                         => RCaseClass(RType("RMap"), RRMap(map))
      case RClass(rtype, parameters)         => RCaseClass(RType("RClass"), Reify.reify(rtype) :: RRList(parameters) :: Nil)
      case RCaseClass(rtype, parameters)     => RCaseClass(RType("RCaseClass"), Reify.reify(rtype) :: RRList(parameters) :: Nil)
      case RInfix(lhs, name, rhs)            => RCaseClass(RType("RInfix"), List(reify(lhs), RString(name), reify(rhs)))
      case RMethod(target, name, parameters) => RCaseClass(RType("RMethod"), List(reify(target), RString(name), RRList(parameters)))
      case RVarArgs(args)                    => RCaseClass(RType("RVarArgs"), RRList(args))
    }

    def reflect(reified: Reified): Option[Reified] = PartialFunction.condOpt(reified) {
      case RCaseClass(RType("RBoolean", _), List(RBoolean(value)))                                        => RBoolean(value)
      case RCaseClass(RType("RInt", _), List(RInt(value)))                                                => RInt(value)
      case RCaseClass(RType("RLong", _), List(RLong(value)))                                              => RLong(value)
      case RCaseClass(RType("RString", _), List(RString(value)))                                          => RString(value)
      case RCaseClass(RType("RPrimitive", _), List(RString(value)))                                       => RPrimitive(value)
      case RCaseClass(RType("ROption", _), List(RROption(value)))                                         => ROption(value)
      case RCaseClass(RType("REither", _), List(RREither(value)))                                         => REither(value)
      case RCaseClass(RType("RList", _), List(RRList(elements)))                                          => RList(elements)
      case RCaseClass(RType("RMap", _), List(RRMap(map)))                                                 => RMap(map)
      case RCaseClass(RType("RInfix", _), List(Reified(lhs), RString(name), Reified(rhs)))                => RInfix(lhs, name, rhs)
      case RCaseClass(RType("RClass", _), RType(rtype) :: RRList(parameters) :: Nil)                      => RClass(rtype, parameters)
      case RCaseClass(RType("RCaseClass", Nil), RType(rtype) :: RRList(parameters) :: Nil)                => RCaseClass(rtype, parameters)
      case RCaseClass(RType("RMethod", _), Reified(target) :: RString(name) :: RRList(parameters) :: Nil) => RMethod(target, name, parameters)
      case RCaseClass(RType("RVarArgs", _), List(RRList(args)))                                           => RVarArgs(args)
    }

    object RROption {
      def apply(opt: Option[Reified]): Reified = opt match {
        case None        => RPrimitive("None")
        case Some(value) => RCaseClass(RType("Some"), reify(value))
      }

      def unapply(reified: Reified): Option[Option[Reified]] = PartialFunction.condOpt(reified) {
        case RPrimitive("None")                                 => None
        case RCaseClass(RType("Some", _), List(Reified(value))) => Some(value)
      }
    }

    object RREither {
      def apply(either: Either[Reified, Reified]): Reified = either match {
        case Left(value)  => RCaseClass(RType("Left"), reify(value))
        case Right(value) => RCaseClass(RType("Right"), reify(value))
      }

      def unapply(reified: Reified): Option[Either[Reified, Reified]] = PartialFunction.condOpt(reified) {
        case RCaseClass(RType("Left", _),  List(Reified(value))) => Left(value)
        case RCaseClass(RType("Right", _), List(Reified(value))) => Right(value)
      }
    }

    object RRMap {
      def apply(map: Map[Reified, Reified]): Reified = map match {
        case empty if empty.isEmpty => RPrimitive("Map").empty
        case nonEmpty               => RCaseClass(
          RType("Map"), nonEmpty.toList.map(kv => RInfix(reify(kv._1), " -> ", reify(kv._2)))
        )
      }

      def unapply(reified: Reified): Option[Map[Reified, Reified]] = PartialFunction.condOpt(reified) {
        case RMethod(RPrimitive("Map"), "empty", Nil) => Map.empty
        case RCaseClass(RType("Map", _), elements) => Map(elements collect {
          case RInfix(Reified(key), " -> ", Reified(value)) => key -> value
        }: _*)
      }
    }

    object RRList {
      def apply(values: List[Reified]): Reified = values match {
        case Nil    => RPrimitive("Nil")
        case nonNil => RCaseClass(RType("List"), nonNil.map(reify))
      }

      def unapply(reified: Reified): Option[List[Reified]] = PartialFunction.condOpt(reified) {
        case RPrimitive("Nil")            => Nil
        case RCaseClass(RType("List", _), nonEmpty) => nonEmpty.flatMap(reflect)
      }
    }

    object RRSet {
      def apply(values: Set[Reified]): Reified =
        RCaseClass(RType("Set"), values.toList.map(reify))

      def unapply(reified: Reified): Option[Set[Reified]] = PartialFunction.condOpt(reified) {
        case RCaseClass(RType("Set", _), values) => values.flatMap(reflect).toSet
      }
    }

    Reify[Reified](RType("Reified"), reify, reflect)
  }

  def tokenize(reified: Reified): Token = {
    def loop(reified: Reified): Token = reified match {
      case RCaseClass(rtype, parameters)          => Compound(rtype.tokenize, Arguments(parameters.map(loop)))
      case RClass(rtype, parameters)              => Compound(rtype.tokenize.modify(name => s"new $name"), Arguments(parameters.map(loop)))
      case RInfix(lhs, name, rhs)                 => Infix(loop(lhs), name, loop(rhs))
      case RMethod(target, name, parameters)      => Method(loop(target), name, Arguments(parameters.map(loop)))
      case RPrimitive(value)                      => Primitive(value)
      case RString(value)                         => TString(value)
      case RVarArgs(value)                        => Arguments(value.map(loop))
    }

    loop(reified)
  }

  def escape(reified: Reified): Reified = {
    def loop(reified: Reified): Reified = reified match {
      case RCaseClass(rtype, parameters)     => RCaseClass(rtype, parameters.map(loop))
      case RClass(rtype, parameters)         => RClass(rtype, parameters.map(loop))
      case RInfix(lhs, name, rhs)            => RInfix(loop(lhs), name, loop(rhs))
      case RMethod(target, name, parameters) => RMethod(loop(target), name, parameters.map(loop))
      case RPrimitive(value)                 => RPrimitive(value)
      case RString(value)                    => RString(value.escape)
      case RVarArgs(value)                   => RVarArgs(value.map(loop))
    }

    loop(reified)
  }

  final def transform(pf: PartialFunction[Reified, Reified], reified: Reified): Reified = {
    def loop(reified: Reified): Reified = pf.lift(reified) match {
      case Some(result) => result
      case None => reified match {
        case RCaseClass(rtype, parameters)     => RCaseClass(rtype, parameters.map(loop))
        case RClass(rtype, parameters)         => RClass(rtype, parameters.map(loop))
        case RInfix(lhs, name, rhs)            => RInfix(lhs.transform(pf), name, rhs.transform(pf))
        case RMethod(target, name, parameters) => RMethod(target.transform(pf), name, parameters.map(loop))
        case RPrimitive(value)                 => RPrimitive(value)
        case RString(value)                    => RString(value)
        case RVarArgs(args)                    => RVarArgs(args.map(loop))
      }
    }

    loop(reified)
  }

  private val prim: Injector[String, Reified] = Injector.from[String].pf[Reified](RPrimitive(_): Reified) {
    case RPrimitive(value) => value
  }
  
  val RBoolean: Injector[Boolean, Reified] = Injector.Boolean andThen prim
  val RInt:     Injector[Int, Reified]     = Injector.Int     andThen prim
  val RLong:    Injector[Long, Reified]    = Injector.Long    andThen prim
  
  val RFile: Injector[File, Reified] = 
    Injector.id[File].xmap[String](_.getPath)(path => Some(new File(path))) andThen RClass.injector[String](RType("File"))
  
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

    def apply(strings: List[String]): List[Reified] = strings.map(Reify.reify(_))
  }

  object RString extends Symmetric.FromExtractor[RString, String]

  case class RString(value: String) extends Reified

  case class RPrimitive(value: String) extends Reified with Dynamic {
    def selectDynamic(name: String): Reified = method(name, Nil)
  }

  object RTuple2 {
    def varArgs[A, B](implicit A: Reify[A], B: Reify[B]): Injector[List[(A, B)], Reified] = new Injector[List[(A, B)], Reified] {
      def inject(values: List[(A, B)]): Reified = RVarArgs(values.map(create[A, B]))

      def unapply(reified: Reified): Option[List[(A, B)]] = for {
        values <- PartialFunction.condOpt(reified) {
          case RVarArgs(values) => values
        }
        tuple2s <- Some(values collect {
          case RTuple2((A(a), B(b))) => (a, b)
        }).filter(_.length == values.length)
      } yield tuple2s 
    }
    
    def create[A: Reify, B: Reify](ab: (A, B)): Reified =
      apply(Reify.reify(ab._1) -> Reify.reify(ab._2))
    
    def apply(t: (Reified, Reified)): Reified =
      RCaseClass(RType(""), List(t._1, t._2))
      
    def unapply(reified: Reified): Option[(Reified, Reified)] = PartialFunction.condOpt(reified) {
      case RCaseClass(RType("", _), a :: b :: Nil) => (a, b)
    }
  }
  
  object RTuple3 {
    def create[A: Reify, B: Reify, C: Reify](abc: (A, B, C)): Reified =
      apply((Reify.reify(abc._1), Reify.reify(abc._2), Reify.reify(abc._3)))
    
    def apply(t: (Reified, Reified, Reified)): Reified =
      RCaseClass(RType(""), List(t._1, t._2, t._3))

    def unapply(reified: Reified): Option[(Reified, Reified, Reified)] = PartialFunction.condOpt(reified) {
      case RCaseClass(RType("", _), a :: b :: c :: Nil) => (a, b, c)
    }
  }
  
  object ROption {
    def create[A: Reify](optA: Option[A]): Reified = apply(optA.map(Reify.reify(_)))
    
    def apply(opt: Option[Reified]): Reified = opt match {
      case None    => RPrimitive("None")
      case Some(a) => RCaseClass(RType("Some"), a)
    } 
    
    def unapply(reified: Reified): Option[Option[Reified]] = PartialFunction.condOpt(reified) {
      case RPrimitive("None")                    => None
      case RCaseClass(RType("Some", _), List(a)) => Some(a)
    }
  }

  object REither {
    def create[L: Reify, R: Reify](value: Either[L, R]): Reified =
      REither(value.left.map(Reify.reify[L]).right.map(Reify.reify[R]))
      
    def apply(value: Either[Reified, Reified]): Reified = value match {
      case Left(l)  => RCaseClass(RType("Left"),  List(l))
      case Right(r) => RCaseClass(RType("Right"), List(r))
    }
    
    def unapply(reified: Reified): Option[Either[Reified, Reified]] = PartialFunction.condOpt(reified) {
      case RCaseClass(RType("Left", _),  List(l)) => Left(l)
      case RCaseClass(RType("Right", _), List(r)) => Right(r)
    }
  }

  object RList {
    def symmetric[A](implicit A: ToRList[A]): A <-> List[Reified] = A.sym

    sealed trait ToRList[A] {
      def sym: A <-> List[Reified]
    }

    object ToRList extends ToRList0 {
      implicit def tuple[A: Reify, B](
        implicit B: ToRList[B]
      ): ToRList[(A, B)] = new ToRList[(A, B)] {
        val A = Reify.of[A]

        def sym: (A, B) <-> List[Reified] = new ((A, B) <-> List[Reified]) {
          def to(ab: (A, B)): Option[List[Reified]] = for {
            rlist <- B.sym.to(ab._2)
          } yield ab._1 ^: rlist

          def from(rlist: List[Reified]): Option[(A, B)] = {
            rlist match {
              case A(a) :: tail => B.sym.from(tail).map(b => (a, b))
            }
          }
        }
      }
    }

    trait ToRList0 {
      implicit def unit: ToRList[Unit] = new ToRList[Unit] {
        def sym: Unit <-> List[Reified] = new (Unit <-> List[Reified]) {
          def to(unit: Unit): Option[List[Reified]] = Some(Nil)

          def from(rlist: List[Reified]): Option[Unit] = PartialFunction.condOpt(rlist) {
            case Nil => ()
          }
        }
      }

      implicit def single[A: Reify]: ToRList[A] = new ToRList[A] {
        val A = Reify.of[A]

        def sym: A <-> List[Reified] = new (A <-> List[Reified]) {
          def to(a: A): Option[List[Reified]] = Some(a ^: Nil)

          def from(rlist: List[Reified]): Option[A] = PartialFunction.condOpt(rlist) {
            case A(a) :: Nil => a
          }
        }
      }
    }

    def create[A: Reify](value: List[A]): Reified = RList(value.map(Reify.reify[A]))
    
    def apply(values: List[Reified]): Reified =
      RCaseClass(RType("List"), values)
      
    def unapply(value: Reified): Option[List[Reified]] = PartialFunction.condOpt(value) {
      case RCaseClass(RType("List", _), values) => values
    }
  }

  object RSet {
    def create[A: Reify](value: Set[A]): Reified = RSet(value.map(Reify.reify[A]))
    
    def apply(value: Set[Reified]): Reified = RCaseClass(RType("Set"), value.toList)
    
    def unapply(reified: Reified): Option[Set[Reified]] = PartialFunction.condOpt(reified) {
      case RCaseClass(RType("Set", _), parameters) => parameters.toSet
    }
  }

  object RMap {
    def create[K, V](value: Map[K, V])(implicit K: Reify[K], V: Reify[V]): Reified =
      RMap(value.map(kv => K.reify(kv._1) -> V.reify(kv._2)))
      
    def apply(value: Map[Reified, Reified]): Reified = RCaseClass(RType("Map"), value.toList.map {
      case (k, v) => RInfix(k, " -> ", v)
    })
    
    def unapply(value: Reified): Option[Map[Reified, Reified]] = PartialFunction.condOpt(value) {
      case RCaseClass(RType("Map", _), RInfix.Many(infixes)) => Map(infixes.map(infix => infix.lhs -> infix.rhs): _*)
    }
  }

  object RCaseClass extends HasParameters {
    protected val baseInjector: Injector[(RType, List[Reified]), Reified] = Injector.from[(RType, List[Reified])].pf({
      case (rtype, parameters) => RCaseClass(rtype, parameters): Reified
    }) {
      case RCaseClass(rtype, parameters) => (rtype, parameters)
    }
  }

  case class RCaseClass(rtype: RType, parameters: List[Reified]) extends Reified

  object RClass extends HasParameters {
    protected val baseInjector: Injector[(RType, List[Reified]), Reified] = Injector.from[(RType, List[Reified])].pf({
      case (rtype, parameters) => RClass(rtype, parameters): Reified
    }) {
      case RClass(rtype, parameters) => (rtype, parameters)
    }
  }
  
  case class RClass(rtype: RType, parameters: List[Reified]) extends Reified
  
  trait HasParameters {
    def apply[A: Reify](rtype: RType, a: A): Reified =
      create(rtype, List(Reify.reify(a)))

    def apply[A: Reify, B: Reify](rtype: RType, a: A, b: B): Reified =
      create(rtype, List(Reify.reify(a), Reify.reify(b)))

    def apply[A: Reify, B: Reify, C: Reify](rtype: RType, a: A, b: B, c: C): Reified =
      create(rtype, List(Reify.reify(a), Reify.reify(b), Reify.reify(c)))

    def apply[A: Reify](name: String, a: A): Reified =
      create(RType(name), List(Reify.reify(a)))

    def apply[A: Reify, B: Reify](name: String, a: A, b: B): Reified =
      create(RType(name), List(Reify.reify(a), Reify.reify(b)))

    def apply[A: Reify, B: Reify, C: Reify](name: String, a: A, b: B, c: C): Reified =
      create(RType(name), List(Reify.reify(a), Reify.reify(b), Reify.reify(c)))

    def apply[A: Reify, B: Reify, C: Reify, D: Reify](name: String, a: A, b: B, c: C, d: D): Reified =
      create(RType(name), List(Reify.reify(a), Reify.reify(b), Reify.reify(c), Reify.reify(d)))

    def injector[A](rtype: RType)(implicit A: Reify[A]): Injector[A, Reified] = 
      (A :: Injector.Nil).withValue(rtype).andThen(baseInjector)
    
    def injector[A, B](rtype: RType)(implicit A: Reify[A], B: Reify[B]): Injector[(A, B), Reified] = 
      (A :: B :: Injector.Nil).withValue(rtype).andThen(baseInjector)
    
    def apply(rtype: RType, parameters: Reified*): Reified =
      create(rtype, parameters.toList)
    
    def apply(name: String, parameters: Reified*): Reified =
      create(RType(name), parameters.toList)

    final protected def create(rtype: RType, parameters: List[Reified]): Reified =
      baseInjector.inject((rtype, parameters))

    protected def baseInjector: Injector[(RType, List[Reified]), Reified]
  }
  
  object RInfix {
    def apply[A: Reify, B: Reify](a: A, name: String, b: B): Reified =
      new RInfix(Reify.reify(a), name, Reify.reify(b))

    object named {
      def apply(name: String, value: Reified): Reified =
        RInfix(RPrimitive(name), " = ", value)

      def unapply(value: Reified): Option[(String, Reified)] = PartialFunction.condOpt(value) {
        case RInfix(RPrimitive(name), " = ", named) => (name, named)
      }

      object ignore {
        def unapply(value: Reified): Option[Reified] = PartialFunction.condOpt(value) {
          case RInfix(RPrimitive(_), " = ", named) => named
          case other                               => other
        }
      }
    }
    
    object Many {
      def unapply(values: List[Reified]): Option[List[RInfix]] = {
        val result = values.collect {
          case it: RInfix => it
        }
        
        if (result.length == values.length) Some(result) else None
      }
    }
  }

  case class RInfix(lhs: Reified, name: String, rhs: Reified) extends Reified

  case class RMethod(target: Reified, name: String, parameters: List[Reified]) extends Reified

  object RMethod {
    def create[T: Reify, A: Reify](target: T, name: String, a: A): Reified =
      new RMethod(Reify.reify(target), name, List(Reify.reify(a)))
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

object Invocation {
  def create(target: Reified, methodName: String): List[Reified] <-> Reified = new (List[Reified] <-> Reified) {
    def to(parameters: List[Reified]): Option[Reified] = Some(target.method(methodName, parameters))

    def from(rmethod: Reified): Option[List[Reified]] = PartialFunction.condOpt(rmethod) {
      case RMethod(`target`, `methodName`, parameters) => parameters
    }
  }
}
