package reify

import scala.language.experimental.macros
import scala.language.implicitConversions

import java.util.Properties
import magnolia.{CaseClass, Magnolia, Param, SealedTrait}
import reify.Reified.{RCaseClass, RInfix, RList, RMethod, RPrimitive}
import reify.Reify.Alternative
import reify.internal.ForwardReferenceChecker
import reify.internal.prelude._
import reify.internal.util.classTag
import symmetric.{Extractor, Injector}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import scala.util.Try


object Reify extends ReifyInstances {
  case class Reifys(value: Reified) {
    override def toString: String = Formatter.Unindented.format(value)
  }

  object Reifys {
    implicit def reify[A: Reify](a: A): Reifys = Reifys(Reify.reify(a))
  }

  implicit class ReifyStringContext(private val self: StringContext) extends AnyVal {
    def reify(args: Reifys*): String = self.s(args: _*)
  }

  trait Companion[A] {
    def unapply(reified: Reified)(implicit A: Reify[A]): Option[A] = A.reflect(reified)

    object Many {
      def unapply(reified: Reified)(implicit A: Reify[List[A]]): Option[List[A]] = A.reflect(reified)
    }
  }

  trait Alternative[A] {
    def reify(value: A, default: A => Reified): Option[Reified]
    def reflect(reified: Reified, default: Reified => Option[A]): Option[A]

    // TODO: Nice to have (just for debugging atm)
    // Bit of a catch-22 here, it would be nice to print out in a nice format, but that requires reification,
    // which we are in the middle of
    // it seems I almost need a package private method on Reify that never delegates to Alternative
    case object println {
      def apply(msg: String)(implicit A: ClassTag[A]): Alternative[A] =
        when(condition = true, msg)

      def when(condition: => Boolean, msg: String)(implicit A: ClassTag[A]): Alternative[A] = {
        def log[X, Y](name: String, from: X, to: Y): Unit = if (condition) Console.println(
          s"""$msg: Alternative[${classTag.simpleClassName[A]}].$name(
             |  ${from.indentBy("  ")}
             |) =
             |  ${to.indentBy("  ")}
             |""".stripMargin
        )

        val formatter = Formatter.Indented(80, escape = false)

        tap(
          reify   = a => optReified => log("reify", a, optReified),
          reflect = reified => optA => log("reflect", formatter.format(reified), optA)
        )
      }
    }

    final def method(target: Reified, name: String): Alternative[A] = Alternative.Method(target, name, this)
    final def ||(rhs: Alternative[A]): Alternative[A] = Alternative.Or.create(this, rhs)

    final def tapReify(reify: A => Option[Reified] => Unit): Alternative[A] = tap(reify, _ => _ => {})
    final def tapRelfect(reflect: Reified => Option[A] => Unit): Alternative[A] = tap(_ => _ => {}, reflect)

    final def tap(reify: A => Option[Reified] => Unit, reflect: Reified => Option[A] => Unit): Alternative[A] =
      Alternative.Tapped(this, reify, reflect)
  }

  object Alternative extends LowPriorityAlternative {
    def of[A](implicit A: Alternative[A]): Alternative[A] = A

    def from[A](pairs: (A, Reified)*): Alternative[A] =
      reify(pairs.toMap).reflect(pairs.map(_.swap).toMap)

    def fromSymmetric[A](sym: A <-> Reified): Alternative[A] = new Alternative[A] {
      def reify(value: A, default: A => Reified): Option[Reified] = sym.to(value)
      def reflect(reified: Reified, default: Reified => Option[A]): Option[A] = sym.from(reified)
    }

    case class reify[A](reifyPF: PartialFunction[A, Reified]) {
      def reflect(reflectPF: PartialFunction[Reified, A]): Alternative[A] = new Alternative[A] {
        def reify(value: A, default: A => Reified): Option[Reified] = reifyPF.lift(value)
        def reflect(reified: Reified, default: Reified => Option[A]): Option[A] = reflectPF.lift(reified)
      }
    }

    case class withDefault[A](reifyPF: (A => Reified) => PartialFunction[A, Reified]) {
      def reflect(reflectPF: Extractor[Reified, A] => PartialFunction[Reified, A]): Alternative[A] = new Alternative[A] {
        def reify(value: A, default: A => Reified): Option[Reified] = reifyPF(default).lift(value)
        def reflect(reified: Reified, default: Reified => Option[A]): Option[A] = reflectPF(default).lift(reified)
      }
    }

    private case class Method[A](target: Reified, name: String, args: Alternative[A]) extends Alternative[A] {
      def reify(value: A, default: A => Reified): Option[Reified] =  for {
        reified     <- args.reify(value, default)
        reifiedArgs <- PartialFunction.condOpt(reified) {
          case RList(list) => list
        }
      } yield target.method(name, reifiedArgs)

      def reflect(reified: Reified, default: Reified => Option[A]): Option[A] = reified match {
        case RMethod(`target`, `name`, list) => args.reflect(RList(list), default)
        case _ => None
      }
    }

    private object Or {
      def create[A](lhs: Alternative[A], rhs: Alternative[A]): Or[A] = {
        def unwrap(alternative: Alternative[A]): List[Alternative[A]] = alternative match {
          case Or(alternatives) => alternatives
          case other            => List(other)
        }

        Or(unwrap(lhs) ::: unwrap(rhs))
      }
    }

    private case class Or[A](alternatives: List[Alternative[A]]) extends Alternative[A] {
      def reify(value: A, default: A => Reified): Option[Reified] =
        alternatives.flatMapFirst(_.reify(value, default))

      def reflect(reified: Reified, default: Reified => Option[A]): Option[A] =
        alternatives.flatMapFirst(_.reflect(reified, default))
    }

    private case class Tapped[A](
      delegateTo: Alternative[A],
      tapReify: A => Option[Reified] => Unit,
      tapReflect: Reified => Option[A] => Unit
    ) extends Alternative[A] {
      def reify(value: A, default: A => Reified): Option[Reified] =
        delegateTo.reify(value, default).tap(tapReify(value))

      def reflect(reified: Reified, default: Reified => Option[A]): Option[A] =
        delegateTo.reflect(reified, default).tap(tapReflect(reified))
    }
  }

  trait LowPriorityAlternative {
    implicit def defaultAlternative[A]: Alternative[A] = NoAlternative$.asInstanceOf[Alternative[A]]

    private object NoAlternative$ extends Alternative[Any] {
      def reify(value: Any, default: Any => Reified): Option[Reified] = None
      def reflect(reified: Reified, default: Reified => Option[Any]): Option[Any] = None
    }
  }

  type Typeclass[A] = Reify[A]

  def combine[A: Alternative](ctx: CaseClass[Reify, A]): Reify[A]    = ReifyImplementations.CaseClassReify(ctx)
  def dispatch[A: Alternative](ctx: SealedTrait[Reify, A]): Reify[A] = ReifyImplementations.SealedTraitReify(ctx)

  def gen[A]: Reify[A] = macro Magnolia.gen[A]

  def reify[A: Reify](value: A): Reified = of[A].reify(value)

  def reflect[A: Reify](reified: Reified): Option[A] = of[A].reflect(reified)

  def defer[A](deferred: => Reify[A]): Reify[A] = new ReifyImplementations.DeferredReify[A](deferred)

  def find(value: Any): Option[Reify[_]] = for {
    notNull <- Option(value)
    reify   <- find(notNull.getClass)
  } yield reify

  lazy val reifysByClass: Map[Class[_], Reify[_]] = Map(
    classOf[Int]     -> reifyInt,
    classOf[Integer] -> reifyInt,
    classOf[String]  -> reifyString,
    classOf[Long]    -> reifyLong,
    classOf[Boolean] -> reifyBoolean
  )
    
  lazy val orphanedInstances: Map[String, String] = 
    new Properties().loadMatching("reify.properties").asScala.toMap

  def find(clazz: Class[_]): Option[Reify[_]] = {
    def builtIn: Option[Reify[_]] = reifysByClass.get(clazz)

    def custom: Option[Reify[_]] = Try {
      import scala.reflect.runtime.{universe => ru}
      
      // TODO: This stringly typed stuff is just wrong, use reflection / mirrors / TypeName / whatever instead.
      val reifyTypeName: String = 
        s"${classTag.className[Reify[_]]}[${clazz.getName.replace('$', '.')}]"

      val companionClass: Class[_] = 
        orphanedInstances.get(clazz.getName).map(Class.forName(_)).getOrElse(clazz)
          
      val rootMirror: ru.Mirror = 
        ru.runtimeMirror(companionClass.getClassLoader)

      val companionSymbol: ru.Symbol = 
        rootMirror.classSymbol(companionClass) |> (if (orphanedInstances.contains(clazz.getName)) _.module else _.companion)

      val companionMirror: ru.InstanceMirror =
        rootMirror.reflect(rootMirror.reflectModule(companionSymbol.asModule).instance)

      val optCompanionReify: Option[Reify[_]] = companionSymbol.typeSignature.members.toList.collectFirst {
        case symbol
          if symbol.typeSignature.toString == reifyTypeName // cope with lazy vals
          => companionMirror.reflectField(symbol.asTerm).get.asInstanceOf[Reify[_]]

        case symbol
          if symbol.typeSignature.toString.stripPrefix("=> ") == reifyTypeName // cope with lazy vals
          => companionMirror.reflectMethod(symbol.asMethod).apply().asInstanceOf[Reify[_]]
      }
      
      optCompanionReify
    }.getOrElse(None)

    builtIn.orElse(custom)
  } // add cache

  def of[A](implicit A: Reify[A]): Reify[A] = 
    checker.check[A].calc(A)

  def apply[CC: ClassTag, A: Reify](
    rtype: RType,
    apply: A => CC,
    CC: Extractor[CC, A]
  ): Reify[CC] = checker.check[A].calc {
    ReifyImplementations.ManualCaseClass1Reify(rtype, apply, CC)
  }

  def apply[CC: ClassTag, A: Reify, B: Reify](
    rtype: RType,
    apply: (A, B) => CC,
    CC: Extractor[CC, (A, B)]
  ): Reify[CC] = checker.check[A].check[B].calc {
    ReifyImplementations.ManualCaseClass2Reify(rtype, apply, CC)
  }

  def apply[CC: ClassTag, A: Reify, B: Reify, C: Reify](
    rtype: RType,
    apply: (A, B, C) => CC,
    CC: Extractor[CC, (A, B, C)]
  ): Reify[CC] = checker.check[A].check[B].check[C].calc {
    ReifyImplementations.ManualCaseClass3Reify(rtype, apply, CC)
  }

  def pf[A](
    rtype: RType,
    reify: A => Reified,
    reflect: PartialFunction[Reified, A]
  ): Reify[A] = apply[A](rtype, reify, reflect.lift)

  def apply[A](
    rtype: RType,
    reify: A => Reified,
    reflect: Reified => Option[A]
  ): Reify[A] = ReifyImplementations.FromFunctions[A](rtype, reify, reflect)

  def sealedTrait[A: Alternative](rtype: RType, f: A => Reify[_ <: A], all: Reify[_ <: A]*): Reify[A] =
    new ReifyImplementations.ManualSealedTraitReify[A](rtype, f, all: _*)

  def infix[CC: ClassTag, A, B](
    name: String,
    apply: (A, B) => CC,
    CC: Extractor[CC, (A, B)]
  )(implicit A: Reify[A], B: Reify[B]): Reify[CC] = ReifyImplementations.InfixReify(name, apply, CC)
  
  private val checker: ForwardReferenceChecker[Reify] = ForwardReferenceChecker[Reify]
}

private[reify] object ReifyImplementations {

  //  case class UseAsArgumentsFor[A, B](from: Reify[A], name: String, f: A => B, g: B => A) extends Reify[B] {
  //    def reify(value: B): Reified = from.reify(g(value)).useAsArgumentsFor(name)
  //
  //    def reflect(reified: Reified): Option[B] =
  //      from.reflect(reified).map(f)
  //  }
  //

  case class CaseClassReify[A: Alternative](ctx: CaseClass[Reify, A]) extends Reify[A] {

    def rtype: RType = RType.TC0(ctx.typeName.short)

    def reify(value: A): Reified = {
      val result: Reified =
        Alternative.of[A].reify(value, reifyWithoutAlternative(_)).getOrElse(reifyWithoutAlternative(value))

      result
    }

    def reflect(value: Reified): Option[A] = {
      val result: Option[A] =
        Alternative.of[A].reflect(value, reflectWithoutAlternative(_)).orElse(reflectWithoutAlternative(value))

      result
    }

    private def reifyWithoutAlternative(value: A): Reified = {
      val result = if (ctx.isObject) {
        RPrimitive(typeName)
      } else {
        val fields: List[Reified] = {
          def reifyFields(useDefaults: Boolean): (Boolean, List[Reified]) = ctx.parameters.foldLeft((false, Nil: List[Reified])) {
            case ((defaulted, acc), parameter) => {
              val pvalue = parameter.dereference(value)

              parameter.default.filter(_ => useDefaults) match {
                case Some(`pvalue`) => (true, acc)
                case _ => (
                  defaulted,
                  parameter.typeclass.reify(pvalue).namedIf(
                    defaulted || pvalue.isInstanceOf[Boolean], parameter.label
                  ) :: acc
                )
              }
            }
          }

          val (defaulted, withDefaults) = reifyFields(useDefaults = true)

          if (!defaulted) withDefaults else {
            val (_, withoutDefaults) = reifyFields(useDefaults = false)

            Formatter.Unindented.shorter(withDefaults, withoutDefaults)
          }
        }

        RCaseClass(typeName, fields.reverse)
      }

      result
    }

    private def reflectWithoutAlternative(reified: Reified): Option[A] = {
      val stableTypeName: String = typeName.toLowerCase

      reified match {
        case RCaseClass(LC(`stableTypeName`), reifiedParameters) => {
          val Default: Extractor[Param[Reify, A], Param[Reify, A]#PType] =
            Extractor.from[Param[Reify, A]].apply(_.default)

          @tailrec
          def loop(
            acc: List[Any],
            currentParameters: List[Param[Reify, A]],
            currentReified: List[Reified]
          ): Option[A] = {
            (currentParameters, currentReified) match {
              case ((cparam @ Default(p)) :: params, Nil) => {
                loop(p :: acc, params, Nil)
              }
              case ((cparam@Default(p)) :: params, reifieds @ RInfix.named(name, _) :: _) if cparam.label != name => {
                loop(p :: acc, params, reifieds)
              }
              case (cparam :: params, RInfix.named.ignore(reifiedParam) :: reifieds) => {
                cparam.typeclass.reflect(reifiedParam) match {
                  case Some(p) => loop(p :: acc, params, reifieds)
                  case _       => None
                }
              }
              case _ => Some(ctx.rawConstruct(acc.reverse))
            }
          }

          loop(Nil, ctx.parameters.toList, reifiedParameters)
        }

        case _ => None
      }
    }
  }

  case class SealedTraitReify[A: Alternative](ctx: SealedTrait[Reify, A]) extends Reify[A] {
    def rtype: RType = RType.TC0(ctx.typeName.full)

    def reify(value: A): Reified =
      Alternative.of[A].reify(value, reifyWithoutAlternative(_)).getOrElse(reifyWithoutAlternative(value))

    def reflect(reified: Reified): Option[A] =
      Alternative.of[A].reflect(reified, reflectWithoutAlternative(_)).orElse(reflectWithoutAlternative(reified))

    private def reifyWithoutAlternative(value: A): Reified = ctx.dispatch(value)(subType => {
      subType.typeclass.reify(subType.cast(value))
    })

    private def reflectWithoutAlternative(reified: Reified): Option[A] = {
      val SubtypeReify: Extractor[String, Reify[A]] = Extractor.fromMap[String, Reify[A]](
        ctx.subtypes.map(subtype => subtype.typeName.short -> subtype.typeclass.asInstanceOf[Reify[A]]).toMap
      )

      reified match {
        case RCaseClass(SubtypeReify(reify), _) => reify.reflect(reified)
        case _ => ctx.subtypes.foldLeft(None: Option[A]) {
          case (Some(a), _) => Some(a)
          case (None, subtype) => subtype.typeclass.reflect(reified)
        }
      }
    }
  }


  case class ManualCaseClass1Reify[CC: ClassTag, A](
    rtype: RType,
    apply: A => CC,
    CC: Extractor[CC, A]
  )(implicit A: Reify[A]) extends Reify[CC] {
    val className: String = classTag.simpleClassName[CC]

    def reify(value: CC): Reified = value match {
      case CC(a) => RCaseClass(className, a)
    }

    def reflect(reified: Reified): Option[CC] = PartialFunction.condOpt(reified) {
      case RCaseClass(`className`, List(A(a))) => apply(a)
    }
  }

  private val LC: Extractor[String, String] = Extractor.from[String].property(_.toLowerCase)

  case class ManualCaseClass2Reify[CC: ClassTag, A, B](
    rtype: RType,
    apply: (A, B) => CC,
    CC: Extractor[CC, (A, B)]
  )(implicit A: Reify[A], B: Reify[B]) extends Reify[CC] {
    val className: String = classTag.simpleClassName[CC]

    def reify(value: CC): Reified = value match {
      case CC(a, b) => RCaseClass(className, a, b)
    }

    def reflect(reified: Reified): Option[CC] = PartialFunction.condOpt(reified) {
      case RCaseClass(`className`, List(A(a), B(b))) => apply(a, b)
    }
  }

  case class ManualCaseClass3Reify[CC: ClassTag, A, B, C](
    rtype: RType,
    apply: (A, B, C) => CC,
    CC: Extractor[CC, (A, B, C)]
  )(implicit A: Reify[A], B: Reify[B], C: Reify[C]) extends Reify[CC] {
    val className: String = classTag.simpleClassName[CC]

    def reify(value: CC): Reified = value match {
      case CC(a, b, c) => RCaseClass(className, a, b, c)
    }

    def reflect(reified: Reified): Option[CC] = PartialFunction.condOpt(reified) {
      case RCaseClass(`className`, List(A(a), B(b), C(c))) => apply(a, b, c)
    }
  }

  class ManualSealedTraitReify[A](
    val rtype: RType,
    f: A => Reify[_ <: A],
    all: Reify[_ <: A]*
  ) extends Reify[A] {

    def reify(value: A): Reified =
      Alternative.of[A].reify(value, reifyWithoutAlternative(_)).getOrElse(reifyWithoutAlternative(value))

    def reflect(reified: Reified): Option[A] = try {
      Alternative.of[A].reflect(reified, reflectWithoutAlternative(_)).orElse(reflectWithoutAlternative(reified))
    } catch {
      case t: Throwable => throw new RuntimeException(s"ManualSealedTraitReify($typeName).reflect($reified)", t)
    }

    private def reifyWithoutAlternative(value: A): Reified = f(value).asInstanceOf[Reify[A]].reify(value)

    private def reflectWithoutAlternative(reified: Reified): Option[A] = all.foldLeft(None: Option[A]) {
      case (result@Some(_), _) => result
      case (None, reify)       => reify.reflect(reified)
    }
  }

  case class FromFunctions[A](rtype: RType, reifyFn: A => Reified, reflectFn: Reified => Option[A]) extends Reify[A] {
    def reify(value: A): Reified = reifyFn(value)

    def reflect(reified: Reified): Option[A] = reflectFn(reified)
  }

  class DeferredReify[A](deferred: => Reify[A]) extends Reify[A] {
    def rtype: RType = _deferred.rtype

    def reify(value: A): Reified = _deferred.reify(value)

    def reflect(reified: Reified): Option[A] = _deferred.reflect(reified)

    override def typeName: String = _deferred.typeName

    private lazy val _deferred: Reify[A] = deferred
  }

  case class InfixReify[CC: ClassTag, A, B](
    name: String,
    apply: (A, B) => CC,
    CC: Extractor[CC, (A, B)]
  )(implicit A: Reify[A], B: Reify[B]) extends Reify[CC] {
    def rtype: RType = RType[A, B](name)

    def reify(value: CC): Reified = value match {
      case CC(a, b) => RInfix(a, name, b)
    }

    def reflect(reified: Reified): Option[CC] = PartialFunction.condOpt(reified) {
      case RInfix(A(a), `name`, B(b)) => apply(a, b)
    }

    override def typeName: String = name
  }

  case class BeforeAndAfter[A](from: Reify[A], beforeReify: Endo[A], afterReflect: Endo[A]) extends Reify[A] {
    def reify(value: A): Reified = from.reify(beforeReify(value))

    def reflect(reified: Reified): Option[A] = from.reflect(reified).map(afterReflect)

    def rtype: RType = from.rtype
  }
}

sealed trait Reify[A] extends Injector[A, Reified] {
  final def inject(value: A): Reified = reify(value)
  final def unapply(reified: Reified): Option[A] = reflect(reified)

  def reify(value: A): Reified

  def reflect(reified: Reified): Option[A]

  final def infix[B](separator: String, rhs: Reify[B])(implicit CC: ClassTag[(A, B)]): Reify[(A, B)] =
    Reify.infix[(A, B), A, B](separator, (a, b) => (a, b), Extractor.identity[(A, B)])(CC, this, rhs)
  
//  final def useAsArgumentsFor[B](name: String, f: A => B, g: B => A): Reify[B] =
//    Reify.UseAsArgumentsFor(this, name, f, g)

  override def toString: String = s"Reify[$typeName]"

  def typeName: String = rtype.typeName

  def rtype: RType

  final def beforeReify(f: Endo[A]): Reify[A] = ReifyImplementations.BeforeAndAfter(this, f, identity)
  
  final def afterReflect(f: Endo[A]): Reify[A] = ReifyImplementations.BeforeAndAfter(this, identity, f)
}



