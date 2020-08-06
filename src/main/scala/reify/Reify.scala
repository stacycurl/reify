package reify

import scala.language.experimental.macros

import magnolia.{CaseClass, Magnolia, SealedTrait}
import reify.Reified.{RCaseClass, RCaseObject, RInfix, RList, RMethod}
import reify.Reify.Alternative
import reify.internal.prelude._
import reify.internal.util.{Around, Extractor, classTag}

import scala.reflect.ClassTag
import scala.util.Try


object Reify extends ReifyInstances {
  trait Companion[A] {
    def unapply(reified: Reified)(implicit A: Reify[A]): Option[A] = A.reflect(reified)

    object Many {
      def unapply(reified: Reified)(implicit A: Reify[List[A]]): Option[List[A]] = A.reflect(reified)
    }
  }

  trait Alternative[A] {
    final def method(target: Reified, name: String): Alternative[A] = Alternative.Method(target, name, this)

    final def ||(rhs: Alternative[A]): Alternative[A] = Alternative.Or.create(this, rhs)

    def reify(value: A, default: (A => Reified)): Option[Reified]

    def reflect(reified: Reified, default: (Reified => Option[A])): Option[A]

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
             |  ${from.indent("  ")}
             |) =
             |  ${to.indent("  ")}
             |""".stripMargin
        )

        val formatter = Formatter.Indented(80, escape = false)

        tap(
          reify   = dynamicElement => optReified => log("reify", dynamicElement, optReified),
          reflect = reified => optDynamicElement => log("reflect", formatter.format(reified), optDynamicElement)
        )
      }
    }

    final def tapReify(reify: A => Option[Reified] => Unit): Alternative[A] = tap(reify, _ => _ => {})
    final def tapRelfect(reflect: Reified => Option[A] => Unit): Alternative[A] = tap(_ => _ => {}, reflect)

    final def tap(reify: A => Option[Reified] => Unit, reflect: Reified => Option[A] => Unit): Alternative[A] =
      Alternative.Tapped(this, reify, reflect)
  }

  object Alternative extends LowPriorityAlternative {
    def of[A](implicit A: Alternative[A]): Alternative[A] = A

    case class reify[A](reifyPF: PartialFunction[A, Reified]) {
      def reflect(reflectPF: PartialFunction[Reified, A]): Alternative[A] = new Alternative[A] {
        def reify(value: A, default: (A => Reified)): Option[Reified] = reifyPF.lift(value)
        def reflect(reified: Reified, default: (Reified => Option[A])): Option[A] = reflectPF.lift(reified)
      }
    }

    case class withDefault[A](reifyPF: (A => Reified) => PartialFunction[A, Reified]) {
      def reflect(reflectPF: Extractor[Reified, A] => PartialFunction[Reified, A]): Alternative[A] = new Alternative[A] {
        def reify(value: A, default: (A => Reified)): Option[Reified] = reifyPF(default).lift(value)
        def reflect(reified: Reified, default: (Reified => Option[A])): Option[A] = reflectPF(default).lift(reified)
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
      def reify(value: A, default: (A => Reified)): Option[Reified] =
        delegateTo.reify(value, default).tap(tapReify(value))

      def reflect(reified: Reified, default: (Reified => Option[A])): Option[A] =
        delegateTo.reflect(reified, default).tap(tapReflect(reified))
    }
  }

  trait LowPriorityAlternative {
    implicit def defaultAlternative[A]: Alternative[A] = NoAlternative$.asInstanceOf[Alternative[A]]

    private object NoAlternative$ extends Alternative[Any] {
      def reify(value: Any, default: (Any => Reified)): Option[Reified] = None
      def reflect(reified: Reified, default: (Reified => Option[Any])): Option[Any] = None
    }
  }

  final case class name(msg: String) extends scala.annotation.StaticAnnotation
  final case class useVarArgs(name: String) extends scala.annotation.StaticAnnotation
  final case class around(around: Around) extends scala.annotation.StaticAnnotation

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

  private def find(clazz: Class[_]): Option[Reify[_]] = {
    val reifysByClass: Map[Class[_], Reify[_]] = Map(
      classOf[Integer] -> reifyInt,
      classOf[String]  -> reifyString
    )

    def builtIn: Option[Reify[_]] = reifysByClass.get(clazz)

    def custom: Option[Reify[_]] = Try {
      import scala.reflect.runtime.{universe => ru}

      // TODO: This stringly typed stuff is just wrong, use reflection / mirrors / TypeName / whatever instead.
      val reifyTypeName = s"${classTag.className[Reify[_]]}[${clazz.getName.replace('$', '.')}]"

      val rootMirror = ru.runtimeMirror(clazz.getClassLoader)
      val companionSymbol = rootMirror.classSymbol(clazz).companion
      val members: List[ru.Symbol] = companionSymbol.typeSignature.members.toList

      val companionInstance = rootMirror.reflectModule(companionSymbol.asModule)
      val companionMirror   = rootMirror.reflect(companionInstance.instance)

       val optCompanionReify: Option[Reify[_]] = members.collectFirst {
        case symbol
          if symbol.typeSignature.toString == reifyTypeName // cope with lazy vals
          => companionMirror.reflectField(symbol.asTerm).get.asInstanceOf[Reify[_]]
      }

      optCompanionReify
    }.getOrElse(None)

    builtIn.orElse(custom)
  }

  def tc1Name[A: Reify](name: String): String = s"$name[${typeName[A]}]"
  def tc2Name[A: Reify, B: Reify](name: String): String = s"$name[${typeName[A]}, ${typeName[B]}]"
  def tc3Name[A: Reify, B: Reify, C: Reify](name: String): String = s"$name[${typeName[A]}, ${typeName[B]}, ${typeName[C]}]"

  def typeName[A: Reify]: String = of[A].typeName

  def rtype[A: Reify]: RType = of[A].rtype

  def of[A](implicit A: Reify[A]): Reify[A] = {
    if (A == null) {
      sys.error("Looks like you have a forward reference")
    }

    A
  }

  def apply[CC: ClassTag, A](
    rtype: RType,
    apply: A => CC,
    CC: Extractor[CC, A]
  )(implicit A: Reify[A]): Reify[CC] = ReifyImplementations.ManualCaseClass1Reify(rtype, apply, CC)

  def apply[CC: ClassTag, A, B](
    rtype: RType,
    apply: (A, B) => CC,
    CC: Extractor[CC, (A, B)]
  )(implicit A: Reify[A], B: Reify[B]): Reify[CC] = ReifyImplementations.ManualCaseClass2Reify(rtype, apply, CC)

  def apply[CC: ClassTag, A, B, C](
    rtype: RType,
    apply: (A, B, C) => CC,
    CC: Extractor[CC, (A, B, C)]
  )(implicit A: Reify[A], B: Reify[B], C: Reify[C]): Reify[CC] = ReifyImplementations.ManualCaseClass3Reify(rtype, apply, CC)

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

    def rtype: RType = RType.TC0(ctx.annotations.collectFirst {
      case Reify.name(name) => name
    }.getOrElse(ctx.typeName.short))

    private val useVarArgs: Option[String] = ctx.annotations.collectFirst {
      case Reify.useVarArgs(name: String) => name
    }

    val around: Around = Around.and(ctx.annotations.collect {
      case Reify.around(around) => around
    })

    def reify(value: A): Reified = {
      val afterReify: Reified =
        Alternative.of[A].reify(value, reifyWithoutAlternative(_)).getOrElse(reifyWithoutAlternative(value))

      val result: Reified =
        afterReify.transform(around.afterReify)

      result
    }

    def reflect(value: Reified): Option[A] = {
      val beforeReflect: Reified =
        value.transform(around.beforeReflect)

      val result: Option[A] =
        Alternative.of[A].reflect(beforeReflect, reflectWithoutAlternative(_)).orElse(reflectWithoutAlternative(beforeReflect))

      result
    }

    private def reifyWithoutAlternative(value: A): Reified = {
      val result = if (ctx.isObject) {
        RCaseObject(typeName)
      } else {
        val fields: List[Reified] = ctx.parameters.foldLeft(Nil: List[Reified]) {
          case (acc, parameter) => parameter.typeclass.reify(parameter.dereference(value)) :: acc
        }

        val caseClass = RCaseClass(typeName, fields.reverse)

        useVarArgs match {
          case None       => caseClass
          case Some(name) => caseClass.useAsArgumentsFor(name)
        }
      }

      result
    }

    private def reflectWithoutAlternative(reified: Reified): Option[A] = {
      reified match {
        case RCaseClass(_, reifiedParameters) => {
          val optResult: Option[List[Any]] = {
            ctx.parameters.zip(reifiedParameters).foldLeft(Some(Nil: List[Any]): Option[List[Any]]) {
              case (optAcc, (cparam, reifiedParam)) =>
                (optAcc, cparam.typeclass.reflect(reifiedParam)) match {
                  case (Some(acc), Some(p)) => Some(p :: acc)
                  case _                    => None
                }
            }
          }

          optResult.map(result => ctx.rawConstruct(result.reverse))
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
      case (result@(Some(_)), _) => result
      case (None, reify)         => reify.reflect(reified)
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
    def rtype: RType = RType.tc2[A, B](name)

    def reify(value: CC): Reified = value match {
      case CC(a, b) => RInfix(a, name, b)
    }

    def reflect(reified: Reified): Option[CC] = PartialFunction.condOpt(reified) {
      case RInfix(A(a), `name`, B(b)) => apply(a, b)
    }

    override def typeName: String = name
  }

  case class TransformReflected[A](from: Reify[A], f: A => A) extends Reify[A] {
    def reify(value: A): Reified = from.reify(value)

    def reflect(reified: Reified): Option[A] = from.reflect(reified).map(f)

    def rtype: RType = from.rtype
  }
}

sealed trait Reify[A] {
  final def unapply(reified: Reified): Option[A] = reflect(reified)

  def reify(value: A): Reified

  def reflect(reified: Reified): Option[A]

//  final def useAsArgumentsFor[B](name: String, f: A => B, g: B => A): Reify[B] =
//    Reify.UseAsArgumentsFor(this, name, f, g)

  override def toString: String = s"Reify[$typeName]"

  def typeName: String = rtype.typeName

  def rtype: RType

  final def transformReflected(f: A => A): Reify[A] = ReifyImplementations.TransformReflected(this, f)
}


sealed trait RType {
  def typeName: String
}

object RType {
  def tc1[A: Reify](name: String): RType = TC1[A](name, Reify.rtype[A])
  def tc2[A: Reify, B: Reify](name: String): RType = TC2[A, B](name, Reify.rtype[A], Reify.rtype[B])
  def tc3[A: Reify, B: Reify, C: Reify](name: String): RType = TC3[A, B, C](name, Reify.rtype[A], Reify.rtype[B], Reify.rtype[C])

  case class TC0(typeName: String) extends RType

  case class TC1[A: Reify](name: String, arg1: RType) extends RType {
    def typeName: String = s"$name[${arg1.typeName}]"

    def reify1: Reify[A] = Reify.of[A]
  }

  case class TC2[A: Reify, B: Reify](name: String, arg1: RType, arg2: RType) extends RType {
    def typeName: String = s"$name[${arg1.typeName}, ${arg2.typeName}]"

    def reify1: Reify[A] = Reify.of[A]
    def reify2: Reify[B] = Reify.of[B]
  }

  case class TC3[A: Reify, B: Reify, C: Reify](name: String, arg1: RType, arg2: RType, arg3: RType) extends RType {
    def typeName: String = s"$name[${arg1.typeName}, ${arg2.typeName}, ${arg3.typeName}]"

    def reify1: Reify[A] = Reify.of[A]
    def reify2: Reify[B] = Reify.of[B]
    def reify3: Reify[C] = Reify.of[C]
  }
}