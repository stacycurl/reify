package reify.internal.util

import reify.Reified
import reify.Reified.{RCaseObject, method}


object Around {
  def apply1(target: String): Around = Around("target", {
    case RCaseObject(`target`) method("", List(arg1)) => arg1
  }, {
    case arg1 => RCaseObject(target) method("", List(arg1))
  })

  def and(values: Seq[Around]): Around = if (values.isEmpty) empty else {
    Around(s"And(${values.map(_.name).mkString(", ")})", And(values.map(_.afterReify)), And(values.map(_.beforeReflect)))
  }

  val empty: Around = Around("empty", PartialFunction.empty[Reified, Reified], PartialFunction.empty[Reified, Reified])

  private case class And(disjuncts: Seq[PartialFunction[Reified, Reified]]) extends PartialFunction[Reified, Reified] {
    def isDefinedAt(reified: Reified): Boolean = disjuncts.exists(_.isDefinedAt(reified))

    def apply(reified: Reified): Reified = disjuncts.foldLeft(reified) {
      case (acc, pf) => pf.lift(acc).getOrElse(acc)
    }
  }
}

case class Around(
  name:          String,
  afterReify:    PartialFunction[Reified, Reified],
  beforeReflect: PartialFunction[Reified, Reified]
)