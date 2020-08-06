package reify

import reify.Reified.RPrimitive
import reify.internal.util.{Extractor, Fields}
import scalaz.deriving

// TODO: Track provenance of field via it's identityHashCode
//       Use provenance as a means of simplifying a series of replacements
//       If every reference to a field requires the same replacement, then change it to a replacement of the field
//       It will be necessary to expand which diff constitutes a failed test, if a test references a field that has changed, it
//         should report the change too.
object ReifiedFields {
  def create(value: Any): ReifiedFields =
    Fields.create(value).reify(value)
}

@deriving(Reify)
case class ReifiedFields(fields: Map[Reified, String]) {
  def reify[A: Reify](value: A): Reified = {
    replace(Reify.reify(value))
  }

  def replace[A: Reify](reified: Reified): Reified = {
    val Fields = Extractor.from[Reified](fields.get)

    val result: Reified = reified.transform {
      case Fields(fieldName) => RPrimitive(fieldName)
    }

    result
  }

  def namesShorterThanDefinitions = ReifiedFields(fields.filter {
    case (reified, name) => Formatter.Unindented.format(reified).length > name.length
  })
}