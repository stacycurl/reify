package reify

import org.scalatest.FreeSpec
import reify.Reified.{RCaseClass, RInt, RList, RLong, RMap, RPrimitive, RString}
import scalaz.annotation.deriving


class ReifiedFieldsSpec extends FreeSpec {
  "create" in {
    assert(ReifiedFields.create(new ClassWithReifiableFields) === ReifiedFields(
      Map(
        RInt(123) -> "anInt",
        RString("foo") -> "aString",
        RCaseClass(RType("Contents"), List(RString("beep-boop"))) -> "someFile",
        RString("private") -> "privateString"
      )
    ))
  }

  "namesShorterThanDefinitions" in {
    assert(ReifiedFields(
      Map(
        RInt(123) -> "anInt",
        RLong(12345678900L) -> "aLong",
        RString("foo") -> "aString",
        RCaseClass(RType("Contents"), List(RString("beep-boop"))) -> "someFile",
        RString("private") -> "privateString"
      )
    ).namesShorterThanDefinitions === ReifiedFields(
      Map(
        RCaseClass(RType("Contents"), List(RString("beep-boop"))) -> "someFile",
        RLong(12345678900L) -> "aLong"
      )
    ))
  }

  "reify" in {
    val reifiedFields = ReifiedFields(Map(RInt(123) -> "i", RString("foo") -> "foo"))

    assert(reifiedFields.reify(123) === RPrimitive("i"))

    assert(reifiedFields.reify(List(123, 456)) === RList(List(RPrimitive("i"), RInt(456))))

    assert(reifiedFields.reify(Map("foo" -> 123)) === RMap(Map(RPrimitive("foo") -> RPrimitive("i"))))

    assert(reifiedFields.reify(Contents("foo")) === RCaseClass("Contents", RPrimitive("foo")))
  }
}

class ClassWithReifiableFields {
  val anInt: Int = 123

  val aString: String = "foo"

  val someFile: Contents = Contents("beep-boop")

  // TODO [19 Nov 2020] Adding this requires adding a deferred to Reified
  //  lazy val aLazyListOfString: List[String] = List("fizzle")

  private val privateString: String = "private"
}

@deriving(Reify)
case class Contents(value: String)
