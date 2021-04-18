package reify

import java.io.File
import org.scalatest.FreeSpec
import reify.Reified.{RBoolean, RCaseClass, RClass, RInfix, RInt, RPrimitive, RString}
import scalaz.annotation.deriving


class ReifySpec extends FreeSpec {
  "HasInt" in {
    assert(roundTrip(HasInt(123)) === Some(HasInt(123)))

    assert(Reify.reify(HasInt(123)) === RCaseClass(RType("HasInt"), List(RInt(123))))
  }

  "HasBoolean" in {
    assert(roundTrip(HasBoolean(false)) === Some(HasBoolean(false)))

    assert(Reify.reify(HasBoolean(false)) === RCaseClass(RType("HasBoolean"), List(RInfix(RPrimitive("b"), " = ", RBoolean(false)))))
  }

  "HasDefault" - {
    "non-default" in {
      assert(roundTrip(HasDefault(456)) === Some(HasDefault(456)))

      assert(Reify.reify(HasDefault(456)) === RCaseClass(RType("HasDefault"), List(RInt(456))))
    }

    "default" in {
      assert(roundTrip(HasDefault()) === Some(HasDefault()))

      assert(Reify.reify(HasDefault()) === RCaseClass(RType("HasDefault"), Nil))
    }
  }

  "HasDefaultThenNonDefault" - {
    "non-default" in {
      assert(roundTrip(HasDefaultThenNonDefault(456, 123)) === Some(HasDefaultThenNonDefault(456, 123)))

      assert(Reify.reify(HasDefaultThenNonDefault(456, 123)) === RCaseClass(
        RType("HasDefaultThenNonDefault"), List(RInt(456), RInt(123))
      ))
    }

    "default, next unnamed" in {
      assert(roundTrip(HasDefaultThenNonDefault(123, 456)) === Some(HasDefaultThenNonDefault(123, 456)))

      assert(Reify.reify(HasDefaultThenNonDefault(123, 456)) === RCaseClass(
        RType("HasDefaultThenNonDefault"), List(RInfix(RPrimitive("n"), " = ", RInt(456))
      )))
    }

    "default, next named" in {
      assert(roundTrip(HasDefaultThenNonDefault(n = 456)) === Some(HasDefaultThenNonDefault(n = 456)))

      assert(Reify.reify(HasDefaultThenNonDefault(n = 456)) === RCaseClass(
        RType("HasDefaultThenNonDefault"), List(RInfix(RPrimitive("n"), " = ", RInt(456))
      )))
    }
  }

  "HasTwoDefaults" - {
    "all non-defaulted" in {
      assert(roundTrip(HasTwoDefaults(100, 200)) === Some(HasTwoDefaults(100, 200)))

      assert(Reify.reify(HasTwoDefaults(100, 200)) === RCaseClass(
        RType("HasTwoDefaults"), List(RInt(100), RInt(200)
      )))
    }

    "first defaulted" in {
      assert(roundTrip(HasTwoDefaults(n = 200)) === Some(HasTwoDefaults(n = 200)))

      assert(Reify.reify(HasTwoDefaults(n = 200)) === RCaseClass(
        RType("HasTwoDefaults"), List(RInfix(RPrimitive("n"), " = ", RInt(200))
      )))
    }

    "second defaulted" in {
      assert(roundTrip(HasTwoDefaults(100)) === Some(HasTwoDefaults(100)))

      assert(Reify.reify(HasTwoDefaults(100)) === RCaseClass(RType("HasTwoDefaults"), List(RInt(100))))
    }

    "both defaulted" in {
      assert(roundTrip(HasTwoDefaults()) === Some(HasTwoDefaults()))

      assert(Reify.reify(HasTwoDefaults()) === RCaseClass(RType("HasTwoDefaults"), Nil))
    }
  }

  "HasTwoDefaultsThenNon" - {
    "all non-defaulted" in {
      assert(roundTrip(HasTwoDefaultsThenNon(100, 200, 300)) === Some(HasTwoDefaultsThenNon(100, 200, 300)))

      assert(Reify.reify(HasTwoDefaultsThenNon(100, 200, 300)) === RCaseClass(
        RType("HasTwoDefaultsThenNon"), List(RInt(100), RInt(200), RInt(300)
      )))
    }

    "first defaulted" in {
      assert(roundTrip(HasTwoDefaultsThenNon(n = 200, l = 300)) === Some(HasTwoDefaultsThenNon(n = 200, l = 300)))

      assert(Reify.reify(HasTwoDefaultsThenNon(n = 200, l = 300)) === RCaseClass(
        RType("HasTwoDefaultsThenNon"),
        List(RInfix(RPrimitive("n"), " = ", RInt(200)), RInfix(RPrimitive("l"), " = ", RInt(300)))
      ))
    }

    "second defaulted" in {
      assert(roundTrip(HasTwoDefaultsThenNon(100, l = 300)) === Some(HasTwoDefaultsThenNon(100, l = 300)))

      assert(Reify.reify(HasTwoDefaultsThenNon(100, l = 300)) === RCaseClass(
        RType("HasTwoDefaultsThenNon"), List(RInt(100), RInfix(RPrimitive("l"), " = ", RInt(300)))
      ))
    }

    "both defaulted" in {
      assert(roundTrip(HasTwoDefaultsThenNon(l = 100)) === Some(HasTwoDefaultsThenNon(l = 100)))

      assert(Reify.reify(HasTwoDefaultsThenNon(l = 100)) === RCaseClass(
        RType("HasTwoDefaultsThenNon"), List(RInfix(RPrimitive("l"), " = ", RInt(100))
      )))
    }
  }

  "bar" in {
    assert(roundTrip(Bar(123, 456)) === Some(Bar(123, 456)))

    assert(Reify.reify(Bar(123, 456)) === RCaseClass(RType("Bar"), List(RInt(123), RInt(456))))
  }

  "find" - {
    "built ins" in {
      assert(Reify.find(123)   === Some(Reify.of[Int]))
      assert(Reify.find("foo") === Some(Reify.of[String]))
    }

    "custom" in {
      assert(Reify.find(Contents("boom"))   === Some(Reify.of[Contents]))
    }
  }
  
  "file" in {
    assert(Reify.reify(new File("some/path")) === RClass(RType("File"), List(RString("some/path"))))
  }

  private def roundTrip[A: Reify](value: A): Option[A] =
    Reify.reflect(Reify.reify(value))
}


@deriving(Reify)
case class HasInt(value: Int)

@deriving(Reify)
case class HasBoolean(b: Boolean)

@deriving(Reify)
case class HasDefault(value: Int = 123)

@deriving(Reify)
case class HasDefaultThenNonDefault(value: Int = 123, n: Int)

@deriving(Reify)
case class HasTwoDefaults(value: Int = 123, n: Int = 456)

@deriving(Reify)
case class HasTwoDefaultsThenNon(value: Int = 1230000, n: Int = 456, l: Int)

@deriving(Reify)
case class Bar(i: Int, anotherInt: Int)