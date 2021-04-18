package reify

import org.scalatest.FreeSpec
import reify.Reified.{RBoolean, RCaseClass, REither, RInfix, RInt, RList, RMap, RMethod, ROption, RPrimitive, RString}
import reify.Token.{Arguments, Compound, Infix, Method, Primitive, TString}


class ReifiedSpec extends FreeSpec {
  "reify" - {
    "RBoolean" in {
      assert(
        Reify.reify(RBoolean(true): Reified) ===
        RCaseClass(RType("RBoolean"), List(RPrimitive("true")))
      )
    }

    "RInt" in {
      assert(
        Reify.reify(RInt(123): Reified) ===
        RCaseClass(RType("RInt"), List(RPrimitive("123")))
      )
    }

    "RString" in {
      assert(
        Reify.reify(RString("Foo"): Reified) ===
        RCaseClass(RType("RString"), List(RString("Foo")))
      )
    }

    "RPrimitive" in {
      assert(
        Reify.reify(RPrimitive("Foo"): Reified) ===
        RCaseClass(RType("RPrimitive"), List(RString("Foo")))
      )
    }

    "ROption" - {
      "none" in {
        assert(
          Reify.reify(ROption(None): Reified) ===
          RCaseClass(RType("RPrimitive"), List(RString("None")))
        )
      }

      "some" in {
        assert(
          Reify.reify(ROption(Some(RInt(123))): Reified) ===
          RCaseClass(RType("ROption"), List(RCaseClass(RType("Some"), List(RCaseClass(RType("RInt"), List(RPrimitive("123")))))))
        )
      }
    }

    "REither" - {
      "left" in {
        assert(
          Reify.reify(REither(Left(RPrimitive("left"))): Reified) === RCaseClass(
            RType("REither"), List(REither(Left(RCaseClass(RType("RPrimitive"), List(RString("left"))))))
          )
        )
      }

      "right" in {
        assert(
          Reify.reify(REither(Right(RInt(123))): Reified) === RCaseClass(
            RType("REither"), List(REither(Right(RCaseClass(RType("RInt"), List(RInt(123))))))
          )
        )
      }
    }

    "RList" - {
      "empty" in {
        assert(Reify.reify(RList(Nil): Reified) === RCaseClass(RType("RList"), List(RPrimitive("Nil"))))
      }

      "nonEmpty" in {
        assert(Reify.reify(RList(List(RInt(123), RInt(456))): Reified) === RCaseClass(
          RType("RList"),
          List(
            RList(List(RCaseClass(RType("RInt"), List(RInt(123))), RCaseClass(RType("RInt"), List(RInt(456)))))
          )
        ))
      }
    }

    "RMap" - {
      "empty" in {
        assert(Reify.reify(RMap(Map.empty): Reified) === RCaseClass(
          RType("RMap"), List(RMethod(RPrimitive("Map"), "empty", Nil)))
        )
      }

      "nonEmpty" in {
        assert(Reify.reify(RMap(Map(RString("key") -> RInt(123))): Reified) === RCaseClass(
          RType("RMap"),
          List(
            RMap(
              Map(
                RCaseClass(RType("RString"), List(RString("key"))) -> RCaseClass(RType("RInt"), List(RInt(123)))
              )
            )
          )
        ))
      }
    }

    "RCaseClass" in {
      assert(Reify.reify(RCaseClass(RType("Foo"), List(RInt(3))): Reified) === RCaseClass(
        RType("RCaseClass"),
        List(
          RCaseClass(RType("RType"), List(RString("Foo"))),
          RList(List(RCaseClass(RType("RInt"), List(RInt(3)))))
        )
      ))
    }

    "Infix" in {
      assert(Reify.reify(RInfix(123, " + ", 456): Reified) === RCaseClass(
        RType("RInfix"), List(RInt(123), RString(" + "), RInt(456))
      ))
    }

    "Method" - {
      "0 parameters" in {
        assert(Reify.reify(RMethod(RPrimitive("Foo"), "oof", Nil): Reified) === RCaseClass(
          RType("RMethod"),
          List(RCaseClass(RType("RPrimitive"), List(RString("Foo"))), RString("oof"), RPrimitive("Nil"))
        ))
      }

      "n parameters" in {
        assert(Reify.reify(RMethod(RPrimitive("Foo"), "oof", List(RInt(123))): Reified) === RCaseClass(
          RType("RMethod"),
          List(
            RCaseClass(RType("RPrimitive"), List(RString("Foo"))),
            RString("oof"),
            RList(List(RCaseClass(RType("RInt"), List(RInt(123)))))
          )
        ))
      }
    }
  }

  "reify.reflect" - {
    def roundtripEquals[A: Reify](a: A): Unit = {
      val reified: Reified     = Reify.reify(a)
      val reflected: Option[A] = Reify.reflect(reified)

      assert(reflected === Some(a))
    }

    "RBoolean" in {
      roundtripEquals(RBoolean(true): Reified)
    }

    "RInt" in {
      roundtripEquals(RInt(123): Reified)
    }

    "RString" in {
      roundtripEquals(RString("Foo"): Reified)
    }

    "RPrimitive" in {
      roundtripEquals(RPrimitive("Foo"): Reified)
    }

    "ROption" - {
      "none" in {
        roundtripEquals(ROption(None): Reified)
      }

      "some" in {
        roundtripEquals(ROption(Some(RInt(123))): Reified)
      }
    }

    "REither" - {
      "left" in {
        roundtripEquals(REither(Left(RPrimitive("left"))): Reified)
      }

      "right" in {
        roundtripEquals(REither(Right(RInt(123))): Reified)
      }
    }

    "RList" - {
      "empty" in {
        roundtripEquals(RList(Nil): Reified)
      }

      "nonEmpty" in {
        roundtripEquals(RList(List(RInt(123), RInt(456))): Reified)
      }
    }

    "RMap" - {
      "empty" in {
        roundtripEquals(RMap(Map.empty): Reified)
      }

      "nonEmpty" in {
        roundtripEquals(RMap(Map(RString("key") -> RInt(123))): Reified)
      }
    }

    "RCaseClass" in {
      roundtripEquals(RCaseClass("Foo", 3, "four"): Reified)
    }

    "Infix" in {
      roundtripEquals(RInfix(123, " + ", 456): Reified)
    }

    "Method" - {
      "0 parameters" in {
        roundtripEquals(RMethod(RPrimitive("Foo"), "oof", Nil): Reified)
      }

      "n parameters" in {
        roundtripEquals(RMethod(RPrimitive("Foo"), "oof", List(RInt(123))): Reified)
      }
    }
  }

  "tokenize" - {
    "RBoolean" in {
      assert(RBoolean(true).tokenize === Primitive("true"))
    }

    "RInt" in {
      assert(RInt(123).tokenize === Primitive("123"))
    }

    "RString" - {
      "double quotes" in {
        assert(RString("Foo").tokenize === TString("Foo"))
      }

      "triple quotes" in {
        assert(RString("""Foo""").tokenize === TString("Foo"))
      }

      "triple quotes containing a quote" in {
        assert(RString("""Foo " ooF""").tokenize === TString("Foo \" ooF"))
      }
    }

    "RPrimitive" - {
      "double quotes" in {
        assert(RPrimitive("Foo").tokenize === Primitive("Foo"))
      }

      "triple quotes containing a quote" in {
        assert(RPrimitive("""Foo " ooF""").tokenize === Primitive("""Foo " ooF"""))
      }
    }

    "ROption" - {
      "none" in {
        assert(ROption(None).tokenize === Primitive("None"))
      }

      "some" in {
        assert(ROption(Some(RInt(123))).tokenize === Compound("Some", Arguments(List(Primitive("123")))))
      }
    }

    "RList" - {
      "empty" in {
        assert(RList(Nil).tokenize === Compound("List", Arguments(List())))
      }

      "nonEmpty" in {
        assert(RList(List(RInt(123), RInt(456))).tokenize === Compound(
          "List", Arguments(List(Primitive("123"), Primitive("456")))
        ))
      }
    }

    "RMap" - {
      "empty" in {
        assert(RMap(Map.empty).tokenize === Compound("Map", Arguments(List())))
      }

      "nonEmpty" in {
        assert(RMap(Map(RString("key") -> RInt(123))).tokenize === Token.compound(
          "Map",
          List(Token.infix(Token.TString("key"), " -> ", Token.primitive("123")))
        ))
      }
    }

    "RCaseClass" in {
      assert(RCaseClass("Foo", 3).tokenize === Compound("Foo", Arguments(List(Primitive("3")))))
    }

    "Infix" in {
      // Token.infix(Token.primitive("123"), " + ", Token.primitive("456"))
      assert(RInfix(123, " + ", 456).tokenize === Infix(Primitive("123"), " + ", Primitive("456")))
    }

    "Method" - {
      "0 parameters" in {
        assert(RMethod(RPrimitive("Foo"), "oof", Nil).tokenize === Method(Primitive("Foo"), "oof", Arguments(List())))
      }

      "n parameters" in {
        assert(RMethod(RPrimitive("Foo"), "oof", List(RInt(123))).tokenize === Method(
          Primitive("Foo"), "oof", Arguments(List(Primitive("123")))
        ))
      }
    }
  }

  "parse" - {
    "boolean" - {
      "unnamed" in {
        assert(Reified.parse(Primitive("true"))  === Some(RBoolean(true)))
        assert(Reified.parse(Primitive("false")) === Some(RBoolean(false)))
      }

      "named" in {
        assert(Reified.parse(Infix(Primitive("name"), " = ", Primitive("true")))  === Some(
          RInfix(RPrimitive("name"), " = ", RBoolean(true))
        ))
        
        assert(Reified.parse(Infix(Primitive("name"), " = ", Primitive("false"))) === Some(
          RInfix(RPrimitive("name"), " = ", RBoolean(false))
        ))
      }
    }
    "int" in {
      assert(Reified.parse(Primitive("123")) === Some(RInt(123)))
    }

    "string" in {
      assert(Reified.parse(TString("Foo")) === Some(RString("Foo")))
      // TODO: What about strings containing double quotes ?
    }

    "option" - {
      "none" in {
        assert(Reified.parse(Primitive("None")) === Some(RPrimitive("None")))
      }

      "some" in {
        assert(Reified.parse(Compound("Some", Arguments(List(Primitive("123"))))) === Some(ROption(Some(RInt(123)))))
      }
    }

    "list" - {
      "empty" in {
        assert(Reified.parse(Compound("List", Arguments(List()))) === Some(RCaseClass(RType("List"), Nil)))
      }

      "nonEmpty" in {
        assert(Reified.parse(Compound("List", Arguments(List(Primitive("123"), Primitive("456"))))) === Some(
          RCaseClass(RType("List"), List(RInt(123), RInt(456)))
        ))
      }
    }

    "RMap" - {
      "empty" in {
        assert(Reified.parse(Compound("Map", Arguments(List()))) === Some(RMap(Map.empty)))
      }

      "nonEmpty" in {
        assert(Reified.parse(Token.compound(
          "Map",
          List(Token.infix(Token.primitive("key"), " -> ", Token.primitive("123")))
        )) === Some(RMap(Map(RPrimitive("key") -> RInt(123)))))
      }
    }

    "RCaseClass" in {
      assert(Reified.parse(Compound("Foo", Arguments(List(Primitive("3"))))) === Some(RCaseClass(RType("Foo"), List(RInt(3)))))
    }

    "Infix" in {
      assert(Reified.parse(Infix(Primitive("123"), " + ", Primitive("456"))) === Some(RInfix(123, " + ", 456)))
    }

    "Method" - {
      "0 parameters" in {
        assert(Reified.parse(Method(Primitive("Foo"), "oof", Arguments(List()))) === Some(RMethod(RPrimitive("Foo"), "oof", Nil)))
      }

      "n parameters" in {
        assert(Reified.parse(Method(Primitive("Foo"), "oof", Arguments(List(Primitive("123"))))) === Some(
          RMethod(RPrimitive("Foo"), "oof", List(RInt(123)))
        ))
      }
    }
  }
}
