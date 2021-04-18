package reify

import org.scalatest.FreeSpec
import reify.Reified._
import reify.Token.{Arguments, Compound, Identifier, Infix, Method, Primitive, TString}

import scala.collection.immutable.{List, Nil}


class TokenSpec extends FreeSpec {
  "format" - {
    "Primitive" in {
      assert(Formatter.Unindented.format(abc) === "abc")
    }

    "TString" - {
      "one liner" in {
        assert(
          Formatter.Unindented.format(TString("Here's a one line string.")) ===
          """"Here's a one line string.""""
        )
      }

      "with newlines" in {
        val threeQuotes = "\"\"\""

        assert(Formatter.Unindented.format(TString(
          """Here's a string.
            |
            |It has several lines.
            |
            |Here's the last line.""".stripMargin)
        ) ===
          s"""${threeQuotes}Here's a string.
            ~  |
            ~  |It has several lines.
            ~  |
            ~  |Here's the last line.${threeQuotes}.stripMargin""".stripMargin('~')
        )
      }
    }

    "Infix" - {
      "static" in {
        assert(
          Formatter.Unindented.format(Infix(Primitive("lhs"), " ~> ", Primitive("rhs"))) ===
          "lhs ~> rhs"
        )
      }

      "dynamic" in {
        assert(
          Formatter.Unindented.format(Primitive("lhs") ~> Primitive("rhs")) ===
          "lhs ~> rhs"
        )
      }
    }

    "Method" - {
      "unindented" - {
        "no parameters" in {
          assert(
            Formatter.Unindented.format(Method(Primitive("target"), "method", Arguments(Nil))) ===
            "target.method"
          )
        }

        "with parameters" in {
          assert(
            Formatter.Unindented.format(
              Method(Primitive("target"), "method", Arguments(List(Primitive("p1"), Primitive("p2"))))
            ) ===
            "target.method(p1, p2)"
          )
        }
      }

      "indented" - {
        "no parameters" in {
          assert(
            noMargin.format(Method(Primitive("target"), "method", Arguments(Nil))) ===
            "target.method"
          )
        }

        "with parameters" in {
          assert(
            noMargin.format(
              Method(Primitive("target"), "method", Arguments(List(Primitive("p1"), Primitive("p2"))))
            ) === "target.method(p1, p2)"
          )
        }
      }
    }

    "Compound" - {
      "unindented" in {
        assert(
          Formatter.Unindented.format(Compound("Foo", Arguments(List(Primitive("3"))))) ===
          "Foo(3)"
        )
      }

      "indented" - {
        val one   = Primitive("1")
        val two   = Primitive("2")
        val three = Primitive("3")

        "non-nested" in {
          assert(
            noMargin.format(list(one, two, three)) ===
              """List(
                |  1,
                |  2,
                |  3
                |)""".stripMargin
          )
        }

        "nested" in {
          assert(
            noMargin.format(list(one, set(two, three))) ===
            """List(
              |  1,
              |  Set(
              |    2,
              |    3
              |  )
              |)""".stripMargin
          )
        }
      }

      "wrapping" - {
        val ones   = Primitive("111")
        val twos   = Primitive("222")
        val threes = Primitive("333")
        val fours  = Primitive("444")

        "non-nested" in {
          assert(
            Formatter.Indented(margin = 22, escape = false).format(list(ones, twos, threes, fours)) ===
            """List(
              |  111, 222, 333, 444
              |)""".stripMargin            
          )
        }

        "nested" in {
          assert(
            Formatter.Indented(margin = 24, escape = false).format(
              set(list(ones, twos), list(ones, twos, threes, fours))
            ) ===  
            """Set(
              |  List(111, 222),
              |  List(111, 222, 333, 444)
              |)""".stripMargin            
          )
        }
      }

      "with infix" in {
        assert(
          Formatter.Unindented.format(Reify.reify(Map("key" -> 123)).tokenize) ===
          """Map("key" -> 123)"""
        )
      }

      "complex with infix" in {
        assert(
          Formatter.Unindented.format(Reify.reify(Reify.reify(Map("key" -> 123))).tokenize) ===
          """RMap(Map(RString("key") -> RInt(123)))"""
        )
      }

      "complex with method" in {
        assert(
          Formatter.Unindented.format(
            Reify.reify(RMethod(RPrimitive("Foo"), "oof", Nil): Reified).tokenize
          ) === """RMethod(RPrimitive("Foo"), "oof", Nil)"""
        )
      }

      "more complex with method" in {
        assert(
          Formatter.Unindented.format(
            Reify.reify(RMethod(RPrimitive("Foo"), "oof", List(RInt(123))): Reified).tokenize
          ) === """RMethod(RPrimitive("Foo"), "oof", List(RInt(123)))"""
        )
      }
    }
  }

  "parse" - {
    "primitive" in {
      assert(Token.parseToken("abc") === Right(abc))
    }

    "tstring" - {
      "double quoted" in {
        assert(Token.parseToken("\"abc\"") === Right(TString("abc")))
      }

      "triple quoted" in {
        assert(Token.parseToken("\"\"\"abc\"\"\"") === Right(TString("abc")))
      }
    }

    "compound" - {
      "no args" in {
        assert(Token.parseToken("abc()") === Right(Compound("abc", Arguments(Nil))))
      }

      "one arg" in {
        assert(Token.parseToken("abc(def)") === Right(Compound("abc", Arguments(List(Identifier("def"))))))
      }

      "many args" in {
        assert(Token.parseToken("abc(def, ghi)") === Right(Compound("abc", Arguments(List(Identifier("def"), ghi)))))
      }

      "nested" in {
        assert(Token.parseToken("2") === Right(Primitive("2")))

        assert(Token.parseToken("subtract(2, 1)") === Right(
          Compound("subtract", Arguments(List(Primitive("2"), Primitive("1"))))
        ))
      }
    }

    "method" - {
      "one" in {
        assert(Token.parseToken("abc.def(ghi, jkl)") === Right(Method(abc, "def", Arguments(List(ghi, Identifier("jkl"))))))
      }

      "chain" in {
        assert(Token.parseToken("abc.def(ghi).jkl(mno).pqr(stu)") === Right(
          abc.method("def", ghi).method("jkl", Identifier("mno")).method("pqr", Identifier("stu"))
        ))
      }
    }

    "infix" - {
      "simple" in {
        assert(Token.parseToken("key := value") === Right(Infix(Primitive("key"), ":=", Primitive("value"))))
      }
    }
  }

  "kebab-case" - {
    "compound" in {
      assert(Token.Compound("SomeClass", Arguments(Nil)).kebabCase === Token.Compound("some-class", Arguments(Nil)))
    }

    "method" in {
      assert(Token.Method(
        Token.Compound("SomeClass", Arguments(Nil)), "someMethod", Arguments(Nil)
      ).kebabCase === Token.Method(
        Token.Compound("some-class", Arguments(Nil)), "some-method", Arguments(Nil)
      ))
    }
  }

  "camel-case" - {
    "compound" in {
      assert(Token.Compound(
        "some-class", Arguments(Nil)
      ).camelCase === Token.Compound(
        "SomeClass", Arguments(Nil))
      )
    }

    "method" in {
      assert(Token.Method(
        Token.Compound("SomeClass", Arguments(Nil)), "some-method", Arguments(Nil)
      ).camelCase === Token.Method(
        Token.Compound("SomeClass", Arguments(Nil)), "someMethod", Arguments(Nil)
      ))
    }
  }

  private val abc: Identifier = Identifier("abc")
  private val ghi: Identifier = Identifier("ghi")

  private def list(tokens: Token*): Token = Compound("List", Arguments(tokens.toList))
  private def set(tokens: Token*): Token = Compound("Set", Arguments(tokens.toList))

  private val noMargin = Formatter.Indented(margin = 0, escape = false)
}