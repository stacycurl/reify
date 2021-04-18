package reify

import org.scalatest.FreeSpec
import reify.Reified.RCaseClass


class RTypeSpec extends FreeSpec {
  "typeName" - {
    "no type parameters" in {
      assert(RType("Foo").typeName === "Foo")
    }
    
    "one type parameter" in {
      assert(RType("Option", RType("Foo")).typeName === "Option[Foo]")
    }
    
    "two type parameters" in {
      assert(RType("Either", RType("Foo"), RType("Oof")).typeName === "Either[Foo, Oof]")
    }
    
    "nested type parameters" in {
      assert(RType("Option", RType("Either", RType("Foo"), RType("Oof"))).typeName === "Option[Either[Foo, Oof]]")
    }
  }
  
  "reify" - {
    "no type parameters" in {
      assert(Reify.reify(RType("Foo")) === RCaseClass(RType("RType"), "Foo"))
    }

    "one type parameter" in {
      assert(Reify.reify(RType("Option", RType("Foo"))) === RCaseClass(RType("RType"), "Option", RType("Foo")))
    }

    "two type parameters" in {
      assert(Reify.reify(RType("Either", RType("Foo"), RType("Oof"))) === RCaseClass(RType("RType"), "Either", RType("Foo"), RType("Oof")))
    }

    "nested type parameters" in {
      assert(Reify.reify(RType("Option", RType("Either", RType("Foo"), RType("Oof")))) === RCaseClass("RType", "Option", RType("Either", RType("Foo"), RType("Oof"))))
    }
  }
}
