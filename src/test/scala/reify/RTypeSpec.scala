package reify

import org.scalatest.FreeSpec


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
}
