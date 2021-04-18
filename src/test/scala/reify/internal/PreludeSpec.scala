package reify.internal

import org.scalatest.FreeSpec
import reify.internal.prelude._

class PreludeSpec extends FreeSpec {
  "RStringSyntax" - {
    "indentBy" in {
      assert("""Hello sailor !
        |
        |
        |""".stripMargin.indentBy(">>> ") === """Hello sailor !
        |>>> 
        |>>> 
        |>>> """.stripMargin)
    }
  }
}
