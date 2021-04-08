package symmetric

import org.scalatest.FreeSpec


class ExtractorSpec extends FreeSpec {
  "isA[A]" - {
    "A" in {
      assert(isADog.unapply(loki) === Some(loki))
    }
    
    "B" in {
      assert(isADog.unapply(felix) === None)
    }

    "B <: A" in {
      assert(isAPet.unapply(loki)  === Some(loki))
      assert(isAPet.unapply(felix) === Some(felix))
      assert(isAPet.unapply(123)   === None)
    }
  }
  
  private val isADog = Extractor.isA[Dog]
  private val isAPet = Extractor.isA[Pet]
  
  private val loki: Dog = Dog("Loki")
  private val felix: Cat = Cat("felix")
}
