package symmetric

import org.scalatest.FreeSpec
import reify.Reified
import reify.internal.prelude.<->


class SymmetricSpec extends FreeSpec {
  "fromReify" in {
    val symm: Cat <-> Reified = Symmetric.fromReify[Cat]

    assert(roundTrip(Cat("hello"), symm) === Some(Cat("hello")))
  }
  
  "forSealedTrait" in {
    val symm: Pet <-> Reified = Symmetric.forSealedTrait(
      Symmetric.fromReify[Cat].lhsTo[Pet], 
      Symmetric.fromReify[Dog].lhsTo[Pet]
    )

    assert(roundTrip(Cat("hello"), symm) === Some(Cat("hello")))
    assert(roundTrip(Dog("hello"), symm) === Some(Dog("hello")))
  }
  
  private def roundTrip[A, B](a: A, ab: A <-> B): Option[A] = for {
    there     <- ab.to(a)
    backAgain <- ab.from(there)
  } yield backAgain
}

