package reify.internal

import scala.language.higherKinds

object ForwardReferenceChecker {
  def apply[TC[_]]: ForwardReferenceChecker[TC] = new ForwardReferenceChecker[TC]
}

class ForwardReferenceChecker[TC[_]] {
  def check[A](implicit A: TC[A]): ForwardReferenceChecker[TC] = {
    if (isNull(A)) {
      Console.err.println("Looks like you have a forward reference")
    }

    this
  }
  
  protected def isNull[A](A: TC[A]): Boolean = {
    A == null
  }

  def calc[A](f: => A): A = f
}





