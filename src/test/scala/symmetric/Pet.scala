package symmetric

import reify.Reify
import scalaz.annotation.deriving


sealed trait Pet

@deriving(Reify)
case class Cat(name: String) extends Pet

@deriving(Reify)
case class Dog(name: String) extends Pet
