package reify.internal.util

import reify.{Reified, Reify, Token}
import scalaz.annotation.deriving


@deriving(Reify)
case class Trace[A: Reify](value: A, reified: Reified, token: Token, kebabCase: Token, formatted: String) {
  override def toString: String =
    s"""Trace:
       |  value: $value
       |
       |  reified: $reified
       |  
       |  token: $token
       |  
       |  kebabCase: $kebabCase
       |  
       |  formatted: $formatted
       |""".stripMargin
}
