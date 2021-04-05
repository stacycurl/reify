package reify.internal.util

import reify.{Reified, Token}


case class Trace(reified: Reified, token: Token, kebabCase: Token, formatted: String) {
  override def toString: String =
    s"""Trace:
       |  reified: $reified
       |  
       |  token: $token
       |  
       |  kebabCase: $kebabCase
       |  
       |  formatted: $formatted
       |""".stripMargin
}
