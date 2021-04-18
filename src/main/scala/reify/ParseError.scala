package reify

sealed trait ParseError {
  override def toString: String = asString

  def asString: String

  def trace: (String, Option[Token], Option[Token], Option[Reified])
}

object ParseError {
  case class TokenParseError(toParse: String, error: String) extends ParseError {
    def trace: (String, Option[Token], Option[Token], Option[Reified]) = (
      toParse, None, None, None
    )

    def asString: String =
      s"""Unalbe to parse string to token
         |  error:
         |    $error
         |
         |  toParse:
         |    $toParse
         |""".stripMargin
  }

  case class ReifiedParseError(toParse: String, token: Token, camelCase: Token) extends ParseError {
    def trace: (String, Option[Token], Option[Token], Option[Reified]) = (
      toParse, Option(token), Option(camelCase), None
    )

    def asString: String =
      s"""Unable to parse token to Reified
         |  camelCase:
         |    $camelCase
         |
         |  token:
         |    $token
         |
         |  toParse:
         |    $toParse
         |""".stripMargin
  }

  case class ReflectReifiedError[A](
    toParse: String, token: Token, camelCase: Token, reified: Reified, reify: Reify[A]
  ) extends ParseError {
    def trace: (String, Option[Token], Option[Token], Option[Reified]) = (
      toParse, Option(token), Option(camelCase), Option(reified)
    )

    def asString: String =
      s"""Unable to reflect Reified to Dependency
         |  reify:
         |    $reify
         |
         |  reified:
         |    $reified
         |
         |  camelCase:
         |    $camelCase
         |
         |  token:
         |    $token
         |
         |  toParse:
         |    $toParse
         |""".stripMargin
  }
}