package reify

import scala.language.dynamics

import reify.Token.Infix
import reify.internal.prelude._
import reify.internal.util.Extractor
import scalaz.deriving

import scala.util.parsing.combinator.RegexParsers

@deriving(Reify)
sealed trait Token extends Dynamic {
  def applyDynamic(separator: String)(rhs: Token): Token = Infix(this, s" $separator ", rhs)

  def method(name: String, args: Token*): Token = Token.Method(this, name, args.toList)

  def camelCase: Token
  def kebabCase: Token
}

object Token {

  val fromReified: Extractor[Reified, Token] =
    Extractor.from[Reified].apply(Reify.defer(Reify.of[Token]).unapply)

  def primitive(value: String): Token = Primitive(value)

  def compound(name: String, tokens: List[Token]): Token = Compound(name, tokens)

  def infix(lhs: Token, separator: String, rhs: Token): Token = Infix(lhs, separator, rhs)

  @deriving(Reify)
  case class Primitive(value: String) extends Token { // Rename to Literal
    def camelCase: Token = this
    def kebabCase: Token = this
  }

  @deriving(Reify)
  case class Identifier(value: String) extends Token {
    def camelCase: Token = Identifier(value.camelCase)
    def kebabCase: Token = Identifier(value.kebabCase)
  }

  @deriving(Reify)
  case class TString(value: String) extends Token {
    def camelCase: Token = this
    def kebabCase: Token = this
  }

  @deriving(Reify)
  case class Infix(lhs: Token, separator: String, rhs: Token) extends Token {
    def camelCase: Token = Infix(lhs.camelCase, separator, rhs.camelCase)
    def kebabCase: Token = Infix(lhs.kebabCase, separator, rhs.kebabCase)
  }

  @deriving(Reify)
  case class Compound(name: String, tokens: List[Token]) extends Token {
    def camelCase: Token = Token.Compound(name.camelCase, tokens.map(_.camelCase))
    def kebabCase: Token = Token.Compound(name.kebabCase, tokens.map(_.kebabCase))
  }

  @deriving(Reify)
  case class Arguments(values: List[Token]) extends Token {
    def isEmpty: Boolean = values.isEmpty

    def camelCase: Token = Arguments(values.map(_.camelCase))
    def kebabCase: Token = Arguments(values.map(_.kebabCase))
  }

  @deriving(Reify)
  case class Method(target: Token, name: String, parameters: List[Token]) extends Token {
    def camelCase: Token = {
      val camelCaseName = name.camelCase
      val uncapitalisedCamelCaseName = camelCaseName.charAt(0).toLower + camelCaseName.substring(1)

      Method(target.camelCase, uncapitalisedCamelCaseName, parameters.map(_.camelCase))
    }

    def kebabCase: Token = Method(target.kebabCase, name.kebabCase, parameters.map(_.kebabCase))
  }

  def parse(value: String): Either[String, Token] = new TokenParser().parse(value)

  private class TokenParser extends RegexParsers {
    def parse(value: String): Either[String, Token] = parse(token, value) match {
      case Success(matched, _) => Right(matched)
      case Failure(msg, _)     => Left("FAILURE: " + msg)
      case Error(msg, _)       => Left("ERROR: " + msg)
    }

    def token: Parser[Token] = (infix | compound | tstring | bool | num | identifier) ~ rep(((".") ~> suffix)) ^^ {
      case target ~ invocationChain => invocationChain.foldLeft(target) {
        case (acc, (name, args)) => Method(acc, name, args)
      }
    }

    private def suffix: Parser[(String, List[Token])] = primitive ~ ("(" ~> repsep(token, comma) <~ ")") ^^ {
      case Primitive(method) ~ args => (method, args)
    }

    private def infix: Parser[Infix] = primitive ~ (rep(" ") ~> operator <~ rep(" ")) ~ primitive ^^ {
      case lhs ~ op ~ rhs => Infix(lhs, op, rhs)
    }

    private def operator: Parser[String] = """[:=->]+""".r

    private def primitive: Parser[Primitive] = """[a-zA-Z0-9\-]+""".r ^^ { Primitive(_) }
    private def identifier: Parser[Identifier] = """[a-zA-Z0-9\-]+""".r ^^ { Identifier(_) }

    private def bool: Parser[Primitive] = "true|false".r ^^ { Primitive(_)}
    private def num: Parser[Primitive] = "[0-9]+".r ^^ { Primitive(_)}

    private def tstring:   Parser[TString] = {
      val doubleQuoted = doubleQuote ~ """[^"]+""".r ~ doubleQuote ^^ { case _ ~ value ~ _ => TString(value) }
      val tripleQuotes = tripleQuote ~ """[^"]+""".r ~ tripleQuote ^^ { case _ ~ value ~ _ => TString(value) }

      doubleQuoted | tripleQuotes
    }

    private def compound: Parser[Compound] = {
      identifier ~ ("(" ~> repsep(token, comma) <~ ")") ^^ {
        case Token.Identifier(name) ~ args => Compound(name, args)
      }
    }

    private val comma: Parser[Unit] = ignored(",")
    private val doubleQuote: Parser[Unit] = ignored("\"")
    private val tripleQuote: Parser[Unit] = ignored("\"\"\"")

    private def ignored(value: String): Parser[Unit] = value ^^ (_ => Unit)
  }
}
