package reify

import scala.language.dynamics

import reify.Token.{Arguments, Infix}
import reify.internal.prelude._
import scalaz.annotation.deriving
import symmetric.Extractor

import scala.util.parsing.combinator.RegexParsers


@deriving(Reify)
sealed trait Token extends Dynamic {
  def applyDynamic(separator: String)(rhs: Token): Token = Infix(this, s" $separator ", rhs)

  def method(name: String, args: Token*): Token = Token.Method(this, name, Arguments(args.toList))

  def camelCase: Token
  def kebabCase: Token
  
  def isEmpty: Boolean = this match {
    case Arguments(values) => values.isEmpty
    case other             => false
  }
}

object Token {

  val fromReified: Extractor[Reified, Token] =
    Extractor.from[Reified].apply(Reify.defer(Reify.of[Token]).unapply)

  def primitive(value: String): Token = Primitive(value)

  def compound(name: String, tokens: List[Token]): Token = Compound(TType(name, Nil), Arguments(tokens))

  def infix(lhs: Token, separator: String, rhs: Token): Token = Infix(lhs, separator, rhs)

  @deriving(Reify)
  case class Primitive(value: String) extends Token { // Rename to Literal
    def camelCase: Token = this
    def kebabCase: Token = this
  }

  @deriving(Reify)
  case class TString(value: String) extends Token {
    def camelCase: Token = this
    def kebabCase: Token = this
  }

  @deriving(Reify)
  case class Identifier(value: String) extends Token {
    def camelCase: Token = Identifier(value.camelCase)
    def kebabCase: Token = Identifier(value.kebabCase)
  }
  
  object TType {
    def many(names: String*): List[TType] = names.map(apply(_)).toList
    
    def apply(name: String, args: TType*): TType = new TType(name, args.toList)
  }
  
  @deriving(Reify)
  case class TType(name: String, args: List[TType]) extends Token {
    def modify(
      name: String => String = identity,
      args: List[TType] => List[TType] = identity
    ): TType = copy(
      name = name(this.name),
      args = args(this.args)
    )
    
    def camelCase: TType = TType(name.camelCase, args.map(_.camelCase))
    def kebabCase: TType = TType(name.kebabCase, args.map(_.kebabCase))
    
    def nonEmpty: Boolean = args.nonEmpty
  }

  @deriving(Reify)
  case class Infix(lhs: Token, separator: String, rhs: Token) extends Token {
    def camelCase: Token = Infix(lhs.camelCase, separator, rhs.camelCase)
    def kebabCase: Token = Infix(lhs.kebabCase, separator, rhs.kebabCase)
  }
  
  object Compound {
    def apply(name: String, arguments: Token): Compound = new Compound(TType(name, Nil), arguments)  
  }

  @deriving(Reify)
  case class Compound(ttype: TType, arguments: Token) extends Token {
    def camelCase: Token = Token.Compound(ttype.camelCase, arguments.camelCase)
    def kebabCase: Token = Token.Compound(ttype.kebabCase, arguments.kebabCase)
  }

  @deriving(Reify)
  case class Function(ttype: TType, arguments: Token) extends Token {
    def camelCase: Token = Function(ttype.camelCase, arguments.camelCase)
    def kebabCase: Token = Function(ttype.kebabCase, arguments.kebabCase)

    def compound: Compound = Compound(ttype, arguments)
  }

  @deriving(Reify)
  case class Arguments(values: List[Token]) extends Token {
    def camelCase: Arguments = Arguments(values.map(_.camelCase))
    def kebabCase: Arguments = Arguments(values.map(_.kebabCase))
  }

  @deriving(Reify)
  case class Method(target: Token, name: String, arguments: Token) extends Token {
    def camelCase: Token = Method(target.camelCase, name.camelCase.uncapitalise, arguments.camelCase)
    def kebabCase: Token = Method(target.kebabCase, name.kebabCase, arguments.kebabCase)

    def function: Function = Function(TType(name), arguments)
  }

  def parseToken(value: String): Either[String, Token] = new TokenParser().parseToken(value)
  def parseFunction(value: String): Either[String, Token] = new TokenParser().parseFunction(value)

  private class TokenParser extends RegexParsers {
    def parseToken(value: String): Either[String, Token] = handleResult(parse(token, value))
    def parseFunction(value: String): Either[String, Token] = handleResult(parse(function, value))
    
    private def handleResult[A](result: ParseResult[A]): Either[String, A] = result match {
      case Success(matched, _) => Right(matched)
      case Failure(msg, _)     => Left("FAILURE: " + msg)
      case Error(msg, _)       => Left("ERROR: " + msg)
    }
    
    def token: Parser[Token] = (infix | compound | tstring | bool | num | identifier) ~ rep(((".") ~> function)) ^^ {
      case target ~ invocationChain => invocationChain.foldLeft(target) {
        case (acc, Function(ttype, args)) => Method(acc, ttype.name, args)
      }
    }
    
    private def params: Parser[TType ~ List[Token]] = (ttype ~ tokens).filter {
      case ttype ~ tokens => ttype.nonEmpty || tokens.nonEmpty
    }

    private def function: Parser[Token] = {
      val withArgs = params ^^ {
        case ttype ~ args => Function(ttype, Arguments(args))
      }
      
      withArgs | identifier
    }

    private def infix: Parser[Infix] = primitive ~ (rep(" ") ~> operator <~ rep(" ")) ~ primitive ^^ {
      case lhs ~ op ~ rhs => Infix(lhs, op, rhs)
    }

    private def operator: Parser[String] = """[:=->]+""".r

    private def _new: Parser[String] = """new""".r
    
    private def primitive: Parser[Primitive] = """[a-zA-Z0-9\-]+""".r ^^ { Primitive(_) }
    private def identifier: Parser[Identifier] = """[a-zA-Z0-9\-]+""".r ^^ { Identifier(_) }
    
    private def ttype: Parser[TType] = identifier ~ ("[" ~> repsep(ttype, comma) <~ "]").? ^^ {
      case Token.Identifier(name) ~ optArgs => TType(name, optArgs.getOrElse(Nil))
    }

    private def bool: Parser[Primitive] = "true|false".r ^^ { Primitive(_)}
    private def num: Parser[Primitive] = "[0-9]+".r ^^ { Primitive(_)}

    private def tstring:   Parser[TString] = {
      val doubleQuoted = doubleQuote ~ """[^"]+""".r ~ doubleQuote ^^ { case _ ~ value ~ _ => TString(value) }
      val tripleQuotes = tripleQuote ~ """[^"]+""".r ~ tripleQuote ^^ { case _ ~ value ~ _ => TString(value) }

      doubleQuoted | tripleQuotes
    }
    
    private def tokens: Parser[List[Token]] = ("(" ~> rep1sep(token, comma) <~ ")").?.map(_.getOrElse(Nil))

    private def compound: Parser[Compound] = _new.? ~ params ^? {
      case None    ~ (ttype ~ args) => Compound(ttype,                               Arguments(args))
      case Some(_) ~ (ttype ~ args) => Compound(ttype.modify(name => s"new $name}"), Arguments(args))
    }

    private val List(comma, doubleQuote, tripleQuote) = List(",", "\"", "\"\"\"").map(value => value ^^ (_ => ()))
  }
}
