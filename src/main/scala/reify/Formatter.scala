package reify

import reify.Reified.RList
import reify.Token.{Arguments, TType}
import reify.internal.prelude._

import scala.collection.immutable.{List, Nil}

sealed trait Formatter {
  final def shorter(first: List[Reified], second: List[Reified]): List[Reified] =
    if (lengthOf(RList(first)) < lengthOf(RList(second))) first else second

  final def lengthOf(reified: Reified): Int = format(reified).length

  final def format(reified: Reified): String = format(reified.tokenize)

  def format(token: Token): String
}

object Formatter {
  case class Lines(values: List[(Int, String)]) {
    def value: String = toList.mkString("\n")

    def suffixWith(suffix: String): Lines = Lines {
      values.reverse.zipWithIndex.map {
        case ((indent, line), 0)     => (indent, s"$line$suffix")
        case ((indent, line), index) => (indent, line)
      }.reverse
    }

    def +(rhs: Lines): Lines = {
      def join(first: (Int, String), second: (Int, String)): List[(Int, String)] = {
        List((first._1, first._2 + toLine(second)))
      }

      Lines((values, rhs.values) match {
        case (init :+ last, head +: tail) => init ++ join(last, head) ++ tail
        case _                            => values ++ rhs.values
      })
    }

    def indent: Lines = Lines(values.map {
      case (i, l) => (i + 1, l)
    })

    def toList: List[String] = values.map(toLine)

    private def toLine(t: (Int, String)): String = ("  " * t._1) + t._2
  }  
  
  object Lines {
    def apply(values: String*): Lines = 
      Lines(values.map(l => (0, l)).toList)
          
    val empty: Lines = Lines(Nil)
    
    implicit class ListOfLinesSyntax(private val self: List[Lines]) {
      def lmkString(sep: String): Lines = self match {
        case Nil          => Lines.empty 
        case one :: Nil   => one
        case head :: tail => tail.foldLeft(head) {
          case (acc, next) => acc.suffixWith(sep) + next
        } 
      }
    }
  }
  
  case class WithinMargin(margin: Int) extends Formatter {
    def format(token: Token): String = best(tokenFormats(token)).value
    
    def best(results: List[Lines]): Lines = {
      val withSizes: List[(Lines, (Int, Int))] = results.map(result => {
        val lines = result.toList
        
        val width = lines match {
          case Nil   => 0
          case other => other.map(_.length).max 
        }
        
        val height = lines.length
        
        result -> (width, height)
      }).sortBy {
        case (_, (width, height)) => (height, -width)
      }
  
      val widestUnderMargin: Option[(Lines, (Int, Int))] = {
        withSizes.find {
          case (result, (width, height)) => width <= margin
        }
      }
  
      val shortestOverMargin: (Lines, (Int, Int)) = {
        withSizes.minBy {
          case (_, (width, height)) => (width - margin, height)
        }
      }
      
      widestUnderMargin.getOrElse(shortestOverMargin)._1
    }
    
    
    def tokenFormats(tok: Token): List[Lines] = {
      import Token._
      
      def indent(vs: List[Lines]): List[Lines] =
        vs.map(_.indent)
      
      val result = tok match {
        case it: TType => List(Lines(formatTType(it)))
        case Primitive(value) => List(Lines(value))
        case Identifier(value) => List(Lines(value))
        case TString(value) => List(Lines(formatString(value)))
          
        case Compound(ttype, arguments) => for {
          a <- tokenFormats(arguments)
        } yield {
          Lines(s"${formatTType(ttype)}(") + a + Lines(")")
        }
  
        case function: Function => tokenFormats(function.compound)
  
        case Infix(lhs, sep, rhs) => for {
          l <- tokenFormats(lhs)
          r <- tokenFormats(rhs)
        } yield {
          l + Lines(sep) + r
        }
          
        case Arguments(values) => for {
          vs <- values.map(tokenFormats).transpose
          
          a = vs.lmkString(", ")
          b = Lines((0, "") +: a.indent.values :+ (0, ""))
          
          c = {
            val joined = Lines(vs.reverse.zipWithIndex.reverse.flatMap {
              case (lines, 0) => lines.values
              case (lines, _) => lines.suffixWith(",").values
            })

            Lines((0, "") +: joined.indent.values :+ (0, ""))
          }
          
          f  <- List(a, b, c)
        } yield f

        case Method(target, name, arguments) => for {
          t <- tokenFormats(target)
          a <- tokenFormats(arguments)
        } yield {
          val sep = if (name.isEmpty) "" else "."

          t + Lines(s"$sep$name(") + a + Lines(")")
        }
      }
      
      result
    }
    
    def formatString(string: String): String = if (string.contains('\n')) {
      // TODO: Generalize margin character to cope with '|' being present in the string already.
      s"${string.indentBy("  |").quote.quote.quote}.stripMargin"
    } else {
  //    val result = string.value.transformIf(escape)(_.escape).quote
  
      if (string.contains("\"")) {
        string.quote.quote.quote
      } else {
        string.quote
      }
    }
    
  }
  
  trait Helper { self: Formatter => 
    final def format(token: Token): String = token match {
      case it: Token.Identifier => formatIdentifier(it)
      case it: Token.Primitive  => formatPrimitive(it)
      case it: Token.TString    => formatString(it)
      case it: Token.Infix      => formatInfix(it)
      case it: Token.Method     => formatMethod(it)
      case it: Token.Function   => formatFunction(it)
      case it: Token.Compound   => formatCompound(it)
      case it: Token.Arguments  => formatArguments(it)
      case it: Token.TType      => formatTType(it)        
    }
  
    def formatIdentifier(identifier: Token.Identifier): String
  
    def formatPrimitive(primitive: Token.Primitive): String
  
    def formatString(string: Token.TString): String = if (string.value.contains('\n')) {
      // TODO: Generalize margin character to cope with '|' being present in the string already.
      s"${string.value.indentBy("  |").quote.quote.quote}.stripMargin"
    } else {
  //    val result = string.value.transformIf(escape)(_.escape).quote
  
      if (string.value.contains("\"")) {
        string.value.quote.quote.quote
      } else {
        string.value.quote
      }
    }
  
    final def formatInfix(infix: Token.Infix): String = List(
      format(infix.lhs), 
      infix.separator, 
      format(infix.rhs)
    ).mkString("")
  
    def formatMethod(method: Token.Method): String = {
      val sep = if (method.name.isEmpty) "" else "."
  
      s"${format(method.target)}$sep${formatFunction(method.function)}" 
    }
  
    def formatFunction(function: Token.Function): String
  
    def formatCompound(compound: Token.Compound): String
  
    def formatArguments(arguments: Token.Arguments): String
  
    protected def escape: Boolean    
  }
  
  case class Indented(margin: Int, escape: Boolean) extends Formatter with Helper {
    private val indent: String = "  "
    private val unindented = new Unindented(escape)

    def formatIdentifier(identifier: Token.Identifier): String = identifier.value

    def formatPrimitive(primitive: Token.Primitive): String = if (escape) primitive.value.escape else primitive.value

    // TODO: This isn't indented
    def formatFunction(function: Token.Function): String = {
      val parameters: String = if (function.arguments.isEmpty) "" else {
        function.arguments match {
          case Arguments(values) => values.map(format).mkString("(", ", ", ")")
          case other             => format(other)
        }
      }

      (s"${function.name}" +: parameters +: Nil).mkString("")
    }

    def formatCompound(compound: Token.Compound): String = {
      if (compound.arguments.isEmpty) s"${formatTType(compound.ttype)}()" else {
        val unindented = this.unindented.formatCompound(compound)
        val ul = unindented.length

        if (ul <= margin) unindented else {
          val formattedArguments = formatArguments(compound)

          s"${formatTType(compound.ttype)}(${beforeFirstItem}$formattedArguments${afterLastItem})"
        }
      }
    }
    
    private def formatArguments(compound: Token.Compound): String = {
      val unindented = applyIndent(this.unindented.formatArguments(compound.arguments))
      
      if (unindented.length <= margin) unindented else compound.arguments match {
        case Arguments(values) => formatTokens(values)
        case other             => format(other)
      } 
    }

    def formatArguments(arguments: Token.Arguments): String = {
      val unindented = this.unindented.formatArguments(arguments)

      if (unindented.length <= margin) unindented else formatTokens(arguments.values)
    }

    def applyIndent(value: String): String =
      value.split("\n").map(item => s"$indent$item").mkString("\n")

    private def formatTokens(tokens: List[Token]): String =
      tokens.map(token => applyIndent(format(token))).mkString(itemSeparator)

    private val itemSeparator: String = s",\n"

    private val afterLastItem: String = "\n"

    private def beforeFirstItem: String = afterLastItem
  }

  object Unindented extends Unindented(escape = false)

  class Unindented(protected val escape: Boolean) extends Formatter with Helper {
    def formatIdentifier(identifier: Token.Identifier): String = identifier.value

    def formatPrimitive(primitive: Token.Primitive): String = if (escape) primitive.value.escape else primitive.value

    def formatFunction(function: Token.Function): String = {
      // TODO: Some empty methods may require parenthesis
      val parameters: String = if (function.arguments.isEmpty) "" else {
        function.arguments match {
          case Arguments(values) => values.map(format).mkString("(", ", ", ")")
          case other             => format(other)
        }
      }

      (s"${function.name}" +: parameters +: Nil).mkString("")
    }

    def formatCompound(compound: Token.Compound): String =
      s"${formatTType(compound.ttype)}(${formatArguments(compound.arguments)})"
    
    def formatArguments(token: Token): String = token match {
      case Arguments(values) => formatArguments(values)
      case other             => format(token)
    }
    
    def formatArguments(arguments: Token.Arguments): String =
      formatArguments(arguments.values)

    def formatArguments(tokens: List[Token]): String = {
      tokens
        .map(token => format(token))
        .mkString(", ")
    }
  }

  private def formatTType(ttype: TType): String = ttype match {
    case TType(name, Nil)  => name
    case TType(name, args) => s"$name[${args.map(formatTType).mkString(", ")}]"
  }
}