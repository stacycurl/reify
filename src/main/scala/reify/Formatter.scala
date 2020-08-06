package reify

import reify.internal.prelude._

sealed trait Formatter {
  final def format(reified: Reified): String = format(reified.tokenize)

  final def format(token: Token): String = token match {
    case identifier: Token.Identifier => formatIdentifier(identifier)
    case primitive: Token.Primitive => formatPrimitive(primitive)
    case string: Token.TString => formatString(string)
    case infix: Token.Infix => formatInfix(infix)
    case method: Token.Method => formatMethod(method)
    case compound: Token.Compound => formatCompound(compound)
    case arguments: Token.Arguments => formatArguments(arguments)
  }

  def formatIdentifier(identifier: Token.Identifier): String

  def formatPrimitive(primitive: Token.Primitive): String

  def formatString(string: Token.TString): String = if (string.value.contains('\n')) {
    // TODO: Generalize margin character to cope with '|' being present in the string already.
    s"${string.value.indent("  |").quote.quote.quote}.stripMargin"
  } else {
//    val result = string.value.transformIf(escape)(_.escape).quote

    if (string.value.contains("\"")) {
      string.value.quote.quote.quote
    } else {
      string.value.quote
    }
  }

  def formatInfix(infix: Token.Infix): String

  def formatMethod(method: Token.Method): String

  def formatCompound(compound: Token.Compound): String

  def formatArguments(arguments: Token.Arguments): String

  protected def escape: Boolean
}

object Formatter {

  case class Indented(margin: Int, escape: Boolean) extends Formatter {
    private val indent: String = "  "
    private val unindented = new Unindented(escape)

    def formatIdentifier(identifier: Token.Identifier): String = identifier.value

    def formatPrimitive(primitive: Token.Primitive): String = if (escape) primitive.value.escape else primitive.value

    def formatInfix(infix: Token.Infix): String = List(
      format(infix.lhs),
      infix.separator,
      format(infix.rhs)
    ).mkString("")

    // TODO: This isn't indented
    def formatMethod(method: Token.Method): String = {
      val parameters: String = if (method.parameters.isEmpty) "" else {
        method.parameters.map(format).mkString("(", ", ", ")")
      }

      val name = if (method.name.isEmpty) "" else s".${method.name}"

      (format(method.target) +: s"${name}" +: parameters +: Nil).mkString("")
    }

    def formatCompound(compound: Token.Compound): String = {
      if (compound.tokens.isEmpty) s"${compound.name}()" else {
        val unindented = this.unindented.formatCompound(compound)
        val ul = unindented.length

        if (ul <= margin) unindented else {
          val formattedArguments =
            formatArguments(compound)

          s"${compound.name}(${beforeFirstItem}$formattedArguments${afterLastItem})"
        }
      }
    }

    private def formatArguments(compound: Token.Compound): String = {
      val unindented = applyIndent(this.unindented.formatArguments(compound.tokens))
      val ul = unindented.length

      if (ul <= margin) unindented else {
        compound.tokens.map(token => applyIndent(format(token)))
          .mkString(itemSeparator)
      }
    }

    def formatArguments(arguments: Token.Arguments): String = {
      val unindented = this.unindented.formatArguments(arguments)
      val ul = unindented.length

      if (ul <= margin) unindented else {
        arguments.values.map(token => applyIndent(format(token)))
          .mkString(itemSeparator)
      }
    }

    def applyIndent(value: String): String =
      value.split("\n").map(item => s"$indent$item").mkString("\n")

    private val itemSeparator: String = s",\n"

    private val afterLastItem: String = "\n"

    private def beforeFirstItem: String = afterLastItem
  }

  object Unindented extends Unindented(escape = false)

  class Unindented(protected val escape: Boolean) extends Formatter {
    def formatIdentifier(identifier: Token.Identifier): String = identifier.value

    def formatPrimitive(primitive: Token.Primitive): String = if (escape) primitive.value.escape else primitive.value

    def formatInfix(infix: Token.Infix): String = List(
      format(infix.lhs),
      infix.separator,
      format(infix.rhs)
    ).mkString("")

    def formatMethod(method: Token.Method): String = {
      // TODO: Some empty methods may require parenthesis
      val parameters: String = if (method.parameters.isEmpty) "" else {
        method.parameters.map(format).mkString("(", ", ", ")")
      }

      val name = if (method.name.isEmpty) "" else s".${method.name}"

      (format(method.target) +: s"${name}" +: parameters +: Nil).mkString("")
    }

    def formatCompound(compound: Token.Compound): String =
      s"${compound.name}(${formatArguments(compound.tokens)})"

    def formatArguments(arguments: Token.Arguments): String =
      formatArguments(arguments.values)

    def formatArguments(tokens: List[Token]): String = {
      tokens
        .map(token => format(token))
        .mkString(", ")
    }
  }
}