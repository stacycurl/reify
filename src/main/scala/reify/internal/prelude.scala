package reify.internal

import reify.{Reified, Reify}

object prelude {
  implicit class RAnySyntax[A](val self: A) extends AnyVal {
    def tap[Discarded](actions: (A ⇒ Discarded)*): A = { actions.foreach(action ⇒ action(self)); self }

    def indent(by: String): String = self.toString.indent(by)

    def >>[B](b: B): B = b

    def transformIf(condition: Boolean)(f: A => A): A = if (condition) f(self) else self
  }

  implicit class RStringSyntax(val self: String) extends AnyVal {
    def quote: String = quoteWith('"')
    def quoteWith(char: Char): String = s"$char$self$char"

    def indent(by: String): String =
      self.split("\n").mkString(s"\n$by")

    def escape: String = self.flatMap {
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '"'  => "\\\""
      case '\'' => "\\\'"
      case '\\' => "\\\\"
      case ch if ch.isControl => "\\0" + Integer.toOctalString(ch.toInt)
      case ch => String.valueOf(ch)
    }

    def camelCase: String = "\\-([a-z\\d])".r.replaceAllIn("-" + self, m => m.group(1).toUpperCase()).stripPrefix("-")
    def kebabCase: String = "[A-Z\\d]".r.replaceAllIn(self, m => "-" + m.group(0).toLowerCase()).stripPrefix("-")
  }

  implicit class RListSyntax[A](val self: List[A]) extends AnyVal {
    def flatMapFirst[B](f: A => Option[B]): Option[B] = {
      @scala.annotation.tailrec
      def recurse(current: List[A]): Option[B] = current match {
        case Nil          => None
        case head :: tail => f(head) match {
          case Some(result) => Some(result)
          case None         => recurse(tail)
        }
      }

      recurse(self)
    }
  }

  implicit class RListReifiedSyntax(val self: List[Reified]) extends AnyVal {
    def ^:[A: Reify](lhs: A): List[Reified] = Reify.reify(lhs) :: self
  }
}
