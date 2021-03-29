package reify

import java.io.PrintStream


object RConsole {
  private val formatter = Formatter.Indented(100, escape = false)

  val out: RConsole = RConsole(Console.out, formatter, enabled = true)
  val err: RConsole = RConsole(Console.err, formatter, enabled = true)
}

case class RConsole(printStream: PrintStream, formatter: Formatter, enabled: Boolean) {
  def diagnose[T: Reify, A: Reify, R: Reify](target: T, method: String, arg: A, result: R): Unit = {
    val reified = Reified.RInfix(Reify.reify(target).method(method, arg), " <=> ", Reify.reify(result))

    printStream.println(formatter.format(reified))
  }

  def println[A: Reify](a: => A): Unit = println("", a)

  def println[A: Reify](prefix: => String, a: => A): Unit = if (enabled) {
    printStream.println(s"$prefix${formatter.format(Reify.reify(a))}")
  }

  def printReified(reified: Reified): Unit = if (enabled) {
    printStream.println(formatter.format(reified))
  }
}