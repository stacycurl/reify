package reify

import java.io.PrintStream


object RConsole {
  private val formatter = Formatter.Indented(100, escape = false)

  val out: RPrintStream = RPrintStream(Console.out, formatter)
  val err: RPrintStream = RPrintStream(Console.err, formatter)
}

case class RPrintStream(printStream: PrintStream, formatter: Formatter) {
  def diagnose[T: Reify, A: Reify, R: Reify](target: T, method: String, arg: A, result: R): Unit = {
    val reified = Reified.RInfix(
      Reify.reify(target).method(method, arg),
      " <=> ",
      Reify.reify(result)
    )

    printStream.println(formatter.format(reified))
  }

  def println[A: Reify](a: A): Unit = println("", a)
  def println[A: Reify](prefix: String, a: A): Unit = printStream.println(s"$prefix${formatter.format(Reify.reify(a))}")
}