package reify.internal

import java.io.{File, FileInputStream, InputStream}
import java.util.zip.{ZipEntry, ZipFile}
import java.util.{Properties => JProperties}
import reify.{Reified, Reify}
import symmetric.Extractor.ExtractorSyntax
import symmetric.{Extractor, Symmetric}

import scala.util.matching.Regex
import scala.util.{Properties, Try}


object prelude {
  type Endo[A] = A => A
  type <->[A, B] = Symmetric[A, B]
  
  import reify.Reify.ReifyStringContext
  
  implicit class ExtractorReifySyntax[A, B](private val self: Extractor[A, B]) extends AnyVal {
    def logR(name: String)(implicit A: Reify[A], B: Reify[B]): Extractor[A, B] = new ExtractorSyntax(self).tap(
      around = a => optB => println(s"$name: " + reify"$a => $optB\n")
    )
  }
  
  implicit class RegexMatcher(private val self: StringContext) extends AnyVal {
    def r: Regex = self.parts.mkString("(.+)")
      .replaceAllLiterally("?", "\\?")
      .r
  }
  
  implicit class RAnySyntax[A](private val self: A) extends AnyVal {
    def tap[Discarded](actions: (A ⇒ Discarded)*): A = { actions.foreach(action ⇒ action(self)); self }

    def indentBy(by: String): String = self.toString.indentBy(by)

    def >>[B](b: B): B = b

    def |>[B](f: A => B): B = f(self)
  }

  implicit class RStringSyntax(private val self: String) extends AnyVal {
    def quote: String = quoteWith('"')
    def quoteWith(char: Char): String = s"$char$self$char"

    def indentBy(by: String): String =
      self.split("\n", -1).mkString(s"\n$by")

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
    def uncapitalise: String = self.charAt(0).toLower + self.substring(1)
  }

  implicit class RListSyntax[A](private val self: List[A]) extends AnyVal {
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

  implicit class RListReifiedSyntax(private val self: List[Reified]) extends AnyVal {
    def ^:[A: Reify](lhs: A): List[Reified] = Reify.reify(lhs) :: self

    // This doesn't actually use self, it's only here because otherwise the method above is found first.
    def ^:[A](implicit A: Reify[A]): Extractor[List[Reified], A] = Extractor.from[List[Reified]].collect {
      case A(a) :: Nil => a
    }
  }
  
  def time[A](name: String)(f: => A): A = {
    val (value, elapsed) = timeOf(name)(f)
    
    println(s"$name: $elapsed")
    
    value
  }
  
  implicit class RPropertiesSyntax(private val self: JProperties) {
    def loadMatching(relativeFileName: String): JProperties = self.tap(props => for {
      stream <- classPathInputStreams(relativeFileName)
      _      <- Try(props.load(stream)).toOption
    } yield stream.close())
  }
  
  def timeOf[A](name: String)(f: => A): (A, Long) = {
    val start = System.currentTimeMillis()
    val value = f
    val end   = System.currentTimeMillis()
    
    (value, end - start)
  }

  private def classPathInputStreams(fileName: String): Array[InputStream] = 
    Properties.propOrElse("java.class.path", ".").split(File.pathSeparator).map(new File(_)).flatMap(inputStream(_, fileName))
  
  private def inputStream(parent: File, relativeFileName: String): Option[InputStream] = Try(if (parent.isDirectory) {
    new FileInputStream(new File(parent, relativeFileName))
  } else {
    new ZipFile(parent).getInputStream(new ZipEntry(relativeFileName))
  }).toOption
}
