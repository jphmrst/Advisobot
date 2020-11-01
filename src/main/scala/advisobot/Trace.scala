
package advisobot.core
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{Builder,HashSet,HashMap,ListBuffer}
import java.nio.file.{Paths, Files}
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable}
import org.maraist.util.UniqueHashCode
import org.maraist.outlines.{Outline}

// =================================================================

object Trace {
  private var trace=false
  private var indent=0
  val indenter = ". "
  def trace(format: String, args: Any*): Unit = {
    if (trace) {
      printf((indenter*indent)+format, args: _*)
      println()
    }
  }
  def traceBegin(name: String): Unit = {
    if (trace) {
      println((indenter*indent)+"["+name+"]")
      indent = indent+1
    }
  }
  def traceBegin(name: String, format: String, args: Any*): Unit = {
    if (trace) {
      printf((indenter*indent)+"["+name+" "+format+"]", args: _*)
      println()
      indent = indent+1
    }
  }
  def traceEnd(name: String): Unit = {
    if (trace) {
      indent = indent-1
      println((indenter*indent)+"[/"+name+"]")
    }
  }
  def traceEnd(name: String, format: String, args: Any*): Unit = {
    if (trace) {
      indent = indent-1
      printf((indenter*indent)+"[/"+name+" "+format+"]", args: _*)
      println()
    }
  }
}
