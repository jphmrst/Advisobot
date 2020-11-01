
package advisobot.core
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{Builder,HashSet,HashMap,ListBuffer}
import java.nio.file.{Paths, Files}
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable}
import org.maraist.util.UniqueHashCode
import org.maraist.outlines.{Outline}
import Trace._

// =================================================================

trait SelectionFormatter {
  val formatter: String
  val colorName: String
}

object SelectionFormatter {
  val plainFormatter = new SelectionFormatter() {
    override val formatter: String = ""
    override val colorName: String = "black"
  }
  val handwrittenFormatter = new SelectionFormatter() {
    override val formatter: String = """\fontfamily{augie}\selectfont\large """
    override val colorName: String = "blue"
  }
  var currentFormatter: SelectionFormatter = plainFormatter
}
