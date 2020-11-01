
package advisobot.core
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{Builder,HashSet,HashMap,ListBuffer}
import java.nio.file.{Paths, Files}
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable}
import org.maraist.util.UniqueHashCode
import org.maraist.outlines.{Outline}
import Trace._

trait CourseSelection extends LaTeXRenderable {
  def plainText: String
}
object CourseSelection {
  implicit def fromCourse(cl: Course): CourseSelection = new SpecificClass(cl)
  implicit def fromString(desc: String): CourseSelection =
    new DescribedClasses(desc)
}

class SpecificClass(val cl: Course) extends CourseSelection {
  override def toLaTeX(doc:LaTeXdoc): Unit = cl.toLaTeX(doc)
  override def plainText: String = cl.tag()
}

class DescribedClasses(val desc: String) extends CourseSelection {
  override def toLaTeX(doc:LaTeXdoc): Unit = {
    doc ++= "\\begin{tabular}{@{}c@{}}"
    doc ++= desc
    doc ++= "\\end{tabular}"
  }
  override def plainText: String = desc.replace("\\\\"," ")
}

class PickOneSelection(val selections: Seq[CourseSelection])
extends CourseSelection {
  override def toLaTeX(doc:LaTeXdoc): Unit = {
    doc ++= """\begin{tabular}{@{}c@{}}"""
    var sep = ""
    for(selection <- selections) {
      doc ++= sep
      selection.toLaTeX(doc)
      sep = """\\ -or- \\ """
    }
    doc ++= """\end{tabular}"""
  }
  override def plainText: String = {
    val sb = new StringBuilder()

    var sep = ""
    for(selection <- selections) {
      sb ++= sep
      sb ++= selection.plainText
      sep = " or "
    }

    sb.toString()
  }
}

object PickOne {
  def apply(sel: ScheduleSuggestion,
            sels: ScheduleSuggestion*): ScheduleSuggestion = {
    val csBuilder = Seq.newBuilder[CourseSelection]
    csBuilder += sel.description
    var units = sel.units
    for (s <- sels) {
      csBuilder += s.description
      units = units or s.units
    }
    new ScheduleSuggestion(new PickOneSelection(csBuilder.result()), units)
  }
}
