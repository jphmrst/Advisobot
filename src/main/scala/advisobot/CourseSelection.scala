
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

  def hasPrerequisiteIn(sels : List[ScheduleSuggestion]): Boolean = {
    for (sel <- sels) {
      if (hasPrerequisite(sel.description)) {
        return true
      }
    }
    return false
  }
  def hasPrerequisite(cs : CourseSelection): Boolean
  def isPrerequisiteOf(c : Course): Boolean
  def course: Option[Course]
  def isCourse(c : Course): Boolean = course match {
    case Some(cs) => cs == c
    case None => false
  }
}
object CourseSelection {
  implicit def fromCourse(cl: Course): CourseSelection = new SpecificClass(cl)
  implicit def fromString(desc: String): CourseSelection =
    new DescribedClasses(desc)
}

class SpecificClass(val cl: Course) extends CourseSelection {
  override def toLaTeX(doc:LaTeXdoc): Unit = cl.toLaTeX(doc)
  override def plainText: String = cl.tag()
  override def hasPrerequisite(cs : CourseSelection): Boolean =
    cs.isPrerequisiteOf(cl)
  override def isPrerequisiteOf(later : Course): Boolean = {
    later.prerequisites.exists(_.isCourse(cl))
  }
  override def course: Option[Course] = Some(cl)
  override def toString(): String = "Specific[" + cl.toString() + "]"
}

class DescribedClasses(val desc: String) extends CourseSelection {
  override def toLaTeX(doc:LaTeXdoc): Unit = {
    doc ++= "\\begin{tabular}{@{}c@{}}"
    doc ++= desc
    doc ++= "\\end{tabular}"
  }
  override def plainText: String = desc.replace("\\\\"," ")
  override def hasPrerequisite(cs : CourseSelection): Boolean = false
  override def isPrerequisiteOf(c : Course): Boolean = false
  override def course: Option[Course] = None
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
  override def hasPrerequisite(cs : CourseSelection): Boolean = false
  override def isPrerequisiteOf(c : Course): Boolean = false
  override def course: Option[Course] = None
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
