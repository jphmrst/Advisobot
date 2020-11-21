
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

class ScheduleSuggestion(val description: CourseSelection,
                         val units: UnitsRange)
extends LaTeXRenderable {
  override def toLaTeX(doc: LaTeXdoc): Unit = {
    val selectionFormatter = SelectionFormatter.currentFormatter
    val colorName = selectionFormatter.colorName
    val formatter = selectionFormatter.formatter
    val alerting = ScheduleSuggestion.resetAlert

    doc ++= """      \\ \hline\multicolumn{1}{|c|}{"""
    if (alerting) { doc ++= "\\cellcolor{red!25}" }
    doc ++= """\textcolor{"""
    doc ++= colorName
    doc ++= "}{"
    doc ++= formatter
    units.toLaTeX(doc)
    doc ++= """}} & \multicolumn{1}{c|}{"""
    if (alerting) { doc ++= "\\cellcolor{red!25}" }
    doc ++= """\textcolor{"""
    doc ++= colorName
    doc ++= "}{"
    doc ++= formatter
    description.toLaTeX(doc)
    doc ++= "}}\n"
  }
  override def toString(): String =
    "Suggestion<" + units.toString() + ": " + description.toString()
}

object ScheduleSuggestion {
  implicit def fromCourse(cl: Course): ScheduleSuggestion =
    new ScheduleSuggestion(new SpecificClass(cl), cl.units)
  def toTake(description: CourseSelection, units: UnitsRange) =
    new ScheduleSuggestion(description, units)

  private var alertNext: Boolean = false
  def setAlertNext: Unit = { alertNext = true }
  def resetAlert: Boolean = {
    val result = alertNext
    alertNext = false
    result
  }
}
