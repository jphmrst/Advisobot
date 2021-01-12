
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

  type CandSchedule = SortedMap[Term,List[ScheduleSuggestion]]

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

  /**
    * Transform a function by multiplying its result by a scalar
    * constant.
    */
  def weigh(
    weight: Double,
    fn: CandSchedule => Double
  ): CandSchedule => Double =
    (sched) => weight * fn(sched)

  /**
   * TODO Standard scoring function for a gap in time between
   * prerequisites.
   */
  def scorePrereqGap(sched: CandSchedule): Double =
    ???

  /**
   * TODO Standard counting function for scoring slight over- and
   * underloads.
   */
  def scoreOverUnder(sched: CandSchedule): Double =
    ???

  /**
   * TODO Standard counting function for scoring hard overloads.
   */
  def scoreHardOverload(sched: CandSchedule): Double =
    ???

  /**
   * TODO Standard counting function for scoring hard underloads
   * (except in last semester).
   */
  def scoreHardUnderload(sched: CandSchedule): Double =
    ???

  /**
   * TODO Standard scoring function for scoring uneven distribution of
   * classes matching a particular predicate.
   */
  def scoreUneven(
    predicate: ScheduleSuggestion => Boolean,
    sched: CandSchedule
  ): Double = ???

//  /**
//   * Standard scoring function for
//   */
//  def score(sched: CandSchedule): Double =
//    ???

}

/**
 * Placeholder for unimplemented object ideas.  This object will eventually
 * go away.
 */
object ScheduleDerivationsPlaceholder {
  import ScheduleSuggestion.CandSchedule

  /**
   *  TODO Generate a possible schedule by swapping two classes.
   */
  def generateSwap(
    sched: CandSchedule,
    earlyTerm: Term, earlyItem: Int, laterTerm: Term, laterItem: Int
  ): Option[CandSchedule] = ???

  /**
   *  TODO Generate a possible schedule by moving one class
   *  to an earlier term.
   */
  def generatePull(
    sched: CandSchedule,
    earlyTerm: Term, laterTerm: Term, item: Int
  ): Option[CandSchedule] = ???

  /**
   *  TODO Generate a possible schedule by combining two terms.
   */
  def generateCombine(sched: CandSchedule, earlyTerm: Term, laterTerm: Term):
  Option[CandSchedule] = ???
}
