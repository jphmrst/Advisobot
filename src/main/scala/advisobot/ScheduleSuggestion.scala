
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

  def hasPrerequisite(preqCand: ScheduleSuggestion): Boolean =
    description.hasPrerequisite(preqCand.description)

  def +(append: String): ScheduleSuggestion =
    new ScheduleSuggestion(description + append, units)

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
    * Given a candidate schedule and a class, return the index of the
    * entry where that class occurs, or -1 if it does not occur.
    */
  def inSchedule(course: Course, sched: CandSchedule): Int = {
    var slot = 0

    for((term, suggestions) <- sched) {
      for(suggestion <- suggestions) {
        if (suggestion.description.isCourse(course)) {
          return slot
        }
      }
      slot = slot + 1
    }

    return -1
  }

  /**
   * Standard scoring function for a gap in time between
   * prerequisites.  By default, returns the sum of the number of
   * semesters separating each prerequisite/follow-up pair in the
   * schedule.
   */
  def scorePrereqGap(sched: CandSchedule)(implicit advisees: Advisees, who: Person): Double = {
    var total = 0.0
    var thisSlot = 0

    for((_, suggestions) <- sched) {
      if (thisSlot > 0)
        for(suggestion <- suggestions)
          total = total + scoreGapsIsPrereqsFor(sched, thisSlot, suggestion)
      thisSlot = thisSlot + 1
    }

    total
  }

  /**
   * Standard scoring function for a gap in time between
   * prerequisites.  By default, returns the some of the semesters
   * separating each prerequisite/follow-up pair in the schedule.
   */
  def scoreGapsIsPrereqsFor(
    sched: CandSchedule, slot: Int, suggestion: ScheduleSuggestion
  ): Double = {
    var total = 0.0
    var preSlot = 0.0

    for((_, preSuggestions) <- sched if preSlot < slot) {
      for(preSuggestion <- preSuggestions) {
        if (suggestion.hasPrerequisite(preSuggestion)) {
          total = total + slot - preSlot - 1
        }
      }

      preSlot = preSlot + 1
    }

    total
  }

  /**
   * TODO Standard counting function for scoring slight over- and
   * underloads.
   */
  def scoreOverUnder(sched: CandSchedule)(implicit advisees: Advisees, who: Person): Double =
    ???

  /**
   * TODO Standard counting function for scoring hard overloads.
   */
  def scoreHardOverload(sched: CandSchedule)(implicit advisees: Advisees, who: Person): Double =
    ???

  /**
   * TODO Standard counting function for scoring hard underloads
   * (except in last semester).
   */
  def scoreHardUnderload(sched: CandSchedule)(implicit advisees: Advisees, who: Person): Double =
    ???

  /**
   * TODO Standard scoring function for scoring uneven distribution of
   * classes matching a particular predicate.
   */
  def scoreUneven(
    predicate: ScheduleSuggestion => Boolean,
    sched: CandSchedule
  )(implicit advisees: Advisees, who: Person): Double = ???

//  /**
//   * Standard scoring function for
//   */
//  def score(sched: CandSchedule): Double =
//    ???
}
