package uwlcs.advisobot
import scala.collection.Map
import scala.collection.mutable.HashMap
import org.maraist.latex.LaTeXdoc
import advisobot.core.{Program,Requirement,Select,Person,Grade,Term,Course,
                       Achievement,SideCondition,Viewer,CoursePredicate, UnitsRange,
                       CourseSelection, SpecificClass, PickOneSelection}
import advisobot.builder._

// -----------------------------------------------------------------

abstract class Advisees(people:Person*)
extends advisobot.core.Advisees(people:_*) {

  def isUpper(c: Course): Boolean = (c.number >= 300)
  def isUpper(s: advisobot.core.ScheduleSuggestion): Boolean = s match {
    case (ss: ScheduleSuggestion) => ss.isUpper
    case _ => isUpper(s.description)
  }
  def isUpper(cs: CourseSelection): Boolean = cs match {
    case (specific: SpecificClass) => (specific.cl.number >= 300)
    case (descr: DescribedClasses) => descr.isUpper
    case (pick: PickOneSelection) =>
      pick.selections.map(isUpper(_)).fold(true)((x, y) => x && y)
    case _ => false
  }

  def upperUnitsProspective(who: Person): Int = {
    var total = who.otherUnits

    for(course <- who.current; if isUpper(course)) {
      total = total + course.units
    }

    for((course, grade) <- who.completions;
        if grade.pass
        && !(who.current.contains(course))
        && isUpper(course)
      ) {
      total = total + course.units
    }
    total
  }

  override def writeProspectiveUnits(doc: LaTeXdoc, who: Person) = {
    val pastUnits = who.unitsCompleted
    val currentUnits = who.current.foldLeft(0)(_+_.units)
    val totalUnits = who.unitsProspective
    if (totalUnits > 0) {
      doc ++= "  \\\\ \\multicolumn{2}{|l|}{Units}\n"
      doc ++= "  \\\\ \\multicolumn{2}{|c|}{\\large \\textbf{Previously --- "
      doc ++= pastUnits.toString()
      doc ++= "}}\n"
      doc ++= "  \\\\ \\multicolumn{2}{|c|}{\\large \\textbf{Currently --- "
      doc ++= currentUnits.toString()
      doc ++= "}}\n"
      doc ++= "  \\\\ \\multicolumn{2}{|c|}{\\large \\textbf{Total --- "
      doc ++= totalUnits.toString()
      doc ++= "}}\n"
      val upper = upperUnitsProspective(who)
      if (upper > 0) {
        doc ++= "  \\\\ \\multicolumn{2}{|c|}{\\large \\textbf{Total upper --- "
        doc ++= upper.toString()
        doc ++= "}}\n"
      }
    } else {
      doc ++= """  \\ \multicolumn{2}{|l|}{Units earned}
      \\ \multicolumn{2}{|c|}{\writegap}
"""
    }
  }

  override def writePostplanUnits(doc: LaTeXdoc, who: Person) = {
    var postPlanUnits: UnitsRange = new UnitsRange(who.unitsProspective)
    var postPlanUpperUnits: UnitsRange = upperUnitsProspective(who)
    var nowOrForward = who.plannedAfter(this)

    for ((semester, plan) <- nowOrForward) {
      for (item <- plan) {
        postPlanUnits = postPlanUnits and item.units
        if (isUpper(item)) {
          postPlanUpperUnits = postPlanUpperUnits and item.units
        }
      }
    }

    doc ++= "Total units after planned period: "
    postPlanUnits.toLaTeX(doc)
    doc ++= "\n"

    doc ++= "\\\\Total upper-level units after planned period: "
    postPlanUpperUnits.toLaTeX(doc)
    doc ++= "\n"
  }
}

class DescribedClasses(desc: String, val isUpper: Boolean = false)
extends advisobot.core.DescribedClasses(desc)

class ScheduleSuggestion(description: CourseSelection, units: UnitsRange,
                         val isUpper: Boolean = false)
extends advisobot.core.ScheduleSuggestion(description, units)

