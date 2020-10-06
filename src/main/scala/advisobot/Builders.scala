
package advisobot.builder
import scala.collection.Map
import scala.collection.mutable.{HashSet,HashMap,ListBuffer}
import advisobot.core._
import org.maraist.latex.LaTeXdoc

// =================================================================

case class ConditionNoteOnly(val desc: String) extends SideCondition {
  override def satisfied(acs: List[Achievement])(implicit who: Person) = true
  override def renderSatisfaction(achievements: List[Achievement]
                                )(implicit doc: LaTeXdoc, who: Person) = {
    doc ++=* desc
  }
}

case class UnitsCondition(
  val desc: String, val units: Int,
  val filterPred: (Course) => Boolean = { (x) => true }
) extends SideCondition {
  override def satisfied(acs: List[Achievement])(implicit who: Person) =
    acs.foldLeft(0)(_+_.units) < units
  override def renderSatisfaction(achievements: List[Achievement]
                                )(implicit doc: LaTeXdoc, who: Person) = {
    doc ++= units.toString()
    doc ++= " units "
    doc ++= desc
    doc ++= " required, \\colorbox{"
    val earned = achievements.filter(_.courseCheck(filterPred)).foldLeft(0)(_+_.units)
    if (earned < units) {
      doc ++= "red!30"
    } else {
      doc ++= "green"
    }
    doc ++= "}{"
    doc ++= earned.toString()
    doc ++= " earned}"
  }
}

// =================================================================

trait TabularViewer extends Viewer {
  override def write(implicit doc: LaTeXdoc, who: Person,
                     satisfiers: Map[Requirement,List[Achievement]]): Unit = {
    writeHeader
    writeLines
    writeFooter
  }

  def title: String
  def columns: Int

  def writeHeader(implicit doc: LaTeXdoc, who: Person,
                  satisfiers: Map[Requirement,List[Achievement]]): Unit = {
    doc ++= "      \\begin{tabular}[t]{|"
    for (i <- 1 to columns) {
      if (i>1) { doc ++= "@{~~}" }
      doc ++= "c@{~}l"
    }
    doc ++= "|}\n        \\multicolumn{"
    doc ++= (2 * columns).toString()
    doc ++= "}{c}{"
    doc ++= title
    doc ++= "}\n"
  }

  def writeLines(implicit doc: LaTeXdoc, who: Person,
                 satisfiers: Map[Requirement,List[Achievement]]): Unit

  def writeFooter(implicit doc: LaTeXdoc, who: Person,
                 satisfiers: Map[Requirement,List[Achievement]]): Unit = {
    doc ++= "      \\\\ \\hline\n      \\end{tabular}\\reqBoxVspace\n\\\\ "
  }
}

class CollectedUnitsViewer(
  override val title: String, override val columns: Int,
  val requiredUnits: Int, val sources: Requirement*
) extends TabularViewer {
  def writeLines(implicit doc: LaTeXdoc, who: Person,
                 satisfiers: Map[Requirement,List[Achievement]]): Unit = {
    var units:Int = 0
    for (source <- sources) {
      for (achievement <- satisfiers.getOrElse(source, List())) {
        achievement match {
          case course:Course => {
            units = units + course.units
          }
          case _ => { }
        }
      }
    }

    doc ++= "\\\\ \\hline \\multicolumn{"
    doc ++= (2*columns).toString()
    doc ++= "}{|c|}{"
    doc ++= requiredUnits.toString()
    doc ++= " units required, \\colorbox{"
    if (units < requiredUnits) {
      doc ++= "red!30"
    } else {
      doc ++= "green"
    }
    doc ++= "}{"
    doc ++= units.toString()
    doc ++= " earned}}\n"
  }
}

class SimpleViewer(override val title: String, override val columns: Int,
                   val requirements: Requirement*)
extends TabularViewer {
  override def writeLines(implicit doc: LaTeXdoc, who: Person,
                          satisfiers: Map[Requirement,List[Achievement]]): Unit = {
    var item: Int = 0

    for (req <- requirements) {
      doc ++= "\n        "
      if ((item % columns) == 0) {
        doc ++= "\\\\ "
      } else {
        doc ++= "& "
      }
      if (item == 0) {
        doc ++= "\\hline "
      }
      req.formatSatisfaction
      item = item + 1
    }

    if ((item % columns) > 0) {
      doc ++= "\n & \\multicolumn{"
      doc ++= (2 * (columns - (item % columns))).toString()
      doc ++= "}{c|}{}\n"
    }
  }
}

class ConditionsViewer(val req: WithConditions, val columns: Int)
extends TabularViewer {
  override def title: String = req.name
  override def writeLines(
    implicit doc: LaTeXdoc, who: Person, ss: Map[Requirement,List[Achievement]]
  ): Unit = {
    var item: Int = 0
    val courses = ss(req.req)

    if (courses.isEmpty) {
      doc ++= "\\\\ \\hline\\multicolumn{"
      doc ++= (2 * columns).toString()
      doc ++= "}{|c|}{None completed}\n"
    } else {
      for (course <- courses) {
        doc ++= "\n        "
        if ((item % columns) == 0) {
          doc ++= "\\\\ "
        } else {
          doc ++= "& "
        }
        if (item == 0) {
          doc ++= "\\hline "
        }
        if (who.isCurrent(course)) {
          doc ++= "\\colorbox{yellow}{\\textsc{now}} & "
        } else {
          doc ++= "\\textcolor{green}{\\checkmark} & "
        }
        course.formatSatisfier(doc)
        item = item + 1
      }

      if ((item % columns) > 0) {
        doc ++= "\n & \\multicolumn{"
        doc ++= (2 * (columns - (item % columns))).toString()
        doc ++= "}{c|}{}\n"
      }
    }

    for (check <- req.checks) {
      doc ++= "\\\\ \\multicolumn{"
      doc ++= (2 * columns).toString()
      doc ++= "}{|l|}{\\hspace*{0.25em}$\\bullet$\\hspace*{0.5em}"
      check.renderSatisfaction(courses)
      doc ++= "}\n"
    }
  }
}

object ConditionsViewer {
  def apply(req: Requirement, columns: Int): TabularViewer = req match {
    case wc: WithConditions => new ConditionsViewer(wc, columns)
    case _ => new SimpleViewer(req.name, 1, req)
  }
}

// =================================================================

object OneOf {
  def apply(courses:Requirement*):Requirement =
    Select(1, courses.toList)
}

object HasSuffix {
  def apply(name: String, suffix: String): CoursePredicate =
    new CoursePredicate(name, _.suffix.fold(false)(_.equals(suffix)))
}

object CourseOfUnits {
  def apply(prefix: String, num: Int): CoursePredicate = apply(prefix, num, 1)
  def apply(prefix: String, num: Int, units: Int): CoursePredicate =
    new CoursePredicate(prefix+num.toString(),
                        (course) => (course.prefix.equals(prefix)
                                     && course.number.equals(num)
                                     && course.units >= units))
}

object AtLevel {
  def apply(desc:String, prefix: String,
            levelPred: (Int) => Boolean): CoursePredicate =
    new CoursePredicate(desc, (course) => (course.prefix.equals(prefix)
                                           && levelPred(course.number)))
}

case class WithConditions(val name: String, val req: Requirement,
                          val checks: List[SideCondition] = List())
extends Requirement {
  override def addSatisfiers(
    implicit who: Person,
    satisfiers: HashMap[Requirement,List[Achievement]],
    checkset: HashSet[Course]
  ): Option[List[Achievement]] = {
    req.addSatisfiers(who, satisfiers, checkset) match {
      case None => None
      case Some(achievements) => {
        if (checks.foldLeft(true)(_ && _.satisfied(achievements))) {
          succeeding(achievements, satisfiers)
        } else {
          None
        }
      }
    }
  }

  override def count: Int = 1
  override def formatUnsatisfied(doc:LaTeXdoc): Unit = { doc ++= name }
}

/**
 * Provides a shortcut for a call to
 * {@link advisobot.builder.WithConditions WithConditions}
 * with requirement formed by a call to
 * {@link advisobot.builder.AllSatisfying AllSatisfying}.
 */
object AllWithConditions {
  /**
   * Shortcut for a call to
   * {@link advisobot.builder.WithConditions WithConditions}
   * with requirement formed by a call to
   * {@link advisobot.builder.AllSatisfying AllSatisfying}.
   *
   * @param desc LaTeX string description
   * @param req The requirement to be satisfied
   * @param cnds {@param advisobot.core.SideCondition SideCondition}s
   * to be satisfied
   */
  def apply(desc: String, req: Requirement,
            cnds: List[SideCondition]): WithConditions = {
    WithConditions(desc, AllSatisfying(desc, req), cnds)
  }
}

case class AllSatisfying(val name: String, val req: Requirement)
extends Requirement {
  override def
      addSatisfiers(implicit who: Person, satisfiers:HashMap[Requirement,List[Achievement]],
                    checkset:HashSet[Course]): Option[List[Achievement]] = {
    val buffer = new ListBuffer[Achievement]
    for (possibleSatisfier <- checkset) {
      val thisSet = new HashSet[Course]
      thisSet += possibleSatisfier
      req.addSatisfiers(who, new HashMap[Requirement,List[Achievement]],
                        thisSet) match {
        case None => { }
        case Some(_) => {
          checkset -= possibleSatisfier
          buffer += possibleSatisfier
        }
      }
    }

    succeeding(buffer.toList, satisfiers)
  }

  override def count: Int = 1
  override def formatUnsatisfied(doc:LaTeXdoc): Unit = { doc ++= name }
}

class VariableUnits(val prefix: String, val number: Int,
                    val suffix: Option[String], val shortName: String,
                    val longName: String, val minUnits: Int, val maxUnits: Int,
                    val prerequisites: List[Requirement]) {
  def this(prefix: String, number: Int, name: String,
           minUnits: Int, maxUnits: Int, preqs: List[Requirement]) =
    this(prefix, number, None, name, name, minUnits, maxUnits, preqs)
  def this(prefix: String, number: Int, name: String,
           minUnits: Int, maxUnits: Int) =
    this(prefix, number, None, name, name, minUnits, maxUnits, List())
  def this(prefix: String, number: Int, shortName: String, longName: String,
           minUnits: Int, maxUnits: Int) =
    this(prefix, number, None, shortName, longName, minUnits, maxUnits, List())

  def apply(units:Int):Course = {
    return new Course(units, prefix, number, suffix,
                      shortName, longName, prerequisites)
  }
}
