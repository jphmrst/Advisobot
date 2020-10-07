
package advisobot.core
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{HashSet,HashMap,ListBuffer}
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

import Trace._

object Settings {
  var courseDigits = 3

  lazy val courseNumberFormatter: java.text.NumberFormat =
    new java.text.DecimalFormat("0" * courseDigits)
}

/**
 * Achievements can be attributed to a {@link advisobot.core.Person Person}.
 * This trait allows us to move away from only tracking
 * {@link advisobot.core.Course Course}s.
 */
trait Achievement extends UniqueHashCode {
  def tag():String
  def formatSatisfier(doc:LaTeXdoc): Unit = { doc ++= tag() }
  def courseCheck(pred: (Course) => Boolean): Boolean = this match {
    case course: Course => pred(course)
    case _ => false
  }
  def units: Int
}

/**
 * Placeholder for forms of {@link advisobot.core.Achievement Achievement}
 * besides {@link advisobot.core.Course Course}s; not currently used.
 */
trait Task extends Achievement

/**
 * Representation of one listing in a course catalog.
 *
 * @param units The number of units associated with this course.  Courses
 * with a variable number of units are represented as a
 * {@link advisobot.builder.VariableUnits VariableUnits}, which
 * given a number of units for a particular offering returns a Course
 * instance.
 * @param prefix Department prefix, e.g. "CS" or "MTH".
 * @param number Catalog number of this course.
 * @param shortName Short name for the course, used in the relatively
 * narrow grids of the advising reports.
 * @param longName Full course name.
 * @param prerequisites List of
 * {@link advisobot.core.Requirement Requirement}s considered prerequisite
 * for this course.
 */
class Course(val units:Int,
             val prefix:String, val number:Int, val suffix: Option[String],
             val shortName:String, val longName:String,
             val prerequisites:List[Requirement])
extends Achievement with Ordered[Course] with LaTeXRenderable {
  def this(units:Int, prefix:String, number:Int) =
    this(units, prefix, number, None,
         prefix+number.toString(), prefix+number.toString(), List())
  def this(units:Int, prefix:String, number:Int, name:String) =
    this(units, prefix, number, None, name, name, List())

  implicit def course2singleRequirement(course:Course) = Require(course)

  def this(units:Int, prefix:String, num:Int,
           short:String, long:String, pre:Requirement*) =
    this(units, prefix, num, None, short, long, pre.toList)
  def this(units:Int, prefix:String, num:Int, name:String, pre:Requirement*) =
    this(units, prefix, num, None, name, name, pre.toList)
  def this(units:Int, prefix:String, num:Int, pre:Requirement*) =
    this(units, prefix, num, None, prefix+num, prefix+num, pre.toList)

  private var corequisites:List[Course] = List()

  override def tag():String = suffix match {
    case Some(s) => prefix + Settings.courseNumberFormatter.format(number) + s
    case None => prefix + Settings.courseNumberFormatter.format(number)
  }

  override def formatSatisfier(doc:LaTeXdoc): Unit = toLaTeX(doc)

  override def compare(that: Course): Int =
    (prefix compare that.prefix) match {
      case 0 => number compare that.number
      case x => x
    }

  override def toString(): String = tag()

  override def toLaTeX(doc:LaTeXdoc): Unit = {
    doc ++= prefix
    doc ++= "\\,"
    doc ++= Settings.courseNumberFormatter.format(number)
    suffix match {
      case Some(s) => doc ++= s
      case None => { }
    }
  }
}

// =================================================================

trait Requirement extends UniqueHashCode {

  /**
   * Attempt to find courses satisfying a requirement
   *
   * @param who The person in question
   * @param satisfiers Under-construction (mutable) map from requirements
   * objects to the {@link advisobot.core.Achievement Achievement}s which
   * satisfy them.  When this method is invoked, one expects that this
   * requirement is not a key in this hash, but that after this method (if
   * the requirement is satisfied), this would be a key.
   * @param checkset The courses not yet used to satisfy a requirement
   *
   * @return The list of {@link advisobot.core.Achievement Achievement}s
   * which satisfy this requirement, wrapped as an {@link scala.Option}
   */
  def addSatisfiers(
    implicit who: Person, satisfiers:HashMap[Requirement,List[Achievement]],
    checkset:HashSet[Course]
  ): Option[List[Achievement]]

  def formatSatisfaction(implicit doc: LaTeXdoc, map: Map[Requirement,List[Achievement]], who: Person
  ) = {
    map.get(this) match {
      case Some(sats) => {
        for(i <- 0 until sats.size) {
          if (i>0) doc ++= "   & "
          val sat = sats(i)
          if (who.isCurrent(sat)) {
            doc ++= "\\colorbox{yellow}{\\textsc{now}}"
          } else {
            doc ++= "\\textcolor{green}{\\checkmark}"
          }
          doc ++= " & "
          sat.formatSatisfier(doc)
        }
        for(i <- sats.size until count) {
          if (i>0) doc ++= "   & "
          doc ++= "\\colorbox{red!30}{\\rule{3mm}{0mm}\\rule{0mm}{2mm}} & "
          formatUnsatisfied(doc)
        }
      }
      case None => {
        for(i <- 0 until count) {
          doc ++= "\\colorbox{red!30}{\\rule{3mm}{0mm}\\rule{0mm}{2mm}} & "
          formatUnsatisfied(doc)
        }
      }
    }
  }
  def name: String
  def count: Int
  def formatUnsatisfied(doc:LaTeXdoc): Unit

  protected def succeeding(
    achievements: List[Achievement],
    satisfiers: HashMap[Requirement,List[Achievement]]
  ): Option[List[Achievement]] = {
    satisfiers += (this -> achievements)
    Some(achievements)
  }
}

/** Named predicate used to express additional conditions on a list
 * of courses intended to satisfy some requirement.
 */
trait SideCondition {
  /**
   * @param doc LaTeX document representation
   * @param achievements List of satisfiers to be tested
   * @param who Person in context
   */
  def renderSatisfaction(achievements: List[Achievement]
                       )(implicit doc: LaTeXdoc, who: Person): Unit

  /**
   * @param achievements List of satisfiers to be tested
   * @param who Person in context
   */
  def satisfied(achievements: List[Achievement])(implicit who: Person): Boolean
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class CoursePredicate(val name: String, val pred: Course => Boolean)
extends Requirement {
  override def addSatisfiers(implicit who: Person, satisfiers: HashMap[Requirement,List[Achievement]],
                    checkset: HashSet[Course]): Option[List[Achievement]] = {
    for (possibleSatisfier <- checkset) {
      if (pred(possibleSatisfier)) {
        checkset -= possibleSatisfier
        return succeeding(List[Achievement](possibleSatisfier), satisfiers)
      }
    }
    None
  }

  override def count: Int = 1
  override def formatUnsatisfied(doc:LaTeXdoc): Unit = { doc ++= name }
}

object CoursePredicate {
  def apply(name: String, pred: Course => Boolean): CoursePredicate =
    new CoursePredicate(name, pred)
}

case class Require(val course:Course) extends Requirement {
  override def addSatisfiers(implicit who: Person, satisfiers:HashMap[Requirement,List[Achievement]],
                             checkset:HashSet[Course]): Option[List[Achievement]] = {
    if (checkset.contains(course)) {
      checkset -= course
      val steps = List[Achievement](course)
      satisfiers += (this -> steps)
      Some(steps)
    } else None
  }
  override def formatUnsatisfied(doc:LaTeXdoc):Unit = {
    doc ++= course.prefix
    doc ++= "\\,"
    doc ++= course.number.toString()
  }
  override val count: Int = 1
  override def name: String = course.tag
}

case class Select(val shortName:String, val longName:String,
                  val count:Int, val subrequirements:List[Requirement])
extends Requirement {
  override def name: String = shortName
  override def addSatisfiers(
    implicit who: Person, satisfiers:HashMap[Requirement,List[Achievement]],
    checkset:HashSet[Course]
  ): Option[List[Achievement]] = {
    traceBegin("Select.addSatisfiers", "\"%s\" %d", shortName, count)
    var satisfied=0
    var subreqIter=subrequirements.iterator
    val buffer = new ListBuffer[Achievement]
    trace("Initial loop check %d<%d && %s",
          satisfied, count, subreqIter.hasNext.toString())
    while (satisfied<count && subreqIter.hasNext) {
      val thisSubreq = subreqIter.next
      traceBegin("while body", "%s", thisSubreq.name)
      thisSubreq.addSatisfiers(who, satisfiers, checkset) match {
        case Some(steps) => {
          trace("Added %d", steps.length)
          satisfied = satisfied+1
          buffer ++= steps
        }
        case None => {
          trace("None added")
        }
      }
      traceEnd("while body", "%d<%d && %s",
               satisfied, count, subreqIter.hasNext.toString())

    }
    trace("Post-exit loop check: %d<%d && %s",
          satisfied, count, subreqIter.hasNext.toString())
    val result = buffer.toList
    satisfiers += (this -> buffer.toList)
    traceEnd("Select.addSatisfiers")
    result match {
      case Nil => None
      case _ => Some(result)
    }
  }
  override def formatUnsatisfied(doc:LaTeXdoc):Unit =
    doc ++= shortName
    // subrequirements(0).formatUnsatisfied(doc)
}

/** For future expansion */
case class Complete(val task:Task) extends Requirement {
  override def addSatisfiers(
    implicit who: Person, satisfiers:HashMap[Requirement,List[Achievement]],
    checkset:HashSet[Course]
  ): Option[List[Achievement]] = None
  override def name: String = task.toString()
  override def formatUnsatisfied(doc:LaTeXdoc):Unit = { doc ++= task.tag() }
  override val count: Int = 1
}

object Select {
  def apply(name: String, count: Int, reqs: List[Requirement]): Requirement =
    Select(name, name, count, reqs)

  def apply(count: Int, reqs: List[Requirement]): Requirement =
    Select(reqs(0).name, reqs(0).name, count, reqs)

//  def apply(count:Int, reqs:List[Requirement]):Requirement =
//    Select(reqs(0).name, reqs(0).name, count, reqs)
//  def apply(count:Int, reqs:Requirement*):Requirement =
//    Select(count, reqs.toList)
//  def apply(name:String, count:Int, reqs:Requirement*):Requirement =
//    Select(name, name, count, reqs.toList)
//  def apply(short:String, long:String, count:Int,
//            reqs:Requirement*):Requirement =
//    Select(short, long, count, reqs.toList)

}

// =================================================================

trait Viewer {
  def write(implicit doc: LaTeXdoc, who: Person,
            satisfiers: Map[Requirement,List[Achievement]]): Unit
}

abstract class Program(val name: String, val longName: String,
                       val requirements: List[Requirement]) {
  def this(name: String, longName: String, requirements: Requirement*) =
    this(name, longName, requirements.toList)
  def this(name: String, requirements: Requirement*) =
    this(name, name, requirements.toList)

  def viewers: List[Viewer]
  var viewAsProgram = true

  def writeForHandout(doc: LaTeXdoc, who: Person): Unit = {
    val satisfiers = getSatisfiers(who)
    viewers.map(_.write(doc, who, satisfiers))
  }
  def getSatisfiers(who:Person): Map[Requirement, List[Achievement]] = {
    val result = new HashMap[Requirement, List[Achievement]]
    val checkset = who.makeCheckset
    for(requirement <- requirements) {
      requirement.addSatisfiers(who, result, checkset)
    }
    result
  }

  implicit def requirementsByNumber(req: Int) = requirements(req)
}

// =================================================================

class Grade(val pass: Boolean, val display: String,
            val gpa: Option[Double]=None) {
  override def toString(): String = display
}

trait Term extends Ordered[Term] with LaTeXRenderable
object Past {
  def apply(elems: (Term,SortedMap[Course,Grade])*): SortedMap[Term,SortedMap[Course,Grade]] =
    SortedMap[Term,SortedMap[Course,Grade]](elems: _*)
}

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

// =================================================================

class Person(val id:String, val firstNames:String, val lastName:String,
             val email:String, val programs:List[Program],
             val current:List[Course],
             val past:SortedMap[Term,SortedMap[Course,Grade]],
             val active:Boolean, val otherUnits:Int,
             val recommend: SortedMap[Term, List[ScheduleSuggestion]],
             val notes: SortedMap[Term, Outline[String]] = SortedMap(),
             val notesWidth: String = "5.25in") {
  implicit val me: Person = this

  def this(id: String, firstNames: String, lastName: String, email: String,
           programs: List[Program], current: List[Course],
           past: SortedMap[Term,SortedMap[Course,Grade]], otherUnits: Int,
           recommend: SortedMap[Term, List[ScheduleSuggestion]]) =
    this(id, firstNames, lastName, email, programs,
         current, past, true, otherUnits, recommend)

  def this(id: String, firstNames: String, lastName: String, email: String,
           programs: List[Program], current: List[Course],
           past: SortedMap[Term,SortedMap[Course,Grade]], otherUnits: Int,
           recommend: SortedMap[Term, List[ScheduleSuggestion]],
           notes: SortedMap[Term, Outline[String]]) =
    this(id, firstNames, lastName, email, programs,
         current, past, true, otherUnits, recommend, notes)

  def isCompleted(c:Course):Boolean = completed.contains(c)
  def completed = {
    val result = new scala.collection.mutable.ListBuffer[Course]
    for (results <- past.values; (course, grade) <- results; if grade.pass) {
      result += course
    }
    result.result()
  }
  def willHaveCompleted(c:Course):Boolean = isCompleted(c) || isCurrent(c)
  def isCurrent(a:Achievement):Boolean = a match {
    case c:Course => current.contains(c)
    case _ => false
  }

  def classStatusMark(c:Course, short:Boolean):String = ???
  def classStatusMark(c:Course):String = classStatusMark(c,false)

  override def toString(): String = s"$firstNames $lastName"

  def makeCheckset:HashSet[Course] =
    new HashSet[Course] ++= current ++= completed

  def gpa(term: Option[Term] = None,
          cumulative: Boolean = false,
          prefix: Option[String] = None): Option[Double] = {
    var subtotal: Double = 0.0
    var units: Double = 0.0
    for ((thisTerm, results) <- past;
         if (term match {
           case None => true
           case Some(t) => (cumulative && thisTerm <= t) || (thisTerm == t)
         });
         (course, grade) <- results;
         if (prefix match {
           case None => true
           case Some(p) => course.prefix equals p
         })) {
      grade.gpa match {
        case Some(value) => {
          units = units + course.units
          subtotal = subtotal + value * course.units
        }
        case None => { }
      }
    }

    units match {
      case 0 => None
      case _ => Some(subtotal/units)
    }
  }

  def writeReport(doc: LaTeXdoc, report: PersonReport)(implicit advisees: Advisees) =
    report.writeReport(doc, this, advisees)
  def writeHandout(doc:LaTeXdoc)(implicit advisees: Advisees): Unit =
    writeReport(doc, advisees.personReport)
}

object Person {
  def apply(
    id: String, firstNames: String, lastName: String,
    email: String, programs: List[Program],
    current: List[Course],
    past: SortedMap[Term,SortedMap[Course,Grade]],
    recommend: SortedMap[Term, List[ScheduleSuggestion]],
    active: Boolean=true, otherUnits: Int=0,
    notes: SortedMap[Term, Outline[String]] = SortedMap()
  ) = new Person(id, firstNames, lastName, email, programs,
                 current, past, active, otherUnits, recommend, notes)
}

class ScheduleSuggestion(val description: CourseSelection,
                         val units: UnitsRange)
extends LaTeXRenderable {
  override def toLaTeX(doc: LaTeXdoc): Unit = {
    val selectionFormatter = SelectionFormatter.currentFormatter
    val colorName = selectionFormatter.colorName
    val formatter = selectionFormatter.formatter

    doc ++= """      \\ \hline \multicolumn{1}{|c|}{\textcolor{"""
    doc ++= colorName
    doc ++= "}{"
    doc ++= formatter
    units.toLaTeX(doc)
    doc ++= """}} & \multicolumn{1}{c|}{\textcolor{"""
    doc ++= colorName
    doc ++= "}{"
    doc ++= formatter
    description.toLaTeX(doc)
    doc ++= "}}\n"
  }
}

object ScheduleSuggestion {
  implicit def fromCourse(cl: Course): ScheduleSuggestion =
    new ScheduleSuggestion(new SpecificClass(cl), cl.units)
  def toTake(description: CourseSelection, units: UnitsRange) =
    new ScheduleSuggestion(description, units)
}

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

sealed trait Loading extends LaTeXRenderable {
  def and(that: Loading): Loading
  def or(that: Loading): Loading
}

private[core] class NoLoadInfo extends Loading {
  def and(that: Loading): Loading = that
  def or(that: Loading): Loading = that
  override def toLaTeX(doc:LaTeXdoc): Unit = {
    doc ++= "any"
  }
}

object NoLoadInfo {
  val NO_INFO: Loading = new NoLoadInfo()
}

class UnitsRange(val lowerBound: Int, val upperBound: Option[Int])
extends Loading {
  def this() = this(0, None)
  def this(lower: Int, upper: Int) = this(lower, Some(upper))
  override def or(l: Loading): UnitsRange = l match {
    case _: NoLoadInfo => this
    case that: UnitsRange => {
      val newLower: Int = lowerBound min (that.lowerBound)
      val newUpper: Option[Int] = upperBound match {
        case Some(ub1) => that.upperBound match {
          case Some(ub2) => Some(ub1 max ub2)
          case None => None
        }
        case None => None
      }
      new UnitsRange(newLower, newUpper)
    }
  }

  override def and(l: Loading): UnitsRange = l match {
    case _: NoLoadInfo => this
    case that: UnitsRange => {
      val newLower: Int = lowerBound + that.lowerBound
      val newUpper: Option[Int] = upperBound match {
        case Some(ub1) => that.upperBound match {
          case Some(ub2) => Some(ub1+ub2)
          case None => None
        }
        case None => None
      }
      new UnitsRange(newLower, newUpper)
    }
  }

  override def toLaTeX(doc:LaTeXdoc): Unit = {
    doc ++= lowerBound.toString()

    if (upperBound.isDefined && lowerBound < upperBound.get) {
      doc ++= "--"
      upperBound match {
        case Some(x) => { doc ++= x.toString() }
        case None => { }
      }
    }
  }
}

object UnitsRange {
  def exactly(exact: Int) = new UnitsRange(exact, Some(exact))
  def atLeast(start: Int) = new UnitsRange(start, None)
  implicit def fromInt(x: Int): UnitsRange = exactly(x)
}

abstract class Advisees(people:Person*) {
  implicit def cohort: Advisees = this
  val forTerm: Term
  val lastPast: Term
  val institutionShortName: String
  val registrarName: String
  val recordsSystemName: String
  implicit val verbosity: Int = 1

  var photoDirectory: String = "img"
  var reportDirectory: String = "."
  var reportToPhotoDirPath: Option[String] = None

  private val thePersonReport: PersonReport = new DefaultPersonReport
  def personReport: PersonReport = thePersonReport

  def getHandoutFileRoot(person: Person): String =
    person.lastName + person.firstNames.replaceAll(" ", "")

  def reports(): Unit = {
    var i: Int = 0
    if (verbosity>0) println("Processing " + people.size + " advisee records")
    for (person <- people) {
      i = i+1
      if (verbosity>1)  print(s" $i. $person")
      if (person.active) {
        if (verbosity>1) println("...")
        val doc = new LaTeXdoc(reportDirectory + "/"
                               + getHandoutFileRoot(person))
        person.writeHandout(doc)
        doc.close()
      } else {
        if (verbosity>1) println(" --- not active")
      }
    }
    if (verbosity>1) println("Finished")
  }

  def main(args: Array[String]): Unit = {
    reports()
  }
}
