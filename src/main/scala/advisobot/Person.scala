
package advisobot.core
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{Builder,HashSet,HashMap,ListBuffer}
import scala.util.{Random}
import java.nio.file.{Paths, Files}
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable}
import org.maraist.util.UniqueHashCode
import org.maraist.outlines.{Outline}

class Person(
  val id:String, val firstNames:String, val lastName:String,
  val email:String, val programs:List[Program], val current:List[Course],
  val past:SortedMap[Term,SortedMap[Course,Grade]],
  val active:Boolean, val otherUnits:Int,
  val recommend: SortedMap[Term, List[ScheduleSuggestion]] = SortedMap(),
  val notes: SortedMap[Term, Outline[String]] = SortedMap(),

  // Parameters for output
  val notesWidth: String = "5.25in",
  val shrinkNotes: Int = 0,

  // Parameters for generating plan
  val calculateRecommendationIfEmpty: Boolean = true,
  val targetUnits: Int = 15,
  val maxUnits: Int = 18,
  val minUnits: Int = 12,
  val maxLastUnits: Int = 18,
  val minLastUnits: Int = 0,
  val hardMaxUnits: Int = 18,
  val hardMinUnits: Int = 0
) {

  import org.maraist.search.local.BeamSearchConverters._
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
  def completed: List[Course] = {
    val result = new scala.collection.mutable.ListBuffer[Course]
    for (results <- past.values; (course, grade) <- results; if grade.pass) {
      result += course
    }
    result.result()
  }

  def completions: SortedMap[Course,Grade] = {
    val builder = SortedMap.newBuilder[Course,Grade]

    for ((term, results) <- past; (course, grade) <- results) {
      builder += ((course, grade))
    }

    val result = builder.result()
    result
  }

  /**
   * Return the number of units earned in past terms, including units
   * given in otherUnits.
   */
  def unitsCompleted: Int = {
    var total = otherUnits
    for((course, grade) <- completions; if grade.pass) {
      total = total + course.units
    }
    total
  }

  /**
   * Returns a map from each course taken by this person, to the
   * term when they most recently took that course.
   */
  def mostRecentlyTaken(now: Term): SortedMap[Course,Term] = {
    val builder = SortedMap.newBuilder[Course,Term]

    for ((term, results) <- past; (course, grade) <- results) {
      builder += ((course, term))
    }

    for(course <- current) {
      builder += ((course, now))
    }

    builder.result()
  }

  /**
   * Return the number of units earned in past terms plus what
   * might be earned this term, including units given in otherUnits,
   * avoiding duplicate (grade-replacement) classes.
   */
  def unitsProspective: Int = {
    var total = otherUnits

    for(course <- current) {
      total = total + course.units
    }

    for((course, grade) <- completions;
        if grade.pass && !(current.contains(course))
      ) {
      total = total + course.units
    }
    total
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

  /**
   * TODO Assemble a naive first schedule of classes for going forward
   * from the given term.
   *
   * @param base Starting term of planned schedule
   */
  def getNaiveSchedule(base: Term): SortedMap[Term,List[ScheduleSuggestion]] = {
    val builder = SortedMap.newBuilder[Term,List[ScheduleSuggestion]]
    val programNaiveSchedules = programs.map(_.naiveSchedule(base))

    ???
  }

  /**
   * TODO Assemble a model schedule for classes going forward
   *
   * @param base Starting term of planned schedule
   */
  def planSched(base: Term): SortedMap[Term,List[ScheduleSuggestion]] = {
    import org.maraist.search.local.StochasticBeamSearcher
    import org.maraist.search.local.StochasticBeamBuilder
    type Builder =
      StochasticBeamBuilder[SortedMap[Term,List[ScheduleSuggestion]]]

    val searcher =
      new StochasticBeamSearcher[SortedMap[Term,List[ScheduleSuggestion]]](
        evalSched(_),
        false,
        ???, // successors:  (S) => Iterable[Option[S]]
        ???, // nextBeam:    (StochasticBeam[S]) => Option[Builder]
        (_: Builder) => 50,
        (_: Builder) => 10,
        implicitly[Random]  // random:      Random
      )

    searcher.search(getNaiveSchedule(base))
  }

  /**
   *  TODO Score a possible schedule for future semesters.
   */
  def evalSched(sched: SortedMap[Term,List[ScheduleSuggestion]]): Double = ???

  /**
   *  TODO Generate a possible schedule by swapping two classes.
   */
  def generateSwap(
    sched: SortedMap[Term,List[ScheduleSuggestion]],
    earlyTerm: Term, earlyItem: Int, laterTerm: Term, laterItem: Int
  ): Option[SortedMap[Term,List[ScheduleSuggestion]]] = ???

  /**
   *  TODO Generate a possible schedule by moving one class
   *  to an earlier term.
   */
  def generatePull(
    sched: SortedMap[Term,List[ScheduleSuggestion]],
    earlyTerm: Term, laterTerm: Term, item: Int
  ): Option[SortedMap[Term,List[ScheduleSuggestion]]] = ???

  /**
   *  TODO Generate a possible schedule by combining two terms.
   */
  def generateCombine(
    sched: SortedMap[Term,List[ScheduleSuggestion]],
    earlyTerm: Term, laterTerm: Term
  ): Option[SortedMap[Term,List[ScheduleSuggestion]]] = ???

}

object Person {
  def apply(
    id: String, firstNames: String, lastName: String,
    email: String, programs: List[Program],
    current: List[Course],
    past: SortedMap[Term,SortedMap[Course,Grade]],
    recommend: SortedMap[Term, List[ScheduleSuggestion]],
    active: Boolean=true, otherUnits: Int=0,
    notes: SortedMap[Term, Outline[String]] = SortedMap(),
    shrinkNotes: Int = 0
  ) = new Person(id, firstNames, lastName, email, programs,
                 current, past, active, otherUnits, recommend, notes,
                 shrinkNotes = shrinkNotes)
}
