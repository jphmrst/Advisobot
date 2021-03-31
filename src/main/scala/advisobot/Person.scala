
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
  val wideNotes: Boolean = false,
  val planPageBreak: Boolean = false,

  // Parameters for generating plan
  val calculateRecommendationIfEmpty: Boolean = true,
  val planTermsOn: Set[Term] = Set[Term](),
  val planTermsOff: Set[Term] = Set[Term]()
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

    val courseResults: HashMap[Course, Double] = new HashMap[Course, Double]
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
        case Some(value) => courseResults.put(course, value)
        case None => { }
      }
    }

    var subtotal: Double = 0.0
    var units: Double = 0.0
    for ((course, value) <- courseResults) {
      units = units + course.units
      subtotal = subtotal + value * course.units
    }

    units match {
      case 0.0 => None
      case _ => Some(subtotal/units)
    }
  }

  def writeReport(doc: LaTeXdoc, report: PersonReport)(implicit advisees: Advisees) =
    report.writeReport(doc, this)
  def writeHandout(doc:LaTeXdoc)(implicit advisees: Advisees): Unit =
    writeReport(doc, advisees.personReport)

  /**
   * Assemble a naive first schedule of classes for going forward
   * from the given term.
   *
   * @param base Starting term of planned schedule
   */
  def getNaiveSchedule(base: Term): SortedMap[Term,List[ScheduleSuggestion]] = {
    val reqListFn: List[Requirement] => List[ScheduleSuggestion] =
      _.map(_.toSuggestions).fold(List[ScheduleSuggestion]())(_.concat(_))

    val sgsList: List[List[ScheduleSuggestion]] =
      programs.map(_.sequence).map(_.map(reqListFn)).fold(Nil)(parMerge)

    zipSchedule(base, sgsList)
  }

  private def parMerge(
    xss: List[List[ScheduleSuggestion]], yss: List[List[ScheduleSuggestion]]
  ): List[List[ScheduleSuggestion]] = xss match {
    case Nil => yss
    case (xs :: xss2) => yss match {
      case Nil => xss
      case (ys :: yss2) => (xs ++ ys) :: parMerge(xss2,yss2)
    }
  }

  /**
   * Align a series of
   * {@link advisobot.core.ScheduleSuggestion ScheduleSuggestion}s
   * with the next semesters in which this person is taking classes.
   */
  def zipSchedule(base: Term, suggestions: List[List[ScheduleSuggestion]]):
  SortedMap[Term,List[ScheduleSuggestion]] =
    zipSchedule(SortedMap.newBuilder[Term,List[ScheduleSuggestion]],
                base, suggestions)

  /**
   * Internal method to populate a map with
   * {@link advisobot.core.ScheduleSuggestion ScheduleSuggestion}s
   * for coming semesters.
   */
  private def zipSchedule(
    builder: Builder[(Term, List[ScheduleSuggestion]),
                     SortedMap[Term, List[ScheduleSuggestion]]],
    base: Term,
    suggestions: List[List[ScheduleSuggestion]]
  ): SortedMap[Term,List[ScheduleSuggestion]] = suggestions match {
    // When the list of suggestion sets is empty, return the contents of
    // the builders
    case Nil => builder.result()

    // So we have more classes.  First check if we're planning to take
    // this term off.
    case sgSet :: sgSets => planTermsOff(base) match {
      case true => zipSchedule(builder, base.next, suggestions)

      // Not taking this term off.  Still need to either find this it
      // is a main term, or that we're taking classes in this minor
      // term.
      case false => (base.isMain || planTermsOn(base)) match {
        case true => {
          builder += ((base, sgSet))
          zipSchedule(builder, base.next, sgSets)
        }
        case false => zipSchedule(builder, base.next, suggestions)
      }
    }
  }

  /**
   * Assemble a model schedule for classes going forward.
   *
   * @param base Starting term of planned schedule
   */
  def planSched(base: Term)(implicit advisees: Advisees): SortedMap[Term,List[ScheduleSuggestion]] = {
    import org.maraist.search.local.StochasticBeamSearcher
    import org.maraist.search.local.StochasticBeamBuilder
    type Builder =
      StochasticBeamBuilder[SortedMap[Term,List[ScheduleSuggestion]]]

    val searcher =
      new StochasticBeamSearcher[SortedMap[Term,List[ScheduleSuggestion]]](
        advisees.evalSched(_),
        false,
        advisees.getScheduleSuccessors(_),
        advisees.getNextBeam(_),
        advisees.getNextBeamLength(_),
        advisees.getNextBeamOrderShare(_),
        implicitly[Random]  // random: Random
      )

    searcher.search(getNaiveSchedule(base))
  }
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
    shrinkNotes: Int = 0,
    wideNotes: Boolean = false,
    planPageBreak: Boolean = false
  ) = new Person(id, firstNames, lastName, email, programs,
                 current, past, active, otherUnits, recommend, notes,
                 shrinkNotes = shrinkNotes, wideNotes = wideNotes,
                 planPageBreak = planPageBreak)
}
