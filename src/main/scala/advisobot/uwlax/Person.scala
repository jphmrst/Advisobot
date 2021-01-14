package uwlcs.advisobot
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{Builder,HashSet,HashMap,ListBuffer}
import scala.util.{Random}
import org.maraist.latex.LaTeXdoc
import org.maraist.outlines.Outline
import advisobot.core.{Program,Requirement,Select,Grade,Term,Course,
                       Achievement,SideCondition,Viewer,CoursePredicate,
                       ScheduleSuggestion, UnitsRange}
import advisobot.builder._

// -----------------------------------------------------------------

class Person(
  id:String, firstNames:String, lastName:String,
  email:String, programs:List[Program], current:List[Course],
  past:SortedMap[Term,SortedMap[Course,Grade]],
  active:Boolean, otherUnits:Int,
  recommend: SortedMap[Term, List[ScheduleSuggestion]] = SortedMap(),
  notes: SortedMap[Term, Outline[String]] = SortedMap(),

  // Parameters for output
  notesWidth: String = "5.25in",
  shrinkNotes: Int = 0,

  // Parameters for generating plan
  calculateRecommendationIfEmpty: Boolean = true,
  planTermsOn: Set[Term] = Set[Term](),
  planTermsOff: Set[Term] = Set[Term]()
)
extends advisobot.core.Person(
  id, firstNames, lastName, email, programs, current, past,
  active, otherUnits, recommend, notes,
  notesWidth = notesWidth,
  shrinkNotes = shrinkNotes,
  calculateRecommendationIfEmpty = calculateRecommendationIfEmpty,
  planTermsOn = planTermsOn,
  planTermsOff = planTermsOff
) {
}
