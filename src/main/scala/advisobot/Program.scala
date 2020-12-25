
package advisobot.core
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{Builder,HashSet,HashMap,ListBuffer}
import java.nio.file.{Paths, Files}
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable}
import org.maraist.util.UniqueHashCode
import org.maraist.outlines.{Outline}
import Trace._

/**
 * @param sequence Sequential ordering of requirements for constructing
 * a sample schedule.  Co-requisites should be included within the same
 * sublist; the ordering must respect all pre- and co-requisites or
 * schedule generation will fail.   Can be omitted, in which case will
 * be derived from the requirements by making each requirement into a
 * singleton list.
 */
abstract class Program(val name: String, val longName: String,
                       val requirements: List[Requirement],
                       val sequence: List[List[Requirement]]) {
  def this(name: String, longName: String, requirements: List[Requirement]) =
    this(name, longName, requirements, requirements.map(List[Requirement](_)))
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

  def naiveSchedule(base: Term): SortedMap[Term,List[ScheduleSuggestion]] = {
    val suggestionsLists: List[List[ScheduleSuggestion]] =
      sequence.map(_.map(_.toSuggestions)
                    .fold(List[ScheduleSuggestion]())(_.concat(_)))
    val sngList =
      suggestionsLists.fold(List[ScheduleSuggestion]())(_.concat(_))
    base.zipSchedule(sngList)
  }
}
