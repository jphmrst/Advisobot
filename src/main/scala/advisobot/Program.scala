
package advisobot.core
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{Builder,HashSet,HashMap,ListBuffer}
import java.nio.file.{Paths, Files}
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable}
import org.maraist.util.UniqueHashCode
import org.maraist.outlines.{Outline}
import Trace._

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
