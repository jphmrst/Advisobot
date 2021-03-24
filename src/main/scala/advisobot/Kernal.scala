
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
 * Forms of {@link advisobot.core.Achievement Achievement}
 * besides {@link advisobot.core.Course Course}s; not currently used.
 */
trait Task extends Achievement

/**
 * Some task described only by unstructured text.
 */
class TaskDescription(val desc: String, val units: Int) extends Task {
  override def tag(): String = desc
}

object Task {
  def apply(desc: String, units: Int): Task = new TaskDescription(desc, units)
}

trait Viewer {
  def write(implicit doc: LaTeXdoc, who: Person,
            satisfiers: Map[Requirement,List[Achievement]]): Unit
}

class Grade(val pass: Boolean, val display: String,
            val gpa: Option[Double]=None) {
  override def toString(): String = display
}

/**
 *  Period in which classes are offered.
 */
trait Term extends Ordered[Term] with LaTeXRenderable {
  def next: Term
  def nextMajor: Term
  def isMain: Boolean
}

object Past {
  def apply(elems: (Term,SortedMap[Course,Grade])*): SortedMap[Term,SortedMap[Course,Grade]] =
    SortedMap[Term,SortedMap[Course,Grade]](elems: _*)
}
