
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
   * which satisfy this requirement, wrapped as an [[scala.Option]].
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
  def isCourse(c: Course): Boolean

  protected def succeeding(
    achievements: List[Achievement],
    satisfiers: HashMap[Requirement,List[Achievement]]
  ): Option[List[Achievement]] = {
    satisfiers += (this -> achievements)
    Some(achievements)
  }
}

object Requirement {
  implicit def requirementAsSingleton(r: Requirement): List[Requirement] =
    List[Requirement](r)
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
  def isCourse(c: Course): Boolean = pred(c)
}

object CoursePredicate {
  def apply(name: String, pred: Course => Boolean): CoursePredicate =
    new CoursePredicate(name, pred)
}

case class Require(val course: Course) extends Requirement {
  override def addSatisfiers(implicit who: Person,
                             satisfiers: HashMap[Requirement,List[Achievement]],
                             checkset: HashSet[Course]): Option[List[Achievement]] = {
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
  override def name: String = course.tag()
  def isCourse(c: Course): Boolean = {
    c.prefix.equals(course.prefix) && c.number.equals(course.number)
  }
}

case class Select(val shortName:String, val longName:String,
                  val count:Int, val subrequirements:List[Requirement])
extends Requirement {
  override def name: String = shortName
  override def isCourse(c: Course): Boolean =
    subrequirements.exists(_.isCourse(c))
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
      val thisSubreq = subreqIter.next()
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
  override def isCourse(c: Course): Boolean = false
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
