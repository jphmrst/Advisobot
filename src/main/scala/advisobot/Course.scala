
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
      case 0 => (number compare that.number) match {
        case 0 => (suffix zip that.suffix) match {
          case None => 0
          case Some((s1,s2)) => s1 compare s2
        }
        case z => z
      }
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
