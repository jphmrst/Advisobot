
package advisobot.core
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{Builder,HashSet,HashMap,ListBuffer}
import java.nio.file.{Paths, Files}
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable}
import org.maraist.util.UniqueHashCode
import org.maraist.outlines.{Outline}
import Trace._

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
  def this(exact: Int) = this(exact, Some(exact))
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

  def and(i: Int): UnitsRange = {
    val newLower: Int = lowerBound + i
    val newUpper: Option[Int] = upperBound match {
      case Some(ub) => Some(ub + i)
      case None => None
    }
    new UnitsRange(newLower, newUpper)
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
