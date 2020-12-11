
package advisobot.core
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{Builder,HashSet,HashMap,ListBuffer}
import java.io.File
import java.nio.file.{Paths, Files}
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable}
import org.maraist.util.UniqueHashCode
import org.maraist.outlines.{Outline}
import Trace._

abstract class Advisees(people:Person*) {
  implicit def cohort: Advisees = this
  val runOnly: Option[Int]
  val forTerm: Term
  val lastPast: Term
  val institutionShortName: String
  val registrarName: String
  val recordsSystemName: String
  val shrinkNotes: Int
  implicit val verbosity: Int = 1

  var reportDirectory: String = "."
  var reportToPhotoDirPath: Option[String] = None

  /**
   * The photo directory must be either (1) an absolute path, and
   * (2) a relative path from the directory where the LaTeX output
   * is written, to the directory holding images.
   */
  var photoDirectory: String = "img"

  private val thePersonReport: PersonReport = new DefaultPersonReport
  def personReport: PersonReport = thePersonReport

  def getHandoutFileRoot(person: Person): String =
    person.lastName + person.firstNames.replaceAll(" ", "")

  def reports(): Unit = {
    new File(reportDirectory).mkdirs()

    if (verbosity>0) println("Processing " + people.size + " advisee records")
    runOnly match {
      case Some(idx) => {
        report(people(idx), 1)
      }
      case None => {
        var i: Int = 0
        for (person <- people) {
          i = i+1
          report(person, i)
        }
        if (verbosity>1) println("Finished")
      }
    }
  }

  private def report(person: Person, i: Int): Unit = {
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

  def main(args: Array[String]): Unit = {
    reports()
  }
}
