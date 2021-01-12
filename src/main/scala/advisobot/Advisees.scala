
package advisobot.core
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{Builder,HashSet,HashMap,ListBuffer}
import java.io.File
import java.nio.file.{Paths, Files}
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable}
import org.maraist.util.UniqueHashCode
import org.maraist.outlines.{Outline}
import org.maraist.search.local.{StochasticBeam,StochasticBeamBuilder}
import Trace._
import ScheduleSuggestion._

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

  /**
   * TODO Given a possible schedule, return variations of that schedule.
   * This method will be passed as a function to the constructor of
   * {@link org.maraist.search.local.StochasticBeamSearcher StochasticBeamSearcher}.
   *
   * @param src Schedule to be tweaked.
   */
  def getScheduleSuccessors(src: SortedMap[Term,List[ScheduleSuggestion]]):
  Iterable[Option[SortedMap[Term,List[ScheduleSuggestion]]]] = {
    ???
  }

  /**
   * TODO Given the beam resulting from a search interation, return an empty
   * beam when we should continue searching.
   * This method will be passed as a function to the constructor of
   * {@link org.maraist.search.local.StochasticBeamSearcher StochasticBeamSearcher}.
   *
   * @param beam Result of previous round of search.
   */
  def getNextBeam(
    beam: StochasticBeam[SortedMap[Term,List[ScheduleSuggestion]]]):
  Option[StochasticBeamBuilder[SortedMap[Term,List[ScheduleSuggestion]]]] = {
    ???
  }

  /**
   * Given a
   * {@linkplain org.maraist.search.local.StochasticBeamBuilder beam builder},
   * return the length which it should be.
   * This method will be passed as a function to the constructor of
   * {@link org.maraist.search.local.StochasticBeamSearcher StochasticBeamSearcher}.
   *
   * @param newBeam
   * @return In this default implementation, 50
   */
  def getNextBeamLength(
    newBeam: StochasticBeamBuilder[SortedMap[Term,List[ScheduleSuggestion]]]
  ): Int = 50

  /**
   * Given a
   * {@linkplain org.maraist.search.local.StochasticBeamBuilder beam builder},
   * return the number of elements which should be selected based strictly
   * on the beam ordering.
   * This method will be passed as a function to the constructor of
   * {@link org.maraist.search.local.StochasticBeamSearcher StochasticBeamSearcher}.
   *
   * @param beam
   * @return In this default implementation, 10
   */
  def getNextBeamOrderShare(
    beam: StochasticBeamBuilder[SortedMap[Term,List[ScheduleSuggestion]]]
  ): Int = 10

  /**
   * Score a possible schedule for future semesters.  The score are to
   * be minimized, and by default is implemented as a sum of
   * penalties determined by the [[advisobot.core.Advisees.scorers]] method.
   */
  def evalSched(sched: SortedMap[Term,List[ScheduleSuggestion]]): Double =
    (for (scorer <- scorers) yield scorer(sched)).foldLeft(0.0)(_ + _)

  /**
   * An iterable collection of scoring functions, used in the default
   * implementation of [[evalSched]].
   */
  def scorers: Iterable[SortedMap[Term,List[ScheduleSuggestion]] => Double] =
    Seq(
      weigh(10.0, scorePrereqGap(_)),
      weigh(100.0, scoreOverUnder(_)),
      weigh(1000.0, scoreHardOverload(_)),
      weigh(1000.0, scoreHardUnderload(_))
    )
}
