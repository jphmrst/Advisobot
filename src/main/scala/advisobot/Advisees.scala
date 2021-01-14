
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

  // =================================================================

  import advisobot.core.ScheduleSuggestion.CandSchedule
  type CandidateMutator = CandSchedule => Iterator[CandSchedule]
  import org.maraist.util.Iterators._

  /**
   * Given a possible schedule, return variations of that schedule.
   * This method will be passed as a function to the constructor of
   * [[org.maraist.search.local.StochasticBeamSearcher][StochasticBeamSearcher]].
   * By default, uses the transformers in
   * [[scheduleSuccessorBuilders]].
   *
   * @param src Schedule to be tweaked.
   */
  def getScheduleSuccessors(src: CandSchedule): Iterator[CandSchedule] =
    sequenceIterators(for (xformer <- scheduleSuccessorBuilders)
                      yield xformer(src))

  /**
    * The sequence of functions to be used to derive one schedule from
    * another.  By default, includes [[generateSwaps]],
    * [[generatePulls]] and [[generateCombines]].
    */
  def scheduleSuccessorBuilders: Seq[CandidateMutator] = Seq[CandidateMutator](
    generateSwaps(_),
    generatePulls(_),
    generateCombines(_)
  )

  // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

  /**
   *  TODO Generate an iterator of the schedules obtained by swapping
   *  two classes.
   */
  def generateSwaps(sched: CandSchedule): Iterator[CandSchedule] = ???

  /**
   *  TODO Generate a possible schedule by swapping two particular
   *  classes.
   */
  def generateSwap(
    sched: CandSchedule,
    earlyTerm: Term, earlyItem: Int, laterTerm: Term, laterItem: Int
  ): Option[CandSchedule] = ???

  /**
   *  TODO Generate an iterator of the schedules obtained by moving a
   *  class to an earlier term.
   */
  def generatePulls(sched: CandSchedule): Iterator[CandSchedule] = ???

  /**
   *  TODO Generate a possible schedule by moving one particular class
   *  to an earlier term.
   */
  def generatePull(
    sched: CandSchedule,
    earlyTerm: Term, laterTerm: Term, item: Int
  ): Option[CandSchedule] = ???

  /**
   *  TODO Generate an iterator of the schedules obtained by combining
   *  two terms.
   */
  def generateCombines(sched: CandSchedule): Iterator[CandSchedule] = ???

  /**
   *  TODO Generate a possible schedule by combining two specific terms.
   */
  def generateCombine(sched: CandSchedule, earlyTerm: Term, laterTerm: Term):
  Option[CandSchedule] = ???

  // =================================================================

  /**
   * Given the beam resulting from a search interation, return an
   * empty beam when we should continue searching.  The key
   * functionality of this method is the decision as to whether search
   * should continue.  This method will be passed as a function to the
   * constructor of
   * [[org.maraist.search.local.StochasticBeamSearcher][StochasticBeamSearcher]]}.
   * The default implementation bases the decision on the number of
   * generations and on the current low-scoring schedule.
   *
   * @param beam Result of previous round of search.
   */
  def getNextBeam(beam: StochasticBeam[CandSchedule]):
  Option[StochasticBeamBuilder[CandSchedule]] = {
    val generation: Int = beam.generation
    val best: CandSchedule = beam.store.last
    continueSearching(generation, best) match {
      case true => Some(new StochasticBeamBuilder(beam))
      case false => None
    }
  }

  /**
    * TODO Decide whether to continue search.
    */
  def continueSearching(generation: Int, best: CandSchedule): Boolean =
    ???

  /**
   * Given a
   * {@linkplain org.maraist.search.local.StochasticBeamBuilder beam builder},
   * return the length which it should be.  This method will be passed
   * as a function to the constructor of
   * [[org.maraist.search.local.StochasticBeamSearcher][StochasticBeamSearcher]].
   *
   * @param newBeam
   * @return In this default implementation, 50
   */
  def getNextBeamLength(
    newBeam: StochasticBeamBuilder[CandSchedule]
  ): Int = 50

  /**
   * Given a
   * {@linkplain org.maraist.search.local.StochasticBeamBuilder beam builder},
   * return the number of elements which should be selected based strictly
   * on the beam ordering.
   * This method will be passed as a function to the constructor of
   * [[org.maraist.search.local.StochasticBeamSearcher][StochasticBeamSearcher]].
   *
   * @param beam
   * @return In this default implementation, 10
   */
  def getNextBeamOrderShare(
    beam: StochasticBeamBuilder[CandSchedule]
  ): Int = 10

  /**
   * Score a possible schedule for future semesters.  The score are to
   * be minimized, and by default is implemented as a sum of
   * penalties determined by the [[advisobot.core.Advisees.scorers]] method.
   */
  def evalSched(sched: CandSchedule): Double =
    (for (scorer <- scorers) yield scorer(sched)).foldLeft(0.0)(_ + _)

  /**
   * An iterable collection of scoring functions, used in the default
   * implementation of [[evalSched]].
   */
  def scorers: Iterable[CandSchedule => Double] =
    Seq(
      weigh(10.0, scorePrereqGap(_)),
      weigh(100.0, scoreOverUnder(_)),
      weigh(1000.0, scoreHardOverload(_)),
      weigh(1000.0, scoreHardUnderload(_))
    )
}
