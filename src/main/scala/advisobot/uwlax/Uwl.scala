package uwlcs.advisobot
import scala.collection.Map
import scala.collection.mutable.HashMap
import org.maraist.latex.LaTeXdoc
import advisobot.core.{Program,Requirement,Select,Person,Grade,Term,
                       Achievement,SideCondition,Viewer,CoursePredicate,
                       ScheduleSuggestion, UnitsRange}
import advisobot.builder._

// -----------------------------------------------------------------

case class UncheckedCondition(val desc: String) extends SideCondition {
  override def satisfied(acs: List[Achievement])(implicit who: Person) = true
  override def renderSatisfaction(achievements: List[Achievement]
                                )(implicit doc: LaTeXdoc, who: Person) = {
    doc ++= desc
    doc ++= " --- \\colorbox{yellow}{Check via WINGS}"
  }
}

// -----------------------------------------------------------------

object A extends Grade(true, "A", Some(4.0))
object AB extends Grade(true, "AB", Some(3.5))
object B extends Grade(true, "B", Some(3))
object BC extends Grade(true, "BC", Some(2.5))
object C extends Grade(true, "C", Some(2.0))
object D extends Grade(true, "D", Some(1.0))
object F extends Grade(false, "F", Some(0))
object P extends Grade(true, "P")
object S extends Grade(true, "S")
object TA extends Grade(true, "TA")
object TAp extends Grade(true, "TA+")
object TAm extends Grade(true, "TA-")
object TAB extends Grade(true, "TAB")
object TB extends Grade(true, "TB")
object TBp extends Grade(true, "TB+")
object TBm extends Grade(true, "TB-")
object TBC extends Grade(true, "TBC")
object TC extends Grade(true, "TC")
object TCp extends Grade(true, "TC+")
object TCm extends Grade(true, "TC-")
object TD extends Grade(true, "TD")
object TDp extends Grade(true, "TD+")
object TDm extends Grade(true, "TD-")
object TF extends Grade(false, "TF")
object TP extends Grade(true, "TP")
object TS extends Grade(true, "TS")
object U extends Grade(false, "U", Some(0))
object W extends Grade(false, "W")

abstract class SemesterCode(val name: String)
object Fall extends SemesterCode("Fall")
object Spring extends SemesterCode("Spring")
object Winter extends SemesterCode("Winter")
object Summer extends SemesterCode("Summer")

abstract class Semester(val term: SemesterCode, val year: Int, val code: Int)
extends Term {

  def toLaTeX(doc: LaTeXdoc) = {
    doc ++= term.name
    doc ++= " "
    doc ++= year.toString()
  }

  override def compare(o: Term): Int = o match {
    case (that: Semester) => code - that.code
    case _ => -1
  }
  override def toString(): String = term.name + " " + year
}
object Fall14 extends Semester(Fall, 2014, 2154)
object Spring15 extends Semester(Spring, 2015, 2157)
object Summer15 extends Semester(Summer, 2015, 2160)
object Fall15 extends Semester(Fall, 2015, 2164)
object Spring16 extends Semester(Spring, 2016, 2167)
object Summer16 extends Semester(Summer, 2016, 2170)
object Fall16 extends Semester(Fall, 2016, 2174)
object Spring17 extends Semester(Spring, 2017, 2177)
object Summer17 extends Semester(Summer, 2017, 2180)
object Fall17 extends Semester(Fall, 2017, 2184)
object Spring18 extends Semester(Spring, 2018, 2187)
object Summer18 extends Semester(Summer, 2018, 2190)
object Fall18 extends Semester(Fall, 2018, 2194)
object Winter19 extends Semester(Winter, 2019, 2196)
object Spring19 extends Semester(Spring, 2019, 2197)
object Summer19 extends Semester(Summer, 2019, 2200)
object Fall19 extends Semester(Fall, 2019, 2204)
object Winter20 extends Semester(Winter, 2020, 2206)
object Spring20 extends Semester(Spring, 2020, 2207)
object Summer20 extends Semester(Summer, 2020, 2210)
object Fall20 extends Semester(Fall, 2020, 2214)
object Winter21 extends Semester(Winter, 2021, 2216)
object Spring21 extends Semester(Spring, 2021, 2217)
object Summer21 extends Semester(Summer, 2021, 2220)
object Fall21 extends Semester(Fall, 2021, 2224)
object Winter22 extends Semester(Winter, 2022, 2226)
object Spring22 extends Semester(Spring, 2022, 2227)
object Summer22 extends Semester(Summer, 2022, 2230)
object Fall22 extends Semester(Fall, 2022, 2234)
object Winter23 extends Semester(Winter, 2023, 2236)
object Spring23 extends Semester(Spring, 2023, 2237)
object Summer23 extends Semester(Summer, 2023, 2240)
object Fall23 extends Semester(Fall, 2023, 2244)
object Winter24 extends Semester(Winter, 2024, 2246)
object Spring24 extends Semester(Spring, 2024, 2247)
object Summer24 extends Semester(Summer, 2024, 2250)
object Fall24 extends Semester(Fall, 2024, 2254)

object Suggestions {
  val ELECTIVE = new ScheduleSuggestion("Elective", 3)
  val ELECTIVE1 = new ScheduleSuggestion("Elective", 1)
  val ELECTIVE2 = new ScheduleSuggestion("Elective", 2)
  def electivesTotalling(units: Int) =
    new ScheduleSuggestion("Electives", units)
  val LAB_SCIENCE = new ScheduleSuggestion("A lab science", 4)
  val CS_ELECTIVE = new ScheduleSuggestion("CS elective", 3)
  val UPPER_CS_ELECTIVE =
    new ScheduleSuggestion("CS elective\\\\(300/400-lv.)", 3)
  val GRAD_CS_ELECTIVE = new ScheduleSuggestion("CS elective (500-lv.)", 3)
  val MATH_ELECTIVE = new ScheduleSuggestion("Math elective", 3)
  val MINOR_ELECTIVE3 = new ScheduleSuggestion("Minor elective", 3)
  val MINOR_ELECTIVE = new ScheduleSuggestion("Minor elective", UnitsRange.atLeast(1))
  val GENED = new ScheduleSuggestion("Gen.\\ ed. elective",
                                     new UnitsRange(2,3))
  val GENED4 = new ScheduleSuggestion("Gen.\\ ed.\\\\ arts electives", 4)
  val GENED3 = new ScheduleSuggestion("3-unit gen.\\ ed.", 3)
  val GENED2 = new ScheduleSuggestion("2-unit (arts) gen.\\ ed.", 2)
  val NON_CS = new ScheduleSuggestion("Non-CS", UnitsRange.atLeast(2))
  val NON_CS3 = new ScheduleSuggestion("Non-CS", 3)
  val UPPER_NON_CS = new ScheduleSuggestion("Non-CS 300$^+$-lv.", UnitsRange.atLeast(1))
  val UPPER_NON_CS3 = new ScheduleSuggestion("Non-CS 300$^+$-lv.", 3)
  val IS_REQ = new ScheduleSuggestion("I.S. requirement", 3)
  val PSY_ELECT = new ScheduleSuggestion("PSY elective", 3)
  val PHIL_OTHER = new ScheduleSuggestion("Other phil. course", 3)
  val MINOR_REQ = new ScheduleSuggestion("Req. for minor", 3)
  val MINOR_ELECT = new ScheduleSuggestion("Elect. for minor", 3)
  val HIST_ELECT = new ScheduleSuggestion("History elective", 3)
  val HIST_SURVEY = new ScheduleSuggestion("History survey", 3)
  val NON_CS_3unitsplus =
    new ScheduleSuggestion("Non-CS", UnitsRange.atLeast(3))
  val UPPER_NON_CS_3unitsplus =
    new ScheduleSuggestion("Non-CS 300$^+$-lv.", UnitsRange.atLeast(3))
}

// Gen-eds on
// http://catalog.uwlax.edu/undergraduate/generaleducation/#generaleducationrequirementstext

