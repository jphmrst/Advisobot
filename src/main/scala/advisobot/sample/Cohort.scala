import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import advisobot.core.{Advisees,Person,Past,Term,PickOne,ScheduleSuggestion}
import uwlcs.advisobot._
import uwlcs.advisobot.courses._
import uwlcs.advisobot.programs._
import uwlcs.advisobot.Suggestions._
import uwlcs.advisobot.Notes._
import org.maraist.outlines.{Outline,OutlineItem}
import org.maraist.outlines.OutlineItem.{summarized,item}

object Cohort extends Advisees(
  Person("01","John", "Doe",
         "nobody@uwlax.edu",
         programs = List(GenEdAndCHS2018, CSmajor2018, MathMinor2018),
         current = List(CST110, CS220, ERS100, CS225),
         recommend = SortedMap(Fall20 -> List(CS270,CS340,ENG110,MTH208)),
         past = Past(Fall19 -> SortedMap(MS101 -> A,
                                         CS364 -> BC,
                                         MTH207 -> B,
                                         CS120 -> AB)),
         notes = SortedMap(
           Fall20 -> Outline(CHECK_INFO, REQUIRE_TELECON)
         )
       ),

  Person("02", "Jane", "Doe",
         "nobody@uwlax.edu",
         programs = List(GenEdAndCHS2018, CSmajor2018, BusinessAdminMinor2018),
         current = List(CS364, STAT145, CS224python, CS471),
         past = Past(Fall17 -> SortedMap(MTH151 -> C,
                                         PHY142 -> AB,
                                         CST110 -> AB,
                                         MIC130 -> BC,
                                         SOC110 -> B,
                                         UWL100 -> A),
                     Spring18 -> SortedMap(BIO105 -> BC,
                                           CS120 -> B,
                                           HIS102 -> BC,
                                           HED210 -> A,
                                           ENG110 -> AB),
                     Fall18 -> SortedMap(ECO120 -> AB,
                                         POL205 -> B,
                                         CS220 -> AB,
                                         CS225 -> C,
                                         ACC221 -> B),
                     Spring19 -> SortedMap(CS270 -> C,
                                           ENG200 -> AB,
                                           CS340 -> AB,
                                           MTH207 -> D),
                     Fall19 -> SortedMap(ACC222 -> BC, MTH207 -> BC,
                                         CS370 -> BC, CS341 -> B)),
         recommend = SortedMap(Spring20 -> List(CS421, CS_ELECTIVE,
                                                LAB_SCIENCE, UPPER_NON_CS3,
                                                PickOne(GENED,CS_ELECTIVE)),
                               // Past-future
                               Fall20 -> List(LAB_SCIENCE, MTH208, CS421,
                                              MINOR_REQ, MINOR_REQ),
                               Spring21 -> List(CS441, GENED3, CS_ELECTIVE,
                                                MINOR_REQ, MINOR_REQ),
                               Fall21 -> List(CS442, GENED3, GENED3,
                                              MINOR_REQ, MINOR_REQ)
                             ),
         notes = SortedMap(
           Fall20 -> Outline(CHECK_INFO, RETAKES_SO_WINGS,
                             ASK_MIN_SCHED, REQUIRE_TELECON) )
       ),

  Person("07", "Barney", "Thedinosaur",
           "nobody@uwlax.edu",
         programs = List(GenEdAndCHS2018, CSmajor2018, CSonly2018),
         current = List(CS220, CS225, MTH207, ENG110),
         past = Past(Fall19 -> SortedMap(ECO110 -> TS, ECO120 -> TS,
                                         CST110 -> B, CS120 -> B,
                                         ERS100 -> BC, MTH151 -> C)),
         recommend = SortedMap(
           Fall20 -> List(LAB_SCIENCE, MTH208, CS270, CS340),
           Spring20 -> List(ENG110, CS220, CS225,
                            PickOne(MTH207, LAB_SCIENCE))),
         notes = SortedMap(
           Fall20 -> Outline(CHECK_INFO, REQUIRE_TELECON)
         )
       ),

  Person("08", "Frank", "Enstein",
         "nobody@uwlax.edu",
         programs = List(GenEdAndCHS2018, CSmajor2018, CSonly2018),
         current = List(ART102, CS220, CS270, CST110, ENG110),
         past = Past(Fall19 -> SortedMap(MTH207 -> TB,
                                         GEL000Q(4) -> TA,
                                         PHL000L(4) -> TA,
                                         MTH225 -> TAm,
                                         CS000L(4) -> TA,
                                         GEL000E(4) -> TBp,
                                         CS000L(4) -> TA,
                                         GEL000M(4) -> TAm,
                                         GEL000E(4) -> TBp,
                                         CS120 -> AB, ERS220 -> A,
                                         MTH208 -> B, SOC225 -> A)),
         recommend = SortedMap(
           Fall20 -> List(LAB_SCIENCE, CS340,
                          PickOne(CS370, CS_ELECTIVE),
                          PickOne(GENED3, NON_CS3), GENED2),
           Spring20 -> List(CS220, CS270, ENG110, CST110,
                            PickOne(CS_ELECTIVE, GENED3, NON_CS))),
         notes = SortedMap( Fall20 -> Outline(CHECK_INFO, REQUIRE_TELECON) )
       )
) {
  override val runOnly: Option[Int] = None
  override val verbosity: Int = 2
  override val forTerm: Term = Fall20
  override val lastPast: Term = Spring20
  override val institutionShortName: String = "UWL"
  override val registrarName: String = "Records \\& Registration"
  override val recordsSystemName: String = "WINGS"
  override val shrinkNotes: Int = 0
  reportDirectory = "reports"
  reportToPhotoDirPath = Some(".")
  photoDirectory = "../img"
}


