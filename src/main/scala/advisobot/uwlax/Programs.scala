package uwlcs.advisobot.programs
import scala.collection.Map
import scala.collection.mutable.HashMap
import org.maraist.latex.LaTeXdoc
import advisobot.core.{Complete,Program,Requirement,Select,Person,Grade,Term,
                       Achievement,SideCondition,Viewer,CoursePredicate,
                       ScheduleSuggestion, DescribedClasses, UnitsRange, Task}
import advisobot.builder._
import advisobot.builder.Functions._
import uwlcs.advisobot._
import uwlcs.advisobot.courses._

/** Requirements for a CS major (as of 2018) */
object CSmajor2018 extends Program(
  "CS major", "CS major (general)",
  MTH207, MTH208, CS120, CS220, OneOf(CS225,MTH225),
  CS270, CS340, CS341, CS370, CS421, CS441, CS442,
  WithConditions("CS major electives",
                 AllSatisfying("CS elective classes",
                               Select("CS elective class", 1,
                                      List(CS202, CS224gen, CS224python, CS272,
                                           CS227, PHY335, MTH317, MTH371,
                                           CS342,  CS351, CS352, CS353, CS364,
                                           CS372, CourseOfUnits("CS", 395),
                                           CS402, CS410, CS418,
                                           CS419ml, CS419opt, CS419gen,
                                           CS431, CS446, CS449, CS451, CS452,
                                           CS453, CS454, CS455, CS456, CS457,
                                           CS464, CS470, CS471, CS472, CS475,
                                           CS476, CourseOfUnits("CS", 499)))),
                 List(
                   UnitsCondition("total", 12),
                   UnitsCondition("at 300/400-level", 6, _.number>=300),
                   UncheckedCondition("Major GPA must be at least 2.0")
                 )
               )
) {
  // val sequence: Seq[Requirements] =
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("CS major and emphasis core requirements", 4,
                       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
      ConditionsViewer(req=12, columns=5)
    )
}

/** Requirements for a CS major (as of 2018) */
object CSmajorEmbeddedEmph2019 extends Program(
  "CS major (emb.\\ sys.)", "CS major (embedded systems emphasis)",
  MTH207, MTH208, CS120, CS220, OneOf(CS225,MTH225), CS270,
  CS272,  CS340,  CS370, CS372, CS421, CS441, CS442, CS472,
  WithConditions("CS major electives",
                 AllSatisfying("CS elective classes",
                               Select("CS elective class", 1,
                                      List(CS202, CS224gen, CS224python, CS272,
                                           PHY335, MTH317, MTH371, CS342,
                                           CS351, CS352, CS353, CS364, CS372,
                                           CourseOfUnits("CS", 395), CS227,
                                           CS402, CS410, CS418,
                                           CS419ml, CS419opt, CS419gen,
                                           CS431, CS446, CS449, CS451, CS452,
                                           CS453, CS454, CS455, CS456, CS464,
                                           CS470, CS471, CS472, CS475, CS476,
                                           CourseOfUnits("CS", 499)))),
                 List(
                   UnitsCondition("total", 6),
                   UnitsCondition("at 300/400-level", 3, _.number>=300),
                   UncheckedCondition("Major GPA must be at least 2.0")
                 )
               )
) {

  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("CS major core requirements", 4,
                       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
      ConditionsViewer(req=14, columns=5)
    )
}

/** Requirements for the 5-year CS BS/MSE (as of 2018) */
object SE5year2018 extends Program(
  "5yr.\\ CS BS/MSE", "5-year Computer Science BS/MSE",
  MTH207, MTH208, CS120, CS220, OneOf(CS225,MTH225),
  CS270, CS340, CS364, CS370, CS421, CS441, CS442,
  CS555, CS741, CS743, CS744, CS746, CourseOfUnits("CS", 798),
  WithConditions("Combined CS major/MSE electives",
                 AllSatisfying("CS elective classes",
                               Select("CS elective class", 1,
                                      List(CS202, CS224gen, CS224python, CS272,
                                           PHY335, MTH317, MTH371, CS342,
                                           CS351, CS352, CS353, CS372,
                                           CourseOfUnits("CS", 395), CS227,
                                           CS402, CS410, CS418,
                                           CS419ml, CS419opt, CS419gen,
                                           CS431, CS446, CS449, CS451, CS452,
                                           CS453, CS454, CS455, CS456, CS464,
                                           CS470, CS471, CS472, CS475, CS476,
                                           CourseOfUnits("CS", 499),
                                           CS502, CS510, CS518,
                                           CS519ml, CS519opt, CS519gen, CS531,
                                           CS543, CS546, CS549, CS551,
                                           CS552, CS553, CS554, CS556,
                                           CS564, CS570, CS571, CS572,
                                           CS575, CS576))),
                 List(
                   ConditionNoteOnly(
                     "CS364 listed above for CS741 prerequisite"),
                   UnitsCondition("total", 15),
                   UnitsCondition("at 300/400 level", 6,
                                  (c) => c.number>=300 && c.number<500),
                   UnitsCondition("at 500/700 level", 9, _.number>=500)
                 )
               )
) {

  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Combined CS major/MSE core requirements", 4,
                       0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                       10, 11, 12, 13, 14, 15, 16),
      new CollectedUnitsViewer("Capstone project", 8, 12, 17),
      ConditionsViewer(req=18, columns=4)
    )
}


/** Requirements for the 5-year CS BS/MSE (last before 2018) */
object SE5yearPre2018 extends Program(
  "5yr.\\ CS BS/MSE", "5-year Computer Science BS/MSE",
  MTH207, MTH208, CS120, CS220, OneOf(CS225,MTH225),
  CS270, CS340, CS370, CS421, CS441, CS442,
  CS555, CS741, CS743, CS744, CS746,
  CourseOfUnits("CS", 798),
  WithConditions("Combined CS major/MSE electives",
                 AllSatisfying("CS elective classes",
                               Select("CS elective class", 1,
                                      List(CS202, CS224gen, CS224python, CS272,
                                      PHY335, MTH317, MTH371, CS227, CS342,
                                      CS351, CS352, CS353, CS364, CS372,
                                      CourseOfUnits("CS", 395),
                                      CS402, CS410, CS418,
                                      CS419ml, CS419opt, CS419gen,
                                      CS431, CS446, CS449, CS451, CS452,
                                      CS453, CS454, CS455, CS456, CS464,
                                      CS470, CS471, CS472, CS475, CS476,
                                      CourseOfUnits("CS", 499),
                                      CS502, CS510, CS518,
                                      CS519ml, CS519opt, CS519gen, CS531,
                                      CS543, CS546, CS549, CS551,
                                      CS552, CS553, CS554, CS556,
                                      CS564, CS570, CS571, CS572,
                                      CS575, CS576))),
                 List(
                   UnitsCondition("total", 15),
                   UnitsCondition("at 300/400 level", 6,
                                  (c) => c.number>=300 && c.number<500),
                   UnitsCondition("at 500/700 level", 9, _.number>=500)
                 )
               )
) {

  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Combined CS major/MSE core requirements", 4,
                       0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                       10, 11, 12, 13, 14, 15),
      new CollectedUnitsViewer("Capstone project", 8, 12, 16),
      ConditionsViewer(req=17, columns=4)
    )
}

/** Requirements for the CS major with CET emphasis (as of 2018) */
object CSmajorCETemph2018 extends Program(
  "CS major (CET emph.)", "Computer Science major (Computer Engineering Technology emph.)",
  MTH207, MTH208, CS120, CS220, OneOf(CS225,MTH225),
  CS270, CS340, CS341, CS370, CS441,
  Select("CS421/442/455/471", 2, List(CS421, CS442, CS455, CS471)),
  WithConditions("CS-CET electives",
                 AllSatisfying("CS-CET electives",
                               Select("CS-CET electives", 1,
                                      List(CS202, CS224gen, CS224python, CS227,
                                      CS272,
                                      PHY335, MTH317, MTH371, CS342,
                                      CS351, CS352, CS353, CS364, CS372,
                                      CourseOfUnits("CS", 395),
                                      CS402, CS410, CS418,
                                      CS419ml, CS419opt, CS419gen,
                                      CS431, CS446, CS449, CS451, CS452,
                                      CS453, CS454, CS456, CS464,
                                      CS470, CS472, CS475, CS476,
                                      CourseOfUnits("CS", 499),
                                      CS502, CS510, CS518,
                                      CS519ml, CS519opt, CS519gen, CS531,
                                      CS543, CS546, CS549, CS551,
                                      CS552, CS553, CS554, CS556,
                                      CS564, CS570, CS571, CS572,
                                      CS575, CS576))),
                 List(
                   UnitsCondition("total", 12),
                   UnitsCondition("at 300/400 level", 6,
                                  (c) => c.number>=300)
                 )
               )
) {

  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("CS (CET emph.) core requirements", 5,
                       0, 1, 2, 3, 4, 5, 6, 7, 8, 9),

      new Viewer {
        override def write(
          implicit doc: LaTeXdoc, who: Person,
          satisfiers: Map[Requirement,List[Achievement]]
        ): Unit = {
          doc ++= """      \begin{tabular}[t]{|c@{~}l@{~~}c@{~}l|}
        \multicolumn{4}{c}{CS (CET emph.) advanced core}
        \\ \hline """
          requirements(10).formatSatisfaction
          doc ++= "        \\\\ \\hline\n      \\end{tabular}\\reqBoxVspace\\\\\n";
        }
      },

      ConditionsViewer(req=11, columns=4)
    )
}


// -----------------------------------------------------------------

/** Additional CSH requirements in lieu of a minor or second major */
object CSonly2018 extends Program(
  "Additional CSH requirements in lieu of minor/2nd major",
  AllWithConditions("CSH requirements in lieu of minor/2nd major",
                    CoursePredicate("Non-CS", (c) => (
                      !c.prefix.equals("CS")
                      && !c.prefix.equals("CT")
                      && !c.prefix.equals("FYS")
                      && (!c.prefix.equals("MTH") || c.number > 225)
                      && c.suffix.equals(None)
                    )),
                    List(
                      UnitsCondition("total", 18),
                      UnitsCondition("at 300/400 level", 12,
                                     (c) => c.number>=300 && c.number<500)
                    ))
) {
  viewAsProgram = false

  override def getSatisfiers(who:Person): Map[Requirement, List[Achievement]] = {
    val result = new HashMap[Requirement, List[Achievement]]
    val checkset = who.makeCheckset

    // Strike out GenEds from this set
    for(requirement <- GenEdAndCHS2018.requirements) {
      requirement.addSatisfiers(who, result, checkset)
    }

    for(requirement <- requirements) {
      requirement.addSatisfiers(who, result, checkset)
    }
    result
  }

  override def viewers: List[Viewer] =
    List(
      ConditionsViewer(req=0, columns=5)
    )
}

// -----------------------------------------------------------------

/** Requirements for a math minor */
object MathMinor2018 extends Program(
  "Math minor", "Mathematics minor",
  MTH207, MTH208, MTH309,
  WithConditions(
    "300/400-level mathematics electives",
    AllSatisfying(
      "300/400-level mathematics electives",
      Select("300/400-level mathematics electives", 1,
             List(OneOf(MTH225,CS225), OneOf(STAT245,MTH245), MTH265,
             MTH151, MTH310, MTH311, MTH317, MTH320, MTH331, MTH353,
             MTH362, MTH371, MTH407, MTH408, MTH411,
             MTH412, MTH415, MTH421, MTH461, MTH480, CS453
           /* MTH395, MTH495, MTH498, MTH499 */))),
    List(
      UnitsCondition("total", 9),
      new SideCondition() {
        def renderSatisfaction(
          achievements: List[Achievement]
        )(implicit doc: LaTeXdoc, who: Person): Unit = {
          doc ++= "MTH151 may apply for up to 3 units only: "
          if (achievements.contains(MTH151)) {
            if (achievements.foldLeft(0)(_+_.units) < 10) {
              doc ++= "\\colorbox{red!30}{No}"
            } else {
              doc ++= "\\colorbox{green}{OK}"
            }
          } else {
            doc ++= "\\colorbox{green}{N/A}"
          }
        }
        override def satisfied(
          achievements: List[Achievement]
        )(implicit who: Person): Boolean = {
          achievements.contains(MTH151) match {
            case true => achievements.foldLeft(0)(_+_.units) < 10
            case false => true
          }
        }
      }
    )
  )
) {

  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Math minor core", 3, 0, 1, 2),
      ConditionsViewer(req=3, columns=3)
    )
}

// -----------------------------------------------------------------

/** Requirements for a minor in computational science (as of 2021) */
object ComputationalMinor2021
extends Program("Cmptnl. sci. min.", "Computational science minor requirements",
                CMP390, CourseOfUnits("CMP", 490, 3),
                Complete(Task("From major", 3)),
                Complete(Task("From non-major", 3)),

                WithConditions(
                  "Introductory sequence in natural science",
                  AllSatisfying(
                    "Introductory natural science sequence",
                    Select("Options", "Options", 1,
                           List(PHY103, PHY104, PHY203, PHY204,
                                ESC101, ESC221, ESC222,
                                BIO100, BIO105, BIO203, BIO210, BIO304,
                                CHM100, CHM103, CHM104))),
                  List(
                    UncheckedCondition("Must be a two-class introductory sequence")
                  )),
                WithConditions(
                  "Additional introductory natural science courses",
                  AllSatisfying(
                    "Additional natural science introductory courses",
                    Select("Options", "Options", 1,
                           List(PHY103, PHY104, PHY203, PHY204,
                                ESC101, ESC221, ESC222,
                                BIO100, BIO105, BIO203, BIO210, BIO304,
                                CHM100, CHM103, CHM104))),
                  List(
                    UncheckedCondition("Two classes, different department than intro. sequence")
                  ))
) {

  val suggestAdvMajor: ScheduleSuggestion =
    new ScheduleSuggestion(
      new DescribedClasses("In-major\\\\adv.\\ elective"),
      UnitsRange(3,5))

  val suggestAdvNonmajor: ScheduleSuggestion =
    new ScheduleSuggestion(
      new DescribedClasses("Non-major \\\\adv.\\ elective"),
      UnitsRange(3,5))

  val suggestOtherNatSci: ScheduleSuggestion =
    new ScheduleSuggestion(
      new DescribedClasses("Other nat.\\ sci."),
      UnitsRange(3,5))

  val suggestOtherNatSciUpper: ScheduleSuggestion =
    new ScheduleSuggestion(
      new DescribedClasses("Other nat.\\ sci.\\\\(300/400-lv.)"),
      UnitsRange(3,5))

  override def viewers: List[Viewer] =
    List(new SimpleViewer("Computational science minor", 2,  0, 1),
         ConditionsViewer(req=4, columns=2),
         ConditionsViewer(req=5, columns=2),
         new SimpleViewer("Advanced (300/400-level) electives", 2,  2, 3))
}

object UWLupperLevelUnits
extends Program("Total units",
                "UWL total unit requirements",
                WithConditions("UWL total unit requirements",
                               AllSatisfying("From all classes",
                                             Select("From all classes", 1,
                                                    List(AnyClass))),
                               List(
                                 UnitsCondition("total", 120),
                                 UnitsCondition("at 300/400-level", 40, _.number>=300)
                               )
                             ))
{
  override def viewers: List[Viewer] = List(ConditionsViewer(req=0, columns=5))
}


/** Requirements for a minor in business administration (as of 2018) */
object BusinessAdminMinor2018
extends Program("Bus. Admin. min.", "Business Administration minor",
                ACC221, ACC222, OneOf(BLAW205,BUS205), ECO110,
                ECO120, FIN355, IS220,
                MGT308, MKT309, OneOf(MTH175,MTH207), OneOf(MTH145,STAT145)
) {

  override def viewers: List[Viewer] =
    List(new SimpleViewer("Business administration minor", 4,
                          0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
}

/** Requirements for a minor in business administration (as of 2018) */
object InfoSystemsMinor2019
extends Program("Info. Sys. min.", "Information Systems minor",
                IS220, IS300, IS310, IS320, OneOf(IS370,IS401),
                OneOf(ACC327, CS120, IS330, IS340, IS360,
                      IS405, IS410, IS420, MGT395)
) {

  override def viewers: List[Viewer] =
    List(new SimpleViewer("Info. Sys. minor", 6,
                          0, 1, 2, 3, 4, 5))
}

/** Requirements for a minor in economics (as of 2021) */
object EconMinor2021
extends Program("Info. Sys. min.", "Information Systems minor",
                ECO110, ECO120,
                WithConditions(
                  "Economics electives",
                  AllSatisfying(
                    "Economics electives",
                    Select("Elective", "Electives", 1,
                           List(ECO301, ECO303, ECO305, ECO306, ECO307, ECO308,
                                ECO310, ECO311, ECO312, ECO320, ECO321, ECO330,
                                ECO336, ECO340, ECO346, ECO350, ECO375, ECO376,
                                ECO400, ECO402, ECO409, ECO435, ECO440,
                                CourseOfUnits("ECO", 450, 1),
                                CourseOfUnits("ECO", 474, 1),
                                CourseOfUnits("ECO", 499, 1)))),
                  List(
                    UnitsCondition("total", 12)
                  ))
) {

  override def viewers: List[Viewer] =
    List(new SimpleViewer("Economics minor core", 2, 0, 1),
         ConditionsViewer(req=2, columns=3))
}

/** Requirements for a minor in astronomy (as of 2018) */
object AstronomyMinor2018
extends Program("Astronomy minor", "Physics minor with astronomy emphasis",
                OneOf(PHY103,PHY203), OneOf(PHY104,PHY204),
                PHY155, PHY160, PHY250, PHY302, PHY363
) {

  override def viewers: List[Viewer] =
    List(new SimpleViewer("Physics minor with Astronomy emphasis", 4,
                          0, 1, 2, 3, 4, 5, 6))
}

/** Requirements for a minor in French (as of 2018) */
object FrenchMinor2018
extends Program("French minor",
                "French minor (FRE395 and FRE220 may not both count)",
                FRE202, FRE300, FRE301, FRE307,
                Select("Connections", "Connections category", 1,
                       List(FRE305, FRE351, FRE395, FRE403)),
                Select("Culture", "Culture category", 1, List(FRE320, FRE322)),
                WithConditions(
                  "French electives",
                  AllSatisfying(
                    "French classes",
                    Select("Elective", "Electives", 1,
                           List(FRE220, FRE317, FRE430, FRE495,
                           CourseOfUnits("FRE", 450, 1),
                           CourseOfUnits("FRE", 498, 1),
                           CourseOfUnits("FRE", 499, 1)))),
                  List(
                    UnitsCondition("total", 3),
                    UncheckedCondition("FRE220 and FRE395 not both counted")
                  ))
) {

  override def viewers: List[Viewer] =
    List(new SimpleViewer("French minor", 3,  0, 1, 2, 3, 4, 5),
         ConditionsViewer(req=6, columns=5))
}

/** Requirements for a minor in French (as of 2018) */
object CreativeWritingMinor2020
extends Program("Creative writing minor", "Creative writing minor",
                ENG305,
                WithConditions(
                  "Creative writing --- writing, language and publishing courses",
                  AllSatisfying(
                    "Writing, language and publishing",
                    Select("Elective", "Electives", 1,
                           List(ENG313, ENG314, ENG317, ENG320, ENG325, ENG326,
                                ENG327, ENG330, ENG331, ENG332, ENG335, ENG337,
                                ENG339, ENG343, ENG416, ENG417, ENG433))),
                  List(UnitsCondition("total", 3) )),
                Select("Forms requirement",
                       "Forms of fiction or poetry", 1,
                       List(ENG446, ENG449)),
                Select("Seminar requirement",
                       "Seminar in fiction or poetry", 1,
                       List(ENG416, ENG417)),
                WithConditions(
                  "Creative writing --- literature electives",
                  AllSatisfying(
                    "Literature",
                    Select("Elective", "Electives", 1,
                           List(ENG301, ENG302, ENG312, ENG342, ENG344,
                                ENG348, ENG349, ENG356, ENG357, ENG361,
                                ENG362, ENG363, ENG364, ENG366, ENG367,
                                ENG368, ENG370, ENG371, ENG372, ENG380,
                                ENG381, ENG382, ENG385, ENG387,
                                CourseOfUnits("ENG", 403, 3),
                                ENG446, ENG449, ENG462, ENG470, ENG481,
                                ENG495))),
                  List(
                    UnitsCondition("total", 6),
                    UnitsCondition("at 400-level", 3, _.number>=400)
                  ))
) {

  override def viewers: List[Viewer] =
    List(new SimpleViewer("Creative Writing minor core", 3,  0, 2, 3),
         ConditionsViewer(req=1, columns=3),
         ConditionsViewer(req=4, columns=3))
}

/** Requirements for a minor in Spanish (as of 2020) */
object SpanishMinor2020
extends Program(
  "Spanish min.", "Spanish minor",
  OneOf(SPA202, SPA221),
  Select("Writing", "Communities/writing proficiency", 2,
         List(SPA307, SPA309, SPA323, SPA333, SPA335)),
  Select("Oral", "Communities/oral proficiency", 2,
         List(SPA302, SPA310, SPA324)),
  WithConditions("Spanish: Identities and perspectives/human conditions",
                 AllSatisfying("Identities and perspectives",
                               Select("Identities and perspectives", 1,
                                      List(SPA352, SPA353, SPA354, SPA355,
                                           SPA369, SPA370, SPA380, SPA381,
                                           SPA382, SPA403, SPA443,
                                           CourseOfUnits("SPA", 450, 3)))),
                 List(
                   UnitsCondition("from Identities and Perspectives group", 6,
                                  inList(SPA352, SPA353, SPA354, SPA355,
                                         SPA369, SPA370, SPA380, SPA381,
                                         SPA382)),
                   UnitsCondition("total", 9)
                 ))
) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Spanish: Experiences in cultural context", 1, 0),
      new SimpleViewer("Spanish: Diversity and social responsibility", 2, 1, 2),
      ConditionsViewer(req=3, columns=5))
}

/** Requirements for a minor in Spanish (as of 2019) */
object SpanishMinor2019
extends Program(
  "Spanish min.", "Spanish minor",
  SPA202,
  Select("Writing", 2, List(SPA307, SPA323, SPA333)),
  Select("Oral", 2, List(SPA302, SPA310, SPA324)),
  WithConditions("Identities and perspectives",
                 AllSatisfying(
                   "Identities and perspectives",
                    Select("Elective", "Electives", 1,
                           List(SPA352, SPA353, SPA354, SPA355, SPA369,
                           SPA370, SPA380, SPA381, SPA382))),
                 List(
                   UnitsCondition("total", 9)
                 )),
  Select("Human conditions", 1,
         List(SPA403, SPA443, CourseOfUnits("SPA", 450, 3)))
) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Spanish minor core", 4, 0, 1, 2, 4),
      ConditionsViewer(req=3, columns=4))
}

/** Requirements for a minor in Spanish (as of 2018) */
object SpanishMinor2018
extends Program(
  "Spanish min.", "Spanish minor",
  SPA300, OneOf(SPA320, SPA321), SPA326, SPA330,
  Select("SPA327/8/9", 1, List(SPA327, SPA328, SPA329)),
  WithConditions("Spanish minor 300/400-level electives",
                 AllSatisfying("300/400",
                               AtLevel("300/400",
                                       "SPA", (x:Int) => x>=300 && x<500)),
                 List(
                   UnitsCondition("total", 6)
                 ))
) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Spanish minor core", 5, 0, 1, 2, 3, 4),
      ConditionsViewer(req=5, columns=3))
}

/** Requirements for a minor in Spanish pre-2018, from WINGS for that
 *  one student
 */
object SpanishMinorOlder
extends Program(
  "Spanish min.", "Spanish minor",
  SPA326, SPA330, SPA300, SPA321, SPA382, SPA331
) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Spanish minor core", 3, 0, 1, 2, 3, 4, 5))
}

/** Requirements for a minor in German (as of 2018) */
object GermanMinor2018
extends Program(
  "German std. min.", "German studies minor",
  GER202,
  Select("Literature", "Literature", 1, List(GER301, GER403, GER406)),
  Select("Civ.", "Civilization", 1, List(GER320, GER321)),
  Select("Skills", "German skills development", 2,
         List(GER300, GER311, GER313, GER330)),
  WithConditions("300/400-level electives",
                 AllSatisfying("300/400-level electives",
                               Select("300/400-level electives", 1,
                                      List(GER300, GER301, GER311, GER313,
                                      GER315, GER320, GER321,
                                      GER326, GER327, GER328, GER329,
                                      GER330, GER351, GER394, GER406, GER495,
                                      CourseOfUnits("GER", 450, 1),
                                      CourseOfUnits("GER", 498, 1),
                                      CourseOfUnits("GER", 499, 1)))),
                 List(
                   UnitsCondition("total", 2)
                 ))
) {
  override def viewers: List[Viewer] =
    List(new Viewer {
      override def write(implicit doc: LaTeXdoc, who: Person,
                         satisfiers: Map[Requirement,List[Achievement]]): Unit = {
    doc ++= """      \begin{tabular}[t]{|c@{~}l@{~~}c@{~}l@{~~}c@{~}l@{~~}c@{~}l@{~~}c@{~}l|}
        \multicolumn{5}{c}{German studies minor core}
        \\ \hline """
    requirements(0).formatSatisfaction
    doc ++= "\n          & "
    requirements(1).formatSatisfaction
    doc ++= "\n          & "
    requirements(2).formatSatisfaction
    doc ++= "\n          & "
    requirements(3).formatSatisfaction
    doc ++= """ \\ \hline
      \end{tabular}\reqBoxVspace\\"""
    }
  },
         ConditionsViewer(req=4, columns=5))
}

object Philosophy2ndMajor2020
extends Program(
  "Phil. 2nd maj.", "Philosophy second major",
  OneOf(PHL100, PHL200), PHL101, OneOf(PHL201, PHL303),
  PHL205, PHL206, PHL496,
  WithConditions(
    "Advanced philosophy electives",
    AllSatisfying("300/400",
                  AtLevel("300/400",
                          "PHL", (x:Int) => (x>=300 && x<500))),
    List(
      UnitsCondition("total", 6)
    )),
  WithConditions(
    "Additional philosophy electives",
    AllSatisfying("Electives",
                  AtLevel("Electives",
                          "PHL", (x:Int) => (x!=494 && x!=495 && x!=497))),
    List(
      UnitsCondition("total", 6)
    ))) {

  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Philosophy core", 3, 0, 1, 2, 3, 4, 5),
      ConditionsViewer(req=6, columns=6),
      ConditionsViewer(req=7, columns=6))
}


/** Requirements for a minor in Chinese Studies (as of 2019) */
object ChineseStudiesMinor2019
extends Program(
  "Chinese studies min.", "Chinese studies minor",
  CHI201, CHI202, OneOf(CHI301, CHI315), OneOf(CHI305, CHI320),
  WithConditions("Chinese studies minor 300/400-level electives",
                 AllSatisfying("Electives",
                               Select(1,
                                      List(CourseOfUnits("CHI", 398, 1),
                                      CHI301, CHI315, CHI305, CHI320,
                                      CHI326, ENG434, HIS327, HIS334, HIS335,
                                      POL333, POL355))),
                 List(
                   UnitsCondition("total", 4)
                 ))
) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Chinese studies minor core", 4, 0, 1, 2, 3),
      ConditionsViewer(req=4, columns=4))
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/** Requirements for a minor in biology (as of 2018) */
object BioMinor2018
extends Program(
  "Biology minor", "Biology minor", BIO105, BIO203,
    WithConditions(
      "Biology minor electives",
      AllSatisfying(
        "Biology minor electives",
        Select(
          "Biology minor electives", 1,
          List(BIO202, BIO204, BIO210,
          CourseOfUnits("BIO", 260),
          BIO299, BIO302, BIO303, BIO304, BIO306, BIO307, BIO312,
          BIO313, BIO315, BIO321, BIO330, BIO333, BIO337, BIO341,
          BIO356, BIO365, BIO390, BIO401, BIO404, BIO405, BIO406,
          BIO408, BIO410, BIO412, BIO413, BIO414, BIO415, BIO419,
          BIO421, BIO422, BIO424, BIO428, BIO429, BIO432, BIO435,
          BIO436, BIO437, BIO439, BIO440, BIO441, BIO442, BIO443,
          BIO444, BIO446, BIO447, BIO449,
          CourseOfUnits("BIO", 450),
          BIO456, BIO458,
          CourseOfUnits("BIO", 460),
          BIO464, BIO465, BIO466, BIO467, BIO468, BIO469, BIO473,
          BIO476, BIO479, BIO488, BIO489,
          CourseOfUnits("BIO", 490),
          BIO491,
          CourseOfUnits("BIO", 495),
          CourseOfUnits("BIO", 499),
          MIC120, MIC230,
          CourseOfUnits("MIC", 260),
          CourseOfUnits("MIC", 299),
          MIC310, MIC350, MIC380, MIC410, MIC420, MIC421,
          MIC427, MIC428, MIC434, MIC440, MIC442,
          CourseOfUnits("MIC", 450),
          MIC454,
          CourseOfUnits("MIC", 460),
          MIC461,
          CourseOfUnits("MIC", 479),
          CourseOfUnits("MIC", 489),
          CourseOfUnits("MIC", 499)))),
    List(UnitsCondition("total", 16),
         UnitsCondition("numbered 302 or above", 1, _.number>=302)))

) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Biology minor core", 4, 0, 1),
      ConditionsViewer(req=2, columns=4))
}

/** Requirements for a major in biology (as of 2020) */
object BioMajor2020
extends Program(
  "Biology major", "Biology major",
  BIO105, BIO203, BIO306, BIO307, BIO315, BIO491, STAT145, CHM103, CHM104,
  Select("Advanced lab component", 1,
         List(BIO302, BIO303, BIO304, BIO312, BIO313, BIO321, BIO333, BIO341,
              BIO365, BIO401, BIO404, BIO405, BIO406, BIO408, BIO410, BIO412,
              BIO413, BIO414, BIO419, BIO422, BIO436, BIO439, BIO440, BIO442,
              MIC440, MIC442, BIO444, BIO447, BIO449, BIO456, BIO458, BIO467,
              BIO468, MIC421)),
  WithConditions(
    "Biology electives",
    AllSatisfying(
      "Biology electives",
      Select(
        "Biology electives", 1,
        List(BIO202, BIO210, BIO302, BIO303, BIO312, BIO313, BIO321, BIO330,
             BIO333, BIO337, BIO341, BIO365, BIO401, BIO404, BIO405, BIO406,
             BIO408, BIO410, BIO412, BIO413, BIO414, BIO415, BIO419, BIO422,
             BIO424, BIO428, BIO429, BIO432, BIO435, BIO437, BIO439, BIO440,
             BIO441, BIO442, BIO443, BIO444, BIO446, BIO447, BIO449, BIO456,
             BIO458, BIO464, BIO465, BIO466, BIO467, BIO468, BIO473, BIO476,
             BIO488, MIC230, MIC310, MIC350, MIC380, MIC410, MIC420, MIC421,
             MIC427, MIC428, MIC434))),
    List(UnitsCondition("total", 16),
         UnitsCondition("at 400 level", 3,
                        (c) => c.number>=400 && c.number<500))),
  OneOf(
    Select("Chemistry Option A", 2, List(CHM300, CHM302)),
    Select("Chemistry Option B", 3, List(CHM303, CHM304, CHM302)),
    Select("Chemistry Option C", 3, List(CHM303, CHM304, CHM305)))

) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Biology major core", 5, 0, 1, 2, 3, 4, 5, 6, 7, 8),
      new SimpleViewer("Lab component", 4, 9),
      new SimpleViewer("Organic chemistry options", 4, 11),
      ConditionsViewer(req=10, columns=5)
    )
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/** Requirements for a minor in psychology for CSH students (as of 2018) */
object PsychMinor2018
extends Program(
  "Psychology minor", "Psychology minor", PSY100, PSY321,
    WithConditions(
      "Psychology minor electives",
      AllSatisfying(
        "Psychology minor electives",
        Select(
          "Psychology minor electives", 1,
          List(NEU200, NEU490, PSY100, PSY200, PSY204, PSY205, PSY212, PSY241,
               CourseOfUnits("ESS", 259), CourseOfUnits("PSY", 259),
               CourseOfUnits("WGS", 259), PSY282, PSY285,
               CourseOfUnits("PSY", 291), /* PHL259, */ PSY301,
          PSY302, PSY305, PSY307, CourseOfUnits("PSY", 308),
               PSY309, PSY315, CourseOfUnits("PSY", 316), PSY318,
          PSY319, PSY320, PSY321, PSY331, PHL301, PSY333, PSY334, PSY343,
          PSY347, /* ART333, */ PSY350, PSY356, PSY357, PSY358, PSY359, PSY360,
          PSY370, PSY376, PSY377, CourseOfUnits("PSY", 391), PSY403, PSY404,
               CourseOfUnits("PSY", 405), PSY406,
          PSY407, ECO350, GEO408, HIS408, POL408, PSY408, SOC408, PSY410,
          /* ERS408, */ PSY415, PSY417, PSY420, PSY415, SOC422, PSY426, PSY430,
          PSY431, PSY432, PSY434, PSY435, PSY436, PSY439, PSY440, PSY441,
          /* ERS422, */ PSY442, ERS442, PSY443, CourseOfUnits("PSY", 450), PSY451, PSY459, PSY461,
          CourseOfUnits("PSY", 481), CourseOfUnits("PSY", 482), PSY485, PSY489, CourseOfUnits("PSY", 491), CourseOfUnits("PSY", 495)
          ))),
      List(UnitsCondition("total", 15),
           UnitsCondition("at 300/400 level", 9, _.number>=300)))
) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Psychology minor core", 4, 0, 1),
      ConditionsViewer(req=2, columns=4))
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/** Requirements for a minor in earth science (as of 2019-20) */
object EarthScienceMinor2019
extends Program(
  "Earth Science minor", "Earth Science minor",
  ESC101, ESC221, ESC222, OneOf(ESC250, GEO250),
  Select("Physical course", 1,
         List(GEO347, ESC347, ARC347, GEO422, ESC422,
              GEO425, ESC425, GEO426, ESC426, GEO427, ESC427,
              GEO428, ESC428, GEO430, ESC430, GEO460, ESC460)),
  Select("Techniques course", 1,
         List(GEO345, ESC345, GEO355, GEO385, ESC385,
              GEO390, ESC390, ESC412, GEO412, GEO440, ESC440,
              GEO445, GEO455, GEO485, GEO488)),
  Select("Elective", 1,
         List(ARC347,
              ESC321, ESC345, ESC347, ESC355, ESC385, ESC390,
              ESC422, ESC425, ESC426, ESC427, ESC428, ESC430,
              ESC440, ESC445, CourseOfUnits("ESC", 450), ESC455, ESC460,
              CourseOfUnits("ESC", 470), CourseOfUnits("ESC", 476),
              CourseOfUnits("ESC", 490), CourseOfUnits("ESC", 495),
              CourseOfUnits("ESC", 499),
              GEO250, GEO345, GEO347, GEO385, GEO390, GEO422, GEO425, GEO426,
              GEO440, GEO445, CourseOfUnits("GEO", 450), GEO455, GEO460,
              CourseOfUnits("GEO", 470), CourseOfUnits("GEO", 476), GEO485,
              CourseOfUnits("GEO", 490), CourseOfUnits("GEO", 495),
              CourseOfUnits("GEO", 499)))
) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Earth science minor core", 4, 0, 1, 2, 3),
      new SimpleViewer("Additional earth science minor coursework", 3,
                       4, 5, 6))
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/** Requirements for a minor in history (as of 2019) */
object HistoryMinor2019
extends Program(
  "History minor", "History minor", HIS200,
  Select("Survey course", 1,
         List(HIS210, HIS220, HIS230, HIS240, HIS250, HIS260, HIS285)),
  WithConditions(
    "History minor electives",
    AllSatisfying(
      "History minor electives",
      Select(
        "History minor electives", 1,
        List(HIS202, ARC204, HIS204, HIS205, HIS210, HIS230, HIS240, HIS250,
        HIS260, HIS285, ARC295, HIS295, CourseOfUnits("HIS", 300), HIS301, WGS301, ARC302,
        HIS302, HIS305, WGS305, HIS306, HIS308, HIS310, HIS311, ANT312,
        HIS312, HIS313, HIS314, HIS315, WGS315, HIS316, HIS317, HIS318,
        HIS319, HIS320, HIS321, HIS322, HIS323, HIS324, HIS325, HIS326,
        HIS327, HIS328, HIS329, HIS330, ARC331, HIS331, ARC332, HIS332,
        HIS333, HIS334, HIS335, HIS336, HIS337, HIS338, HIS339, ARC340,
        HIS340, HIS341, HIS342, HIS343, HIS344, HIS345, HIS346, HIS347,
        HIS348, HIS349, HIS350, HIS351, HIS352, ANT353, ARC353, HIS353,
        HIS354, HIS355, HIS356, HIS357, HIS358, HIS359, HIS360, HIS361,
        HIS362, HIS363, HIS364, ARC365, HIS365, ARC366, HIS366, ARC367,
        HIS367, ARC368, HIS368, ARC369, HIS369, ARC372, HIS372, HIS373,
        ARC374, HIS374, ARC375, HIS375, HIS377, HIS378, HIS379, HIS380,
        HIS381, HIS382, HIS383, HIS384, HIS385, HIS386, HIS387, HIS388,
        HIS389, HIS390, HIS391, HIS392, HIS393, HIS394, HIS395, ARC396,
        HIS396, HIS397, HIS398, HIS399, HIS401, HIS405, HIS406, HIS407,
        ECO408, GEO408, HIS408, POL408, PSY408, SOC408, ERS409, HIS409,
        ERS411, HIS411, HIS413, HIS414, HIS415, HIS418,
             CourseOfUnits("HIS", 450), CourseOfUnits("HIS", 497)
      ))),
    List(UnitsCondition("total", 12),
         UnitsCondition("at 300/400 level", 9, _.number>=300)))
) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("History minor core", 1, 0, 1),
      ConditionsViewer(req=2, columns=4))
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/** Requirements for a minor in history (as of 2019) */
object EnvStudiesLists {
  val natural = List(BIO307, BIO341, BIO441, BIO464, BIO473, BIO476,
                     CHM412,
                     ESC101, ESC211,
                     GEO200, GEO425, GEO427, GEO460,
                     MIC350, MIC434)
  val social = List(ARC404, ECO346, ESC321, PH335, PSY302, PUB338,
                    REC306, REC345, SOC311, SOC328, SOC332, ERS363, SOC363)
  val arts = List(ENG387, HIS317, HIS321, HIS338, HIS379, PHL341, PHL425)
}
object EnvironmentalStudiesMinor2021
extends Program(
  "Environmental studies minor", "Environmental studies minor",
  ENV201, ENV301, ENV303, ENV496,
  Select("Nat.\\ sci.", 1,
         List(BIO307, BIO341, BIO441, BIO464, BIO473, BIO476,
              CHM412, ESC101, ESC211, GEO200, GEO425, GEO427, GEO460,
              MIC350, MIC434)),
  Select("Soc.\\ sci.", 1,
         List(ARC404, ECO346, ESC321, PH335, PSY302, PUB338,
              REC306, REC345, SOC311, SOC328, SOC332, ERS363, SOC363)),
  Select("A\\&H", 1,
         List(ENG387, HIS317, HIS321, HIS338, HIS379, PHL341, PHL425)),
  Select("Elective", 1,
         List(BIO307, BIO341, BIO441, BIO464, BIO473, BIO476,
              CHM412, ESC101, ESC211, GEO200, GEO425, GEO427, GEO460,
              MIC350, MIC434,
              ARC404, ECO346, ESC321, PH335, PSY302, PUB338,
              REC306, REC345, SOC311, SOC328, SOC332, ERS363, SOC363,
              ENG387, HIS317, HIS321, HIS338, HIS379, PHL341,
              CourseOfUnits("ENV", 450), CourseOfUnits("ENV", 499)))
) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Environmental studies core", 4,
                       0, 1, 2, 3, 4, 5, 6, 7)
    )
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/** Requirements for a minor in women's studies (as of 2019) */
object WomensStudiesMinor2019
extends Program(
  "Women's studies minor", "Women's studies minor",
  OneOf(WGS100, WGS130, WGS150),
  WithConditions(
    "Women's studies minor category II",
    AllSatisfying(
      "Women's studies minor category II",
      Select(
        "Women's studies minor category II", 1,
        List(WGS105, SOC105,
        WGS130,
        WGS212,
        WGS225,
        CourseOfUnits("WGS", 259), CourseOfUnits("ESS", 259),
             CourseOfUnits("PSY", 259), CourseOfUnits("WGS", 300),
        WGS305, HIS305,
        WGS308,
        WGS315, HIS315,
        WGS316, SOC316,
        WGS320,
        WGS321,
        WGS322,
        WGS325,
        CourseOfUnits("WGS", 330),
        WGS333,
        WGS337, SOC337,
        WGS340,
        WGS373,
        WGS374,
        WGS375, SOC375,
        CourseOfUnits("WGS", 450)
      ))),
    List(UnitsCondition("total", 9),
         UnitsCondition("at 300/400 level", 6, _.number>=300))),
  WithConditions(
    "Women's studies minor category III",
    AllSatisfying(
      "Women's studies minor category III",
      Select(
        "Women's studies minor category III", 1,
        List(ANT250, ANT323, ARC372, CST334, CST338, CST419,
             ECO336, ENG220, ENG385, ENG482, HED412, HED472,
             HIS301, WGS301,
             HIS305, WGS305,
             HIS315, WGS315,
             HIS359, HIS360, HIS372, HIS383, HIS386, HIS389,
             PHL342, POL205, POL433, POL436, POL437, POL439,
             PSY305, PSY318, PSY319, SAH307,
             SOC105, WGS105,
             SOC316, WGS316,
             SOC337, WGS337,
             SOC338, SOC369, SOC370,
             SOC375, WGS375, WGS375
      ))),
    List(UnitsCondition("total", 6))),
  WGS499
) {
  override def viewers: List[Viewer] =
    List(
      new SimpleViewer("Women's studies minor core", 4, 0),
      ConditionsViewer(req=1, columns=4),
      ConditionsViewer(req=2, columns=4),
      new SimpleViewer("Women's studies seminar", 4, 3))
}

// -----------------------------------------------------------------

/** Combination of UWL general education requirements and CSH degree requirements (as of 2018) */
object GenEdAndCHS2018
extends Program(
  "Gen. eds.", "General education and CSH requirements",
  CST110, GE01_ENG_2018, GE03_2018, GE04_HST_2018, GE04_GMS_2018,
  CHS_LabSci_2018, GE06_2018, GE07_2018, GE08_2018, GE09_2018
) {
  viewAsProgram = false
  override def viewers: List[Viewer] =
    List(new Viewer {
      override def write(implicit doc: LaTeXdoc, who: Person,
                         satisfiers: Map[Requirement,List[Achievement]]): Unit = {
    doc ++= """      \begin{tabular}[t]{|c@{~}l@{~~}c@{~}l@{~~}c@{~}l@{~~}c@{~}l|}
        \multicolumn{8}{c}{"""
    doc ++= longName
    doc ++= """}
        \\ \hline """
    GE01_ENG_2018.formatSatisfaction
    doc ++= "\n          & "
    GE04_GMS_2018.formatSatisfaction
    doc ++= "\n          & "
    CHS_LabSci_2018.formatSatisfaction
    doc ++= "\n         \\\\ "
    requirements(0).formatSatisfaction
    doc ++= "\n          & "
    GE07_2018.formatSatisfaction
    doc ++= "\n          & "
    GE08_2018.formatSatisfaction
    doc ++= "\n         \\\\ "
    GE03_2018.formatSatisfaction
    doc ++= "\n          & "
    GE04_HST_2018.formatSatisfaction
    doc ++= "\n          & "
    GE06_2018.formatSatisfaction
    doc ++= "\n          & "
    GE09_2018.formatSatisfaction

    // $self->comingElectivesTabularList($who, $dest, @currentElectives, 8);

    doc ++= "        \\\\ \\hline\n      \\end{tabular}\\reqBoxVspace\\\\\n";
    }
  })
}

/** Combination of UWL general education requirements and CSH degree requirements (as of 2019) */
object GenEdAndCHS2019
extends Program(
  "Gen. eds.", "General education and CSH requirements",
  CST110, GE00_FYS_2019, GE01_ENG_2018, GE03_2018, GE04_HST_2018,
  GE04_GMS_2018,
  CHS_LabSci_2018, GE06_2018, GE07_2018, GE08_2018, GE09_2018
) {
  viewAsProgram = false
  override def viewers: List[Viewer] =
    List(new Viewer {
      override def write(implicit doc: LaTeXdoc, who: Person,
                         satisfiers: Map[Requirement,List[Achievement]]): Unit = {
    doc ++= """      \begin{tabular}[t]{|c@{~}l@{~~}c@{~}l@{~~}c@{~}l@{~~}c@{~}l|}
        \multicolumn{8}{c}{"""
    doc ++= longName
    doc ++= """}
        \\ \hline """
    GE00_FYS_2019.formatSatisfaction
    doc ++= "\n          & "
    GE04_GMS_2018.formatSatisfaction
    doc ++= "\n          & "
    CHS_LabSci_2018.formatSatisfaction
    doc ++= "\n         \\\\ "
    GE01_ENG_2018.formatSatisfaction
    doc ++= "\n          & "
    GE07_2018.formatSatisfaction
    doc ++= "\n          & "
    GE08_2018.formatSatisfaction
    doc ++= "\n         \\\\ "
    requirements(0).formatSatisfaction
    doc ++= "\n          & "
    GE04_HST_2018.formatSatisfaction
    doc ++= "\n          & "
    GE06_2018.formatSatisfaction
    doc ++= "\n          & "
    GE09_2018.formatSatisfaction
    doc ++= "\n         \\\\ "
    GE03_2018.formatSatisfaction
    doc ++= "\n          & & & & & &"

    // $self->comingElectivesTabularList($who, $dest, @currentElectives, 8);

    doc ++= "        \\\\ \\hline\n      \\end{tabular}\\reqBoxVspace\\\\\n";
    }
  })
}

object CHS2018 extends Program(
  "CSH requirements", "College of Science and Health requirements",
  CHS_LabSci_2018
) {
  viewAsProgram = false
  override def viewers: List[Viewer] =
    List(new Viewer {
      override def write(implicit doc: LaTeXdoc, who: Person,
                         satisfiers: Map[Requirement,List[Achievement]]): Unit = {
    doc ++= """      \begin{tabular}[t]{|c@{~}l@{~~}c@{~}l|}
        \multicolumn{4}{c}{"""
    doc ++= name
    doc ++= """}
        \\ \hline """
    CHS_LabSci_2018.formatSatisfaction
    doc ++= "\n        \\\\ \\hline\n      \\end{tabular}\\reqBoxVspace\\\\\n";
    }
  })
}


/**
 * Combination of UWL general education requirements and CSH degree
 * requirements.
 */
object GenEdAndCHS2020
extends Program(
  "Gen. eds.", "General education and CSH requirements",
  CST110, GE00_FYS_2019, GE01_ENG_2018, GE03_2018, GE04_HST_2018,
  GE04_GMS_2018,
  CHS_LabSci_2018, GE06_2018, GE07_2018, GE08_2018, GE09_2018
) {
  viewAsProgram = false
  override def viewers: List[Viewer] =
    List(new Viewer {
      override def write(
        implicit doc: LaTeXdoc, who: Person,
        satisfiers: Map[Requirement,List[Achievement]]
      ): Unit = {
        doc ++= """      \begin{tabular}[t]{|c@{~}l@{~~}c@{~}l@{~~}c@{~}l@{~~}c@{~}l|}
        \multicolumn{8}{c}{"""
        doc ++= longName
        doc ++= """}
        \\ \hline """
        GE00_FYS_2019.formatSatisfaction
        doc ++= "\n          & "
        GE04_GMS_2018.formatSatisfaction
        doc ++= "\n          & "
        CHS_LabSci_2018.formatSatisfaction
        doc ++= "\n         \\\\ "
        GE01_ENG_2018.formatSatisfaction
        doc ++= "\n          & "
        GE07_2018.formatSatisfaction
        doc ++= "\n          & "
        GE08_2018.formatSatisfaction
        doc ++= "\n         \\\\ "
        requirements(0).formatSatisfaction
        doc ++= "\n          & "
        GE04_HST_2018.formatSatisfaction
        doc ++= "\n          & "
        GE06_2018.formatSatisfaction
        doc ++= "\n          & "
        GE09_2018.formatSatisfaction
        doc ++= "\n         \\\\ "
        GE03_2018.formatSatisfaction
        doc ++= "\n          & & & & & &"

        // $self->comingElectivesTabularList($who, $dest, @currentElectives, 8);

        doc ++= "        \\\\ \\hline\n      \\end{tabular}\\reqBoxVspace\\\\\n";
      }
    })
}

private[programs]
object CHS_LabSci_2018 extends Select(
  "Lab.\\ sci.", "CHS lab science", 2, //
  List(ANT102, BIO100, BIO105, CHM100, CHM103, ESC101, MIC100,
       PHY103, PHY106, PHY125, PHY155, PHY160, PHY203,
       BIO203, BIO210, BIO304, CHM104, ESC221, ESC222, PHY104, PHY204
     )
)

private[programs]
object GE00_FYS_2019
extends Select("FYS\\,100", "FYS\\,100", 1, List(FYS100))

private[programs]
object GE01_CST_2018
extends Select("CST\\,110", "CST\\,110", 1, List(CST110))

private[programs]
object GE01_ENG_2018
extends Select("ENG\\,110", "ENG\\,110 or ENG\\,112", 1, List(ENG110, ENG112))

private[programs]
object GE03_2018
extends Select(
  "MC/MRS", "Minority cultures or multiracial women's studies (GE 03)", 1,
  List(ANT362, ERS362,
  // ANT365,  // Missing?
  ECO336, EDS206, EFN205,
  ENG207, ENG210, ENG215, ERS207, ERS210, ERS215, ERS253,
  HIS306, HIS336, MUS209, PHL335, POL205, PSY285, PSY318, SAH307,
  SOC225, WGS100, WGS130, ERS100,
  // TSL200,  // Effectively not an option it is the only 1-credit course

  // Transfer credits
  HasSuffix("Lower-division GE3 transfer course", "Y"),
  HasSuffix("Upper-division GE3 transfer course", "Z")
))

private[programs]
object GE04_HST_2018
extends Select("World hist.", "World history (GE 04a)", 1,
               List(ARC200, HIS101, HIS102, HIS110,

               // Transfer credits
               HasSuffix("Lower-division GE4 transfer course", "R"),
               HasSuffix("Upper-division GE4 transfer course", "S")))

private[programs]
object GE04_GMS_2018
extends Select(
  "Gl\\ \\& Mtc.\\ Std.", "Global and multicultural studies (GE 04a)", 1,
  List(ANT202, ECO202, GEO202, HIS202, POL202, SOC202, ANT212, ART301, CHI320,
  ECO120, ECO212, ENG208, ENG212, ENV201, FRE220, GEO110, GEO200, HIS101,
  HIS102, HIS360,
  // new Select("INS250/251/252", 3, INS250,INS251,INS252), // Missing?
  MIC130,
  // MLG304, // Missing?
  MUS204, MUS205, PHL212, PHL336, PHL349, PHY142, POL212,
  POL234, POL244, PSY282, THA351, WGS212,

  // Transfer credits
  HasSuffix("Lower-division GE4 transfer course", "M"),
  HasSuffix("Upper-division GE4 transfer course", "N")))

private[programs]
object GE05_2018 extends Select(
  "Science", "Science: understanding the natural world (GE 05)", 1,
  List(ANT102, BIO100, BIO105, CHM100, CHM103, ESC101, MIC100,
  PHY103, PHY106, PHY125, PHY155, PHY160, PHY203,

  // Transfer credits.  Note that we match lab courses only.
  HasSuffix("Lower-division GE5 (lab) transfer course", "A")
))

private[programs]
object GE06_2018 extends Select(
  "Self \\& soc.",
  "Self and society: understanding oneself and the social world (GE 06)", 1,
  List(ANT101, ARC100, ART251, CST271, ECO110, ECO376, THA376, EDS203, ENG220,
       ERS220, FIN207, GEO102, GER398, PH200, PHL120, POL101, POL102, PSY100,
       SOC110, SOC120, SOC261, THA130, SOC150, SOC150, WGS150,

       // Transfer credits
       HasSuffix("Lower-division GE6 transfer course", "C"),
       HasSuffix("Upper-division GE6 transfer course", "D")
))

private[programs]
object GE07_2018 extends Select(
  "Humanistic",
  "Humanistic studies: the search for values and meaning (GE 07)", 1,
  List(PHL100, CHI305, ENG200, ENG201, ENG202, ENG203, ENG204,
  ENG205, ENG206, FRE395, GER399, HIS205, MLG299, PHL200, POL251, RUS305,

  // Transfer credits
  HasSuffix("Lower-division GE7 transfer course", "E"),
  HasSuffix("Upper-division GE7 transfer course", "F")
))

private[programs]
object GE08_2018 extends Select(
  "Arts", "Arts: the aesthetic experience (GE 08)", 2,
  List(GE08_ART_2018, ESS104, GE08_MUS_2018, PHL332, GE08_THA_2018,

  // Transfer credits --- TODO might need to separate by department
  HasSuffix("Lower-division GE8 transfer course", "H"),
  HasSuffix("Upper-division GE8 transfer course", "O")
))

private[programs]
object GE08_ART_2018 extends Select(
  "Art classes", "Art classes", 1,
  List(ART102, ART160, ART172, ART272, ART302, ART332
))

private[programs]
object GE08_MUS_2018 extends Select(
  "Music classes", "Music classes", 1, List(MUS105, MUS110, MUS317
))

private[programs]
object GE08_THA_2018 extends Select(
  "Theater classes", "Theater classes", 1, List(THA110, THA120, THA201
))

private[programs]
object GE09_2018 extends Select(
  "Health",
  "Health and physical well-being: learning to create healthy lives (GE 09)",
  1, List(HED207, HP105, HPR105,

  // Transfer credits
  HasSuffix("Lower-division GE9 transfer course", "K")
))
