package uwlcs.advisobot
import advisobot.builder._

object Notes {
  import org.maraist.outlines.{Outline,OutlineItem}
  import org.maraist.outlines.OutlineItem.{summarized,item}

  /** Short message about checking for errors. */
  val CHECK_INFO_SHORT = "Please double-check the information on this form against WINGS to make sure that it is correct.  If you find any errors, please let me know so that I can correct it."

  /** Long message about checking for errors, with sublist of particulars. */
  val CHECK_INFO = item(CHECK_INFO_SHORT + "  Please check in particular:",
                      "Your majors and minors",
                      "The list of classes (on the second page) which you have taken, and your results in them",
                      "The classes you are currently taking this semester.  Have you dropped any which I have not noticed (or do you plan to do so)?")

  /** When I will remove the advising hold. */
  val REMOVING_HOLD = "Let me know if you have other questions about your next steps.  I do not believe I have any further questions or concerns, and will be removing any advising hold that the Records Office places on your account."

  /** When I will remove the advising hold, and also note that they are doing well. */
  val WELL_REMOVING_HOLD = item(
    "Let me know if you have other questions about your next steps.  From what I see, you have been choosing classes well, and doing well in them.",
    "I normally require an advising meeting, but I think we can skip ours this semester.  So I will be removing any advising hold from your account when the Records Office adds them.",
    "However, if you have questions and do want to have a video meeting, of course I am happy to do so, and will be sending an email soon about scheduling an appointment.")

  /** When I will remove the advising hold, and note the rest is obvious. */
  val CLEAR_REMOVING_HOLD = item(
    "The classes you need to take at this point are clear.  So I think we can skip an advising meeting this semester, and will remove any advisor's hold on your registration when the Records Office adds them.  However if you have questions and do want to have a video meeting, then of course I am happy to do so, and will be sending an email soon about scheduling an appointment.")

  /** Expect a telecon before removing the advising hold. */
  val REQUIRE_TELECON = "We should meet to go over your progress this semester, and to discuss this course plan.  I will be sending an email soon about scheduling an appointment."

  /** Check the total unit on WINGS because of retakes. */
  val RETAKES_SO_WINGS = "You have retaken one or more classes, so the total units on this page may be an overestimate.  Check with WINGS to make sure you are on track for the 120-unit graduation requirement."

  /** When WINGS shows academic warning for previous semester. */
  val ACAD_WARNING = item(
    "There is a note on WINGS that you are on Academic Warning status.",
    "Triggered by a GPA below 2.0 the previous semester.",
    "When we meet, we'll touch base about your progress this semester, and making sure you move off of warning status next semester.")

  /** When WINGS does not yet show academic warning for previous semester (sometimes in summer). */
  val ACAD_WARNING_EXPECTED = item(
    "Looking at your transcript from last semester, I expect that you will be placed on Academic Warning status.",
    "Triggered by a GPA below 2.0 the previous semester.",
    "When we meet, we'll touch base about your progress this semester, and making sure you move off of warning status next semester.")

  /** When WINGS shows academic probation for previous semester. */
  val ACAD_PROBATION = item(
    "There is a note on WINGS that you are on Academic Probation.",
    "Triggered by a cumulative GPA below 2.0.",
    "When we meet, we'll touch base about your progress this semester, and making sure you are on track to move off of probationary status.")

  /** When they are going to graduate. */
  val GRADUATION = "It looks like you are finished, and will be graduating at the end of the semester.  Congratulations!  I am very happy for you, and I hope to see you walk at the ceremony.  Please do stay in touch with the department.  Becky keeps track of where our alumni go, and what sort of work they are doing.  And I would be interested in hearing what you will be doing as well.  I wish you all the best for your future!"

  /** Check with minor department about offering patterns. */
  val ASK_MIN_SCHED = "You should check with your minor department about the order and scheduling of their classes.  Some classes may not be offered every semester, may have a recomended order, etc."

  /** Check with minor department about offering patterns. */
  val GENED_WAIT = "As you pick your classes, I would suggest that you wait to take more general education classes until later in your degree.  These classes tend to be less demanding than the upper-level classes you will take for your major, and it can be nice to have a few gen.-eds.\\ for the later semesters."

  /** After below-15-unit semesters. */
  val EXPECT_HEAVY_SEMESTER = "One of the requirements for graduation is to complete 120 units of coursework, which averages over four years to 15 units per semester.  Since you have had a few semesters with fewer than 15 units/repeated classes, you should expect to have a heavy semester or two coming up.  Alternatively, you could look for classes to take over summers or J-terms."

  /** Talk to Carla a semester or two before graduation. */
  val GRAD_AUDIT = "The official word on satisfying the requirements for graduation is Carla Burkhardt, in the CSH dean's office.  A semester or so before you hope to graduate, you should email her to ask for a \\emph{graduation audit}.  Let her know what classes you plan to take between when you email, and when you hope to graduate."

  /** Bloody get going for a four-year completion. */
  val START_120 = "It is time to start taking classes in your major.  It takes time to satisfy the CS requirements, and continuing to wait to take CS120 may make it hard to graduate in four years.  Contact the professor of the CS120 section in which you would prefer to enroll to discuss your situation."

  /** Think about a minor. */
  val CONSIDER_MINOR = item(
    "The College of Science and Health requires all students with no minor or second major to take eighteen units outside of Computer Science.  You should start taking some of those courses now.  Twelve of the eighteen units must be upper-level, so you may need some time to satisfy their prerequisites.",
    "Alternatively, you might think about a minor that would interest you, and which would not require too many additional courses.  The 18-unit requirement goes away when you have declared a minor or second major with the Dean's office.")

  /** Think about a minor, with caveats for a compressed schedule in which there is little time for prerequisites for required upper-level electives. */
  val CONSIDER_MINOR_SHORT_TIME_FOR_UPPERS = item(
    "The College of Science and Health requires all students with no minor or second major to take eighteen units outside of Computer Science.",
    "Twelve of these eighteen units must be at the 300/400-level.  It will take some thought to find good candidates for these courses which do not require too many additional classes for below-300 level prerequisites.",
    "Alternatively, you might think about a minor that would interest you, and which would not require too many additional courses.  The 18-unit requirement goes away when you have declared a minor or second major with the Dean's office.")

  /** Think about a math minor, since you've already taken MTH309. */
  val CONSIDER_MATH_MINOR_309 = item(
    "The College of Science and Health requires all students with no minor or second major to take eighteen units from outside of CS.  However, the 18-unit requirement goes away when you have declared a minor or second major with the Dean's office.  With MTH\\,309, you will have most of the requirements for a math minor satisfied.  So by declaring a math minor, you would have more flexibility in your choice of classes.")

  /** Variation for new change-of-majors. */
  val START_120_MAJOR_CHANGE = "It is particularly important to start taking classes in the CS major.  During the next two semesters you will be taking the introductory sequence, which limits you to a few CS classes per semester.  Make sure you stick to the recommended CS classes for each semester, since there is a firm prerequisite structure at the beginning of the major.  If a class you need is full, contact the professor of the section in which you would prefer to enroll to discuss your situation, and be flexible about registering in other sections."
}
