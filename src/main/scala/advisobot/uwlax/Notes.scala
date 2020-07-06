package uwlcs.advisobot
import advisobot.builder._

object Notes {
  import org.maraist.outlines.{Outline,OutlineItem}
  import org.maraist.outlines.OutlineItem.{summarized,item}

  /** Short message about checking for errors. */
  val CHECK_INFO_SHORT = "Please check the information on this form to make sure that it is correct.  If you find any errors, please let me know so that I can correct it."

  /** Long message about checking for errors, with sublist of particulars. */
  val CHECK_INFO = item(CHECK_INFO_SHORT + "  Please check in particular:",
                      "Your majors and minors",
                      "The list of classes (on the second page) which you have taken, and your results in them",
                      "The classes you are currently taking this semester.  Have you dropped any which I have not noticed (or do you plan to do so)?")

  /** When I will remove the advising hold. */
  val REMOVING_HOLD = "Let me know if you have other questions about your next steps.  I do not believe I have any further questions or concerns, and will be removing the advising hold from your account in the next few days."

  /** When I will remove the advising hold, and also note that they are doing well. */
  val WELL_REMOVING_HOLD = item(
    "Let me know if you have other questions about your next steps.  From what I see, you have been choosing classes well, and doing well in them.",
    "I do normally prefer in-person advising, but under the current circumstances I think we can skip ours this semester, so I will be removing the advising hold from your account in the next few days.",
    "However, if you have questions and do want to have a video meeting, of course I am happy to do so, and in the next few days will be sending out instruction for scheduling an appointment.")

  /** Expect a telecon before removing the advising hold. */
  val REQUIRE_TELECON = "We should meet to go over your progress this semester, and to discuss this course plan.  I will be in touch in the next few days to arrange a teleconference."

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

  /** After below-15-unit semesters. */
  val EXPECT_HEAVY_SEMESTER = "One of the requirements for graduation is to complete 120 units of coursework, which averages over four years to 15 units per semester.  Since you have had a few semesters with fewer than 15 units/repeated classes, you should expect to have a heavy semester or two coming up.  Alternatively, you could look for classes to take over summers or J-terms."

  /** Talk to Carla a semester or two before graduation. */
  val GRAD_AUDIT = "The official word on satisfying the requirements for graduation is Carla Burkhardt, in the CSH dean's office.  A semester or so before you hope to graduate, you should email her to ask for a \\emph{graduation audit}.  Let her know what classes you plan to take between when you email, and when you hope to graduate."

  /** Bloody get going for a four-year completion. */
  val START_120 = "It is time to start taking classes in your major.  It takes time to satisfy the CS requirements, and continuing to wait to take CS120 may make it hard to graduate in four years.  Contact the professor of the CS120 section in which you would prefer to enroll to discuss your situation."

  /** Variation for new change-of-majors. */
  val START_120_MAJOR_CHANGE = "It is particularly important to start taking classes in the CS major.  During the next two semesters you will be taking the introductory sequence, which limits you to a few CS classes per semester.  Make sure you stick to the recommended CS classes for each semester, since there is a firm prerequisite structure at the beginning of the major.  If a class you need is full, contact the professor of the section in which you would prefer to enroll to discuss your situation, and be flexible about registering in other sections."
}
