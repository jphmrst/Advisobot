package uwlcs.advisobot
import scala.collection.Map
import scala.collection.mutable.HashMap
import org.maraist.latex.LaTeXdoc
import advisobot.core.{Program,Requirement,Select,Person,Grade,Term,
                       Achievement,SideCondition,Viewer,CoursePredicate,
                       ScheduleSuggestion, UnitsRange}
import advisobot.builder._

// -----------------------------------------------------------------

abstract class Advisees(people:Person*)
extends advisobot.core.Advisees(people:_*) {
}
