
package advisobot.core

object Settings {
  var courseDigits = 3

  lazy val courseNumberFormatter: java.text.NumberFormat =
    new java.text.DecimalFormat("0" * courseDigits)
}
