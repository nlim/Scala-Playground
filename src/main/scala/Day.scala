import java.util.Date
import java.util.Calendar

object Day {
  def nextBusinessDay(d: Date): Date = {
    val c = Calendar.getInstance
    c.setTime(d)
    val n = c.get(Calendar.DAY_OF_WEEK) match {
      case Calendar.FRIDAY => 3
      case Calendar.SATURDAY => 2
      case _ => 1
    }
    c.roll(Calendar.DAY_OF_YEAR, n)
    c.getTime
  }
}
