import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

case class Poll(id: Int, name: String, anonymity: Option[Boolean] = None, afterStop: Option[Boolean] = None,
           startDate: Option[Date] = None, endDate: Option[Date] = None) {

  private val format = new SimpleDateFormat("hh:mm:ss yy:MM:dd")

  private var isRunning = false

  def currentDate: Date = format.parse(format.format(Calendar.getInstance().getTime))

  def running: Boolean = {
    val now = currentDate
    val startTime = startDate.getOrElse(now)
    val endTime = endDate.getOrElse(now)
    if (now.compareTo(startTime) >= 0 && now.compareTo(endTime) <= 0)
      true
    else
      false  }

  def startPoll: String = {
    isRunning = true
    s"Poll '$name' (id $id) started"}

  def stopPoll: String = {
    isRunning = false
    s"Poll '$name' with (id $id) stopped"
  }

  def getResult: String = {
    this.toString
  }

  override def toString = {
    s"""id: $id
       |name: $name
       |anonymity: ${anonymity.getOrElse("undefined")}
       |poll type: ${afterStop.getOrElse("undefined")}
       |start date: ${startDate.getOrElse("undefined")}
       |end date: ${endDate.getOrElse("undefined")}
       |in process: ${isRunning.toString}\n\n""".stripMargin
  }
}