package utils

import java.util.Date

case class PollParams(
  name: String,
  anonymity: Option[Boolean] = Option(true),
  afterStop: Option[Boolean] = Option(true),
  startDate: Option[Date] = None,
  endDate: Option[Date] = None) {

  def unpack: (String, Option[Boolean], Option[Boolean], Option[Date], Option[Date]) = {
    (name, anonymity, afterStop, startDate, endDate)
  }
}
