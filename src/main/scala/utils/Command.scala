package utils

case class Command(
  cmdType: CommandTypes.CommandType,
  pollID: Option[Int] = None,
  pollParams: Option[PollParams] = None,
  questionID: Option[Int] = None,
  questionParams: Option[QuestionParams] = None,
  answer: Option[String] = None
)
