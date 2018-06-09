package utils

object CommandTypes extends Enumeration {
  type CommandType = Value
  val List_, DeletePoll, StartPoll, StopPoll, Result, CreatePoll,
    Begin, End, View, AddQuestion, DeleteQuestion, Answer = Value
}
