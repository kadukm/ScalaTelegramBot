import utils.{Command, PollParams, QuestionTypes, QuestionParams}
import utils.CommandTypes._

import scala.collection.immutable.Map


object CommandsParser extends SimpleParsers {

  private def createPollParser(): Parser[Command] =
    ("/" ~> "create_poll" ~ word ~ anonymity.? ~ visibility.? ~ date.? ~ date.?) ^^ {
      case _ ~ name ~ an ~ v ~ start ~ end => Command(CreatePoll, pollParams = Some(PollParams(name, an, v, start, end)))
    }

  private def commandsWithIdParser(): Parser[Command] =
    "/" ~> commandsWithId ~ number ^^ {
      case cmd ~ pollID => cmd match {
        case "start_poll" => Command(StartPoll, Option(pollID))
        case "stop_poll" => Command(StopPoll, Option(pollID))
        case "delete_poll" => Command(DeletePoll, Option(pollID))
        case "result" => Command(Result, pollID = Option(pollID))
      }
    }

  private def commandsWithoutIdParser(): Parser[Command] =
    "/" ~> commandWithoutId ^^ {
      case "list" => Command(List_)
    }

  private def commandsWithoutContextParser() =
    commandsWithIdParser() | commandsWithoutIdParser() | createPollParser()

  private def beginParser(): Parser[Command] =
    "/begin" ~> number ^^ (pollID => Command(Begin, Option(pollID)))

  private def contextCommandsParser(): Parser[Command] =
    "/" ~> contextWithoutIdParser ^^ {
      case "end" => Command(End)
      case "view" => Command(View)
    }

  private def deleteQParser(): Parser[Command] =
    "/delete_question" ~> number ^^ (questionID => Command(DeleteQuestion, questionID = Option(questionID)))

  private def optionsQuestionParser(): Parser[Command] =
    "/add_question" ~> questionArgumentParser ~ questionTypeParser ~ optionsParser ^^ {
      case question ~ qType ~ options =>
        Command(AddQuestion, questionParams = Option(QuestionParams(question, qType, options)))
    }

  private def openQuestionParser(): Parser[Command] =
    "/add_question" ~> questionArgumentParser ~ questionTypeParser.? ^^ {
      case question ~ qType =>
        Command(AddQuestion, questionParams = Option(QuestionParams(question, qType.getOrElse(QuestionTypes.Open), List.empty)))
    }

  private def answerToQuestionParser(): Parser[Command] =
    "/answer" ~> number ~ "(?s).+".r ^^ {
      case questionID ~ qAnswer => Command(Answer, questionID = Option(questionID), answer = Option(qAnswer))
    }

  private def commandsWithContextParser() =
    beginParser() | contextCommandsParser() | optionsQuestionParser() |
      openQuestionParser() | deleteQParser() | answerToQuestionParser()

  private def fullParser(): Parser[Command] =
    commandsWithoutContextParser() | commandsWithContextParser()

  def parseCmd(cmd: String): Option[Command] = {
    parse(fullParser(), cmd) match {
      case Success(matched, _) => Option(matched)
      case _ => None
    }
  }

}