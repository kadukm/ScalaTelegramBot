import java.text.SimpleDateFormat
import java.util.Date

import utils.QuestionTypes._

import scala.util.parsing.combinator._

class SimpleParsers extends RegexParsers {
  def anonymity: Parser[Boolean] = ("yes" | "no") ^^ {
    _ == "yes"
  }

  def visibility: Parser[Boolean] =
    ("afterstop" | "continuous") ^^ { _ == "afterstop" }

  def date: Parser[Date] =
    """\d{2}:\d{2}:\d{2} \d{2}:\d{2}:\d{2}""".r ^^ {
      new SimpleDateFormat("hh:mm:ss yy:MM:dd").parse(_)
    }

  def word: Parser[String] =
    """[a-zA-Z_ 0-9]+""".r | ("(" ~> word <~ ")") ^^ { _.toString }

  def anyWord: Parser[String] =
    "(" ~> ".+".r <~ ")" ^^ { _.toString }

  def number: Parser[Int] =
    """(0|[1-9]\d*)""".r ^^ {
      _.toInt
    }

  def commandsWithId: Parser[String] = "start_poll" | "stop_poll" | "delete_poll" | "result"

  def commandWithoutId: Parser[String] = "list" ^^ (s => s)

  def contextWithoutIdParser: Parser[String] = "end" | "view"

  def questionTypeParser: Parser[QuestionType] =
    "(" ~> ("open" | "choice" | "multi") <~ ")" ^^ {
      case "open" | "Open" => Open
      case "choice" | "Choice" => Choice
      case "multi" | "Multi" => Multi
    }

  def questionArgumentParser: Parser[String] =
      """\(((\(\()|(\)\))|[^()])*\)""".r ^^ {
      qa => qa.substring(1, qa.length - 1).replace("((", "(").replace("))", ")")
    }

  def optionsParser: Parser[List[String]] =
    rep(("\\r?\\n" ~> questionArgumentParser) | questionArgumentParser)
}
