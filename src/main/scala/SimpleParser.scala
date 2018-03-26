import java.text.SimpleDateFormat
import java.util.Date

import scala.util.parsing.combinator._

class SimpleParser extends RegexParsers {
  def anonymity: Parser[Boolean] = ("yes" | "no") ^^ {_ == "yes"}

  def visibility: Parser[Boolean] = ("afterstop" | "continuous") ^^ {_ == "continuous"}

  def date: Parser[Date] =
    """\d{2}:\d{2}:\d{2} \d{2}:\d{2}:\d{2}""".r  ^^ {new SimpleDateFormat("hh:mm:ss yy:MM:dd").parse(_)}

  def word: Parser[String] = """[a-z ]+""".r | ("(" ~>  word <~ ")") ^^ { _.toString }

  def anyWord: Parser[String] = "(" ~>  ".+".r <~ ")" ^^ {_.toString }

  def number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }

  def commandsWithId: Parser[String] = "start_poll" | "stop_poll" |"delete_poll" | "result" ^^ (s => s)

  def commandWithoutId: Parser[String] = "list" ^^ (s => s)
}
