import scala.util.parsing.combinator._
import scala.collection.immutable.Map


class CommandsParser extends SimpleParser {
  private object Commands {
    def addPoll(map: Map[Int, Poll], poll:Poll): (Map[Int, Poll], String) =
      (map + (poll.id -> poll), s"Poll '${poll.name}' created. Id is ${poll.id}")

    def list(map: Map[Int, Poll]): (Map[Int, Poll], String) = {
      val result = map.map(data => s"id: ${data._2.id}; name: ${data._2.name}").mkString("\n")
      (map, result)
    }

    def deletePoll(map: Map[Int, Poll], id: Int): (Map[Int, Poll], String) =
      (map - id, s"Poll $id was deleted")

    def startPoll(map: Map[Int, Poll], id: Int): (Map[Int, Poll], String) = {
      map.get(id) match {
        case Some(p) => (map, p.startPoll)
        case None => (map, "Poll is undefined")
      }
    }

    def stopPoll(map: Map[Int, Poll], id: Int): (Map[Int, Poll], String) = {
      map.get(id) match  {
        case Some(p) => (map - id, p.stopPoll)
        case None => (map, "Poll is undefined")
      }
    }

    def result(map: Map[Int, Poll], id: Int): (Map[Int, Poll], String) = {
      map.get(id) match  {
        case Some(p) => (map, p.getResult)
        case None => (map, "Poll is undefined")
      }
    }
  }

  private def createPollParser(map: Map[Int, Poll]): Parser[(Map[Int, Poll], String)] =
    ( "/" ~> "create_poll" ~ word ~ anonymity.? ~ visibility.? ~ date.? ~ date.?) ^^
      { case _ ~ name ~ an ~ v ~ start ~ end => Commands.addPoll(map, Poll(NaturalNumbers.getNextId, name, an, v, start, end))}


  private def commandsWithIdParser(map: Map[Int, Poll]): Parser[(Map[Int, Poll], String)] = "/" ~> commandsWithId ~ number ^^
    {case cmd ~ id => cmd match {
      case "start_poll" => Commands.startPoll(map, id)
      case "stop_poll" => Commands.stopPoll(map, id)
      case "delete_poll" => Commands.deletePoll(map, id)
      case "result" => Commands.result(map, id)
    }}

  private def commandsWithoutIdParser(map: Map[Int, Poll]): Parser[(Map[Int, Poll], String)] = "/" ~> commandWithoutId ^^ {
    case "list" => Commands.list(map)
  }

  private def fullParser(map: Map[Int, Poll]): Parser[(Map[Int, Poll], String)] =
    commandsWithIdParser(map) | commandsWithoutIdParser(map) | createPollParser(map)

  def parseCmd(map: Map[Int, Poll], cmd: String): (Map[Int, Poll], String) = {
    parse(fullParser(map), cmd) match {
            case Success(matched,_) => matched
            case _ => (map, "Can't recognize command!")
    }
  }

}