import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

class PollingBot extends TelegramBot
  with Polling {
  private var curator = PollCurator(Map())
  override def receiveMessage(msg: Message): Unit = {
    for (text <- msg.text) {
      val curCmd = CommandsParser.parseCmd(text)
      val res = curator.tryHandleCmd(curCmd, msg.from.get.id)
      val answer = res._2
      curator = res._1
      request(SendMessage(msg.source, answer))
    }
  }
  override def token: String = "TOKEN"
}