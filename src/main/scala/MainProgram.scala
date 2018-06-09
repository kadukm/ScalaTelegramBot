object MainProgram {

  def getUsername(id: Long): String = {
    id.toString
  }

  def main(args: Array[String]): Unit = {
    val bot = new PollingBot
    bot.run()

//    val parser = new CommandsParser()
//    scala.io.Source.fromResource("commands.txt").getLines
//      .foldLeft(PollCurator(Map()))((pollCurator, str) => {
//        val userID = 101010
//        val res = parser.parseCmd(pollCurator, str, userID)
//        println(res._2)
//        res._1
//      })
  }
}


