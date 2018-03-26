object Main extends CommandsParser {
  def main(args: Array[String]): Unit = {
    val emptyMap: Map[Int, Poll] = Map()
    scala.io.Source.fromResource("commands.txt").getLines
      .foldLeft(emptyMap)((map, str) => {
        val res = parseCmd(map, str)
        println(res._2)
        res._1
      })
  }


}
