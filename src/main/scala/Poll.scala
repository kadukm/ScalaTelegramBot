import java.util.{Calendar, Date}

import scala.collection.immutable.Map


case class Poll(id: Int, name: String, anonymity: Option[Boolean] = Option(true), afterStop: Option[Boolean] = Option(true),
                startDate: Option[Date] = None, endDate: Option[Date] = None, isRunning: Boolean = false,
                questions: Map[Int, Question] = Map(), IdGenerator: Iterator[Int] = Stream.from(1).iterator,
                ownerID: Long = -1) {

  def getNextNumber: Int = IdGenerator.next()

  def addQuestion(question: Question, number: Int): Poll = {
    this.copy(questions = questions + (number -> question))
  }

  def deleteQuestion(id: Int): Poll = {
    this.copy(questions = questions - id)
  }

  def currentDate: Date = Calendar.getInstance().getTime

  def startPoll: Poll = {
    this.copy(isRunning = true)
  }

  def stopPoll: Poll = {
    this.copy(isRunning = false)
  }

  def answerToString(t: (Int, Set[Long])): String = {
    if (t._2.nonEmpty)
      t._1 + " votes. (IDs: " + (t._2 - 0).map(e => MainProgram.getUsername(e)).mkString(", ") + ")"
    else "0 votes"
  }

  private def getAnswers(question: Question): String = {
    if (anonymity.getOrElse(false)) {
      s"Answers:${
        question.variants.zipWithIndex
          .map(e => s"\n\t\t ${e._2}: ${e._1} [${question.answers.getOrElse(e._1, (0, Set(0l)))._1} votes]").mkString
      }"
    }
    else {
      s"Answers: ${
        question.variants.zipWithIndex.map(e =>
          s"\n\t\t${e._2}: ${e._1} [${answerToString(question.answers.getOrElse(e._1, (0, Set(0l))))}]").mkString
      }"
    }
  }

  private def questionToString(question: Question): String = {
    val startQuestionStr = s"\n\t${question.question} ${question.qType}\n\t"
    if (isRunning && afterStop.getOrElse(false)) {
      startQuestionStr + s"Variants:${question.variants.zipWithIndex.map(e => s"\n\t\t${e._2}: ${e._1}").mkString}"
    }
    else {
      startQuestionStr + getAnswers(question)
    }
  }

  override def toString: String = {
    s"""id: $id
       |name: $name
       |anonymity: ${anonymity.getOrElse("undefined")}
       |afterstop: ${afterStop.getOrElse("undefined")}
       |start date: ${startDate.getOrElse("undefined")}
       |end date: ${endDate.getOrElse("undefined")}
       |in process: ${isRunning.toString}
       |current questions: ${questions.map(pair => "\n\tID: " + pair._1 + ". " + questionToString(pair._2)).mkString}\n""".stripMargin
  }
}