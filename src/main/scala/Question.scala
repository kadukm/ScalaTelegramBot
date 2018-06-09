import java.security.MessageDigest

import utils.QuestionTypes._

case class Question(question: String, qType: QuestionType,
                    answers: Map[String, (Int, Set[Long])],
                    anonymity: Boolean,
                    variants: Vector[String],
                    users: Set[String] = Set.empty) {

  def answer(answer: String, userId: Long): (Question, String) = {
    val md5 = (x: Long) => MessageDigest.getInstance("MD5").digest(x.toString.getBytes).map("%02x".format(_)).mkString
    val hashId = md5(userId)
    val hiddenUserId = if (anonymity) 0 else userId
    if (users.contains(hashId)) {
      (this, "You have already voted")
    }
    else
      qType match {
        case Open =>
          if (answers.contains(answer)) {
            val oldAnswer = answers.get(answer)
            (this.copy(answers = answers + (answer -> (oldAnswer.get._1 + 1, oldAnswer.get._2 + hiddenUserId)), users = users + hashId),
              "Thank you for answer")
          }
          else
            (this.copy(answers = answers + (answer -> (1, Set(hiddenUserId))), users = users + hashId,
              variants = variants :+ answer),
              "Thank you for answer")
        case Choice =>
          try {
            val ansVar = variants(answer.toInt)
            val oldAnswer = answers.get(ansVar)
            (this.copy(answers = answers - ansVar + (ansVar -> (oldAnswer.get._1 + 1, oldAnswer.get._2 + hiddenUserId)),
              users = users + hashId), "Thank you for choice")
          }
          catch {
            case _: Throwable => (this, "Wrong command")
          }
        case Multi =>
          try {
            val vars = answer.split("\\s+").map(_.toInt).toList
            if (vars.toSet.size != vars.length) (this, "Don't repeat answers")
            else (this.copy(answers = getUpdatedAnswers(vars, userId, answers, hiddenUserId),
              users = users + hashId), "Thank you for the answer!")
          }
          catch {
            case _: Throwable => (this, "Wrong command")
          }
      }
  }

  def getUpdatedAnswers(numbers: List[Int], userId: Long,
                        curAnswers: Map[String, (Int, Set[Long])],
                        hiddenUserId: Long): Map[String, (Int, Set[Long])] = {
    if (numbers.isEmpty)
      curAnswers
    else {
      val ansVar = variants(numbers.head)
      val oldAnswer = curAnswers.get(ansVar)
      if (oldAnswer.get._2.contains(userId)) getUpdatedAnswers(numbers.drop(1), userId, curAnswers, hiddenUserId)
      else getUpdatedAnswers(numbers.drop(1), userId,
        curAnswers - ansVar + (ansVar -> (oldAnswer.get._1 + 1, oldAnswer.get._2 + hiddenUserId)), hiddenUserId)
    }
  }

  override def toString: String = {
    if (qType != Open)
      s"\n\t$question\n\tType: $qType\n\tVariants:${
        variants.zipWithIndex
          .map(e => s"\n\t\t${e._2}: ${e._1} (${answers.getOrElse(e._1, (0, Set(0)))._1} votes)").mkString
      }"
    else
      s"\n\t$question\n\tType: $qType\n\tAnswers:${variants.map(e => "\n\t\t" + e).mkString}"
  }
}
