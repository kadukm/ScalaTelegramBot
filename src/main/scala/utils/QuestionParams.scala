package utils

import QuestionTypes.QuestionType

case class QuestionParams(text: String, qType: QuestionType, options: List[String]) {
  def unpack: (String, QuestionType, Map[String, (Int, Set[Long])], List[String]) =
    (text, qType, options.map(option => option -> (0, Set(0l))).toMap, options)
}
