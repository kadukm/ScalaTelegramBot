import java.text.SimpleDateFormat

import utils.{Command, CommandTypes, QuestionTypes, PollParams, QuestionParams}
import utils.CommandTypes._
import org.scalatest.FunSuite

class CommandsParserTests extends FunSuite {
  def handleCmd(strCmd: String): Command = {
    val maybeCmd = CommandsParser.parseCmd(strCmd)
    assert(maybeCmd.isDefined)
    maybeCmd.get
  }

  val date_parser = new SimpleDateFormat("hh:mm:ss yy:MM:dd")

  def checkCommandFields(cmd: Command, cmdType: CommandTypes.CommandType, pollID: Option[Int] = None,
                         questionID: Option[Int] = None, answer: Option[String] = None): Unit = {
    assertResult(cmdType) { cmd.cmdType }
    pollID match {
      case Some(id) => assertResult(id) { cmd.pollID.get }
      case None => assert(cmd.pollID.isEmpty) }
    questionID match {
      case Some(id) => assertResult(id) { cmd.questionID.get }
      case None => assert(cmd.questionID.isEmpty) }
    answer match {
      case Some(text) => assertResult(text) { cmd.answer.get }
      case None => assert(cmd.answer.isEmpty) }
  }

  def testCreatePoll(): Unit = {
    test("simple create_poll") {
      val cmd = handleCmd("/create_poll (testname)")
      checkCommandFields(cmd, CreatePoll)
      assertResult("testname") { cmd.pollParams.get.name }
    }

    test("create_poll with all parameters") {
      val cmd = handleCmd("/create_poll (my good name) yes afterstop 00:00:00 10:01:01 00:00:00 20:01:01")
      val pollParams = cmd.pollParams.get
      checkCommandFields(cmd, CreatePoll)
      assertResult("my good name") { pollParams.name }
      assert(pollParams.anonymity.get)
      assert(pollParams.afterStop.get)
      assertResult(date_parser.parse("00:00:00 10:01:01")) { pollParams.startDate.get }
      assertResult(date_parser.parse("00:00:00 20:01:01")) { pollParams.endDate.get }
    }

    test("create_poll with few parameters") {
      val cmd = handleCmd("/create_poll (my good name2) yes 00:00:00 10:01:01")
      val pollParams = cmd.pollParams.get
      checkCommandFields(cmd, CreatePoll)
      assertResult("my good name2") { pollParams.name }
      assert(pollParams.anonymity.get)
      assert(pollParams.afterStop.isEmpty)
      assertResult(date_parser.parse("00:00:00 10:01:01")) { pollParams.startDate.get }
      assert(pollParams.endDate.isEmpty)
    }
  }

  def testDeletePoll(): Unit = {
    test("simple delete_poll") {
      val cmd = handleCmd("/delete_poll 2")
      checkCommandFields(cmd, DeletePoll, pollID=Some(2))
    }

    test("bad delete_poll cmd") {
      val res = CommandsParser.parseCmd("/delete_poll bad_parameter")
      assert(res.isEmpty)
    }

    test("delete_poll without args") {
      val res = CommandsParser.parseCmd("/delete_poll")
      assert(res.isEmpty)
    }
  }

  def testResult(): Unit = {
    test("simple result") {
      val cmd = handleCmd("/result 2")
      checkCommandFields(cmd, Result, pollID=Some(2))
    }

    test("bad result cmd") {
      val res = CommandsParser.parseCmd("/result bad_parameter")
      assert(res.isEmpty)
    }

    test("result without args") {
      val res = CommandsParser.parseCmd("/result")
      assert(res.isEmpty)
    }
  }

  def testStartPoll(): Unit = {
    test("simple start_poll") {
      val cmd = handleCmd("/start_poll 2")
      checkCommandFields(cmd, StartPoll, pollID=Some(2))
    }

    test("bad start_poll cmd") {
      val res = CommandsParser.parseCmd("/start_poll bad_parameter")
      assert(res.isEmpty)
    }

    test("start_poll without args") {
      val res = CommandsParser.parseCmd("/start_poll")
      assert(res.isEmpty)
    }
  }

  def testStopPoll(): Unit = {
    test("simple stop_poll") {
      val cmd = handleCmd("/stop_poll 2")
      checkCommandFields(cmd, StopPoll, pollID=Some(2))
    }

    test("bad stop_poll cmd") {
      val res = CommandsParser.parseCmd("/stop_poll bad_parameter")
      assert(res.isEmpty)
    }

    test("stop_poll without args") {
      val res = CommandsParser.parseCmd("/stop_poll")
      assert(res.isEmpty)
    }
  }

  def testList(): Unit = {
    test("simple list") {
      val cmd = handleCmd("/list wrong_data")
      checkCommandFields(cmd, List_)
    }
  }

  def testView(): Unit = {
    test("simple view") {
      val cmd = handleCmd("/view")
      checkCommandFields(cmd, View)
    }
  }

  def testBegin(): Unit = {
    test("simple begin") {
      val cmd = handleCmd("/begin 1")
      checkCommandFields(cmd, Begin, pollID=Some(1))
    }

    test("bad begin cmd") {
      val res = CommandsParser.parseCmd("/begin wrong_data")
      assert(res.isEmpty)
    }

    test("begin without args") {
      val res = CommandsParser.parseCmd("/begin")
      assert(res.isEmpty)
    }
  }

  def testEnd(): Unit = {
    test("simple end") {
      val cmd = handleCmd("/end")
      checkCommandFields(cmd, End)
    }
  }

  def testAddQuestion(): Unit = {
    test("add Choice question") {
      val questionText = "good question"
      val cmd = handleCmd(s"/add_question ($questionText) (choice)\n(first)\n(second)")
      checkCommandFields(cmd, AddQuestion)
      val questionParams = cmd.questionParams.get
      assertResult(questionText) { questionParams.text }
      assertResult(QuestionTypes.Choice) { questionParams.qType }
      assert(questionParams.options.contains("first"))
      assert(questionParams.options.contains("second"))
      assert(!questionParams.options.contains("third"))
    }

    test("add Multi question") {
      val questionText = "good question"
      val cmd = handleCmd(s"/add_question ($questionText) (multi)\n(first)\n(second)")
      checkCommandFields(cmd, AddQuestion)
      val questionParams = cmd.questionParams.get
      assertResult(questionText) { questionParams.text }
      assertResult(QuestionTypes.Multi) { questionParams.qType }
      assert(questionParams.options.contains("first"))
      assert(questionParams.options.contains("second"))
      assert(!questionParams.options.contains("third"))
    }

    test("add Open question") {
      val questionText = "good question"
      val cmd = handleCmd(s"/add_question ($questionText) (open)")
      checkCommandFields(cmd, AddQuestion)
      val questionParams = cmd.questionParams.get
      assertResult(questionText) { questionParams.text }
      assertResult(QuestionTypes.Open) { questionParams.qType }
      assertResult(List.empty) { questionParams.options }
    }

    test("add non-sheer Open question") {
      val questionText = "good question"
      val cmd = handleCmd(s"/add_question ($questionText)")
      checkCommandFields(cmd, AddQuestion)
      val questionParams = cmd.questionParams.get
      assertResult(questionText) { questionParams.text }
      assertResult(QuestionTypes.Open) { questionParams.qType }
      assertResult(List.empty) { questionParams.options }
    }

    test("wrong add_question cmd") {
      val res = CommandsParser.parseCmd("/add_question wrong_data")
      assert(res.isEmpty)
    }
  }

  def testDeleteQuestion(): Unit = {
    test("simple delete_question") {
      val cmd = handleCmd("/delete_question 1")
      checkCommandFields(cmd, DeleteQuestion, questionID=Some(1))
    }

    test("bad delete_question cmd") {
      val res = CommandsParser.parseCmd("/begin wrong_data")
      assert(res.isEmpty)
    }

    test("begin delete_question args") {
      val res = CommandsParser.parseCmd("/begin")
      assert(res.isEmpty)
    }
  }

  def testAnswer(): Unit = {
    test("simple answer to Choice type") {
      val answer = "2"
      val cmd = handleCmd(s"/answer 1 $answer")
      checkCommandFields(cmd, Answer, questionID = Some(1), answer = Some(answer))
    }

    test("simple answer to Multi type") {
      val answer = "1 2 3"
      val cmd = handleCmd(s"/answer 1 $answer")
      checkCommandFields(cmd, Answer, questionID = Some(1), answer = Some(answer))
    }

    test("simple answer to Open type") {
      val answer = "my big answer with \"\n\"and some strange symbols like ©"
      val cmd = handleCmd(s"/answer 1 $answer")
      checkCommandFields(cmd, Answer, questionID = Some(1), answer = Some(answer))
    }

    test("answer cmd without args") {
      val res = CommandsParser.parseCmd("/answer")
      assert(res.isEmpty)
    }
  }

  testCreatePoll()
  testDeletePoll()
  testResult()
  testStartPoll()
  testStopPoll()
  testList()
  testView()
  testBegin()
  testEnd()
  testAddQuestion()
  testDeleteQuestion()
  testAnswer()
}
