import utils.QuestionTypes

import org.scalatest.FunSuite

class PollCuratorTests extends FunSuite {
  def executeCommands(commands: List[(String, Long)], curator: PollCurator = PollCurator(Map())): PollCurator = {
    commands.foldLeft(curator)((curator, testCase) => {
      val strCmd = testCase._1
      val userID = testCase._2
      val curCmd = CommandsParser.parseCmd(strCmd)
      curator.tryHandleCmd(curCmd, userID)._1
    })
  }

  def testCommandsWithoutContext(): Unit = {
    def testCreatePoll(): Unit = {
      test("Create poll") {
        val curator_v1 = PollCurator(Map())
        val res_curator = executeCommands(List(("/create_poll (testname)", 1)), curator_v1)
        assert(!curator_v1.pollMap.contains(1))
        assert(res_curator.pollMap.contains(1))
      }

      test("Create few polls") {
        val curator_v1 = PollCurator(Map())
        val res_curator = executeCommands(List(("/create_poll (testname)", 1), ("/create_poll (testnametwo)", 1)), curator_v1)
        assert(!curator_v1.pollMap.contains(1))
        assert(res_curator.pollMap.contains(1))
        assert(!curator_v1.pollMap.contains(2))
        assert(res_curator.pollMap.contains(2))
      }

      test("Check owner") {
        val res_curator = executeCommands(List(("/create_poll (testname)", 1)))
        assert(res_curator.pollMap(1).ownerID == 1)
      }

      test("Check poll's properties") {
        val name = "testname"
        val res_curator = executeCommands(List((s"/create_poll ($name) yes afterstop 00:00:00 20:01:01", 1)))
        val poll = res_curator.pollMap(1)
        assert(poll.name == name)
        assert(!poll.isRunning)
        assert(poll.afterStop.get)
        assert(poll.anonymity.get)
      }
    }

    def testDeletePoll(): Unit = {
      test("Simple delete") {
        val curator_v1 = executeCommands(List(("/create_poll (testname)", 1)))
        val curator_v2 = executeCommands(List(("/delete_poll 1", 1)), curator_v1)
        assert(curator_v1.pollMap.contains(1))
        assert(!curator_v2.pollMap.contains(1))
      }

      test("Delete by another user") {
        val res_curator = executeCommands(List(("/create_poll (testname)", 1), ("/delete_poll 1", 2)))
        assert(res_curator.pollMap.contains(1))
      }
    }

    def testStartPoll(): Unit = {
      test("Start poll") {
        val curator_v1 = executeCommands(List(("/create_poll (testname)", 1)))
        val curator_v2 = executeCommands(List(("/start_poll 1", 1)), curator_v1)
        assert(!curator_v1.pollMap(1).isRunning)
        assert(curator_v2.pollMap(1).isRunning)
      }

      test("Start poll not by owner") {
        val res_curator = executeCommands(List(("/create_poll (testname)", 1), ("/start_poll 1", 2)))
        assert(!res_curator.pollMap(1).isRunning)
      }
    }

    def testStopPoll(): Unit = {
      test("Simple stop poll") {
        val curator_v1 = executeCommands(List(("/create_poll (testname)", 1), ("/start_poll 1", 1)))
        val curator_v2 = executeCommands(List(("/stop_poll 1", 1)), curator_v1)
        assert(curator_v1.pollMap(1).isRunning)
        assert(!curator_v2.pollMap(1).isRunning)
      }
    }

    testCreatePoll()
    testDeletePoll()
    testStartPoll()
    testStopPoll()
  }

  def testCommandsWithContext(): Unit = {
    def testContext(): Unit = {
      val curator_v1 = executeCommands(List(("/create_poll (testname)", 1)))
      val curator_v2 = executeCommands(List(("/begin 1", 1)), curator_v1)
      val curator_v3 = executeCommands(List(("/end", 1)), curator_v2)
      assert(!curator_v1.contexts.contains(1))
      assertResult(1) { curator_v2.contexts(1) }
      assert(!curator_v3.contexts.contains(1))

      test("Check contexts of different users") {
        val res_curator = executeCommands(List(("/create_poll (testname)", 2), ("/begin 1", 1)))
        assert(res_curator.contexts.contains(1) && !res_curator.contexts.contains(2))
      }
    }

    def testAddQuestion(): Unit = {
      test("Check questions' types") {
        val res_curator = executeCommands(List(
          ("/create_poll (testname)", 1),
          ("/begin 1", 1),
          ("/add_question (good question one) (choice)\n(first)\n(second)", 1),
          ("/add_question (good question two) (open)", 1),
          ("/add_question (good question three) (multi)", 1)))
        assert(res_curator.pollMap(1).questions(1).qType == QuestionTypes.Choice)
        assert(res_curator.pollMap(1).questions(2).qType == QuestionTypes.Open)
        assert(res_curator.pollMap(1).questions(3).qType == QuestionTypes.Multi)
      }

      test("Check choice variants") {
        val res_curator = executeCommands(List(
          ("/create_poll (testname)", 1),
          ("/begin 1", 1),
          ("/add_question (good question one) (choice)\n(first)\n(second)", 1)))
        assert(res_curator.pollMap(1).questions(1).answers.contains("first"))
        assert(res_curator.pollMap(1).questions(1).answers.contains("second"))
      }
    }

    def testDeleteQuestion(): Unit = {
      test("Delete simple question") {
        val curator_v1 = executeCommands(List(
          ("/create_poll (testname)", 1),
          ("/begin 1", 1),
          ("/add_question (good question one) (choice)\n(first)\n(second)", 1)))
        val curator_v2 = executeCommands(List(("/delete_question 1", 1)), curator_v1)
        assert(curator_v1.pollMap(1).questions.contains(1))
        assert(!curator_v2.pollMap(1).questions.contains(1))
      }
    }

    def testAnswer(): Unit = {
      test("Answer Choice") {
        val curator_v1 = executeCommands(List(
          ("/create_poll (testname)", 1),
          ("/begin 1", 1),
          ("/add_question (good question one) (choice)\n(first)\n(second)", 1)))
        val curator_v2 = executeCommands(
          List(("/end", 1),
               ("/start_poll 1", 1),
               ("/begin 1", 1),
               ("/answer 1 0", 1)),
          curator_v1)
        assert(curator_v1.pollMap(1).questions(1).answers("first")._1 == 0)
        assert(curator_v1.pollMap(1).questions(1).answers("second")._1 == 0)
        assert(curator_v2.pollMap(1).questions(1).answers("first")._1 == 1)
        assert(curator_v2.pollMap(1).questions(1).answers("second")._1 == 0)
      }

      test("Answer Open") {
        val res_curator = executeCommands(List(
          ("/create_poll (testname)", 1),
          ("/begin 1", 1),
          ("/add_question (good question) (open)", 1),
          ("/end", 1),
          ("/start_poll 1", 1),
          ("/begin 1", 1),
          ("/answer 1 sometext", 1)))
        assert(res_curator.pollMap(1).questions(1).answers.contains("sometext"))
      }

      test("Answer Multi") {
        val res_curator = executeCommands(List(
          ("/create_poll (testname)", 1),
          ("/begin 1", 1),
          ("/add_question (good question) (multi)\n(first)\n(second)\n(third)", 1),
          ("/end", 1),
          ("/start_poll 1", 1),
          ("/begin 1", 1),
          ("/answer 1 0 2", 1)))
        assert(res_curator.pollMap(1).questions(1).answers("first")._1 == 1)
        assert(res_curator.pollMap(1).questions(1).answers("second")._1 == 0)
        assert(res_curator.pollMap(1).questions(1).answers("third")._1 == 1)
      }
    }

    testContext()
    testAddQuestion()
    testDeleteQuestion()
    testAnswer()
  }

  testCommandsWithoutContext()
  testCommandsWithContext()
}
