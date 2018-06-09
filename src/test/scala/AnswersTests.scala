import org.scalatest.FunSuite
import org.scalatest.Assertions._

class AnswersTests extends FunSuite {
  private def testCmd(testCase: TestCase, curator: PollCurator): PollCurator = {
    val cmd = CommandsParser.parseCmd(testCase.cmd)
    val res = curator.tryHandleCmd(cmd, testCase.userID)
    assertResult(testCase.expRes) { res._2 }
    res._1
  }

  private def testSingleCommand(description: String, testCase: TestCase, curator: PollCurator = PollCurator(Map())): Unit = {
    test(description) {
      testCmd(testCase, curator)
    }
  }

  private def testFewCommands(description: String, commands: List[TestCase]): Unit = {
    commands.foldLeft(PollCurator(Map()))((curator, testCase) => {
      testCmd(testCase, curator)
    })
  }

  private def testCommandsWithoutContext(): Unit = {
    def testCreatePoll(): Unit = {
      testSingleCommand("Create poll without parameters", TestCase("/create_poll (testname)",
        "Poll 'testname' created. Id is 1"))
      testSingleCommand("Create poll with few parameters",
        TestCase("/create_poll (testname) afterstop 04:00:00", "Poll 'testname' created. Id is 1"))
      testSingleCommand("Create poll with all parameters",
        TestCase("/create_poll (testname) yes afterstop 04:00:00 18:03:18 04:00:00 20:03:18", "Poll 'testname' created. Id is 1"))
      testFewCommands("Different users create different polls", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1", 1),
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 2", 2)))
    }

    def testDeletePoll(): Unit = {
      testSingleCommand("Try delete uncreated poll", TestCase("/delete_poll 1", "Poll is undefined"))
      testFewCommands("Different users try delete one poll", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1", 1),
        TestCase("/delete_poll 1", "You are not poll owner", 2),
        TestCase("/delete_poll 1", "Poll 1 was deleted", 1)))
    }

    def testResult(): Unit = {
      testSingleCommand("Ask result for uncreated poll", TestCase("/result 1", "Poll is undefined"))
      testFewCommands("Check result for poll created without parameters just now", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1"),
        TestCase("/result 1", "id: 1\nname: testname\nanonymity: undefined\nafterstop: undefined\n" +
          "start date: undefined\nend date: undefined\nin process: false\ncurrent questions: \n")))
      testFewCommands("Check result for poll created with all parameters just now", List(
        TestCase("/create_poll (testname) yes afterstop 04:00:00 18:03:18 04:00:00 20:03:18", "Poll 'testname' created. Id is 1"),
        TestCase("/result 1", "id: 1\nname: testname\nanonymity: true\nafterstop: true\n" +
          "start date: Sun Mar 18 04:00:00 YEKT 2018\n" +
          "end date: Wed Mar 18 04:00:00 YEKT 2020\nin process: true\ncurrent questions: \n")))
    }

    def testStartPoll(): Unit = {
      testFewCommands("Start poll when start date was defined", List(
        TestCase("/create_poll (testname) 00:00:00 20:12:30", "Poll 'testname' created. Id is 1"),
        TestCase("/start_poll 1", "Start date is defined, you can't start this poll")))
      testFewCommands("Start poll in advance", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1"),
        TestCase("/start_poll 1", "Poll 'testname' (id 1) started")))
    }

    def testStopPoll(): Unit = {
      testFewCommands("Stop poll", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1"),
        TestCase("/stop_poll 1", "Poll 'testname' (id 1) stopped")))
    }

    def testList(): Unit = {
      testFewCommands("Check list of polls by different users", List(
        TestCase("/create_poll (test poll one)", "Poll 'test poll one' created. Id is 1", 1),
        TestCase("/create_poll (test poll two)", "Poll 'test poll two' created. Id is 2", 2),
        TestCase("/list", "id: 1; name: test poll one\nid: 2; name: test poll two", 1),
        TestCase("/list", "id: 1; name: test poll one\nid: 2; name: test poll two", 2)))
    }

    testCreatePoll()
    testDeletePoll()
    testStartPoll()
    testStopPoll()
    testResult()
    testList()
  }

  private def testCommandsWithContext(): Unit = {
    def testView(): Unit = {
      testFewCommands("Check view", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1"),
        TestCase("/begin 1", "Work began with poll 1"),
        TestCase("/view", "id: 1\nname: testname\nanonymity: undefined\n" +
          "afterstop: undefined\nstart date: undefined\n" +
          "end date: undefined\nin process: false\ncurrent questions: \n")))
    }

    def testSimpleBeginEnd(): Unit = {
      testSingleCommand("Work with context of uncreated poll", TestCase("/begin 1", "Can't find poll with id 1"))
      testSingleCommand("End work with context of uncreated poll", TestCase("/end", "Context mode not enabled"))
      testFewCommands("Useless work with context", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1"),
        TestCase("/begin 1", "Work began with poll 1"),
        TestCase("/end", "Work with the poll 1 is finished")))
    }

    def testAddQuestion(): Unit = {
      testFewCommands("Add new question to a poll and view", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1"),
        TestCase("/begin 1", "Work began with poll 1"),
        TestCase("/add_question (good question) (choice)\n(first)\n(second)",
          "A question has been added to the poll 'testname' (1). Question id is 1"),
        TestCase("/view", "id: 1\nname: testname\nanonymity: undefined\nafterstop: undefined\n" +
          "start date: undefined\nend date: undefined\nin process: false\ncurrent questions: " +
          "\n\tID: 1. \n\tgood question Choice\n\tAnswers: \n\t\t0: first [0 votes. (IDs: )]\n\t\t1: second [0 votes. (IDs: )]\n")))
      testFewCommands("Add few questions with different types to a poll", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1"),
        TestCase("/begin 1", "Work began with poll 1"),
        TestCase("/add_question (good question) (choice)\n(first)\n(second)",
          "A question has been added to the poll 'testname' (1). Question id is 1"),
        TestCase("/add_question (second good question) (open)",
          "A question has been added to the poll 'testname' (1). Question id is 2"),
        TestCase("/add_question (third good question) (multi)\n(first)\n(second)\n(third)",
          "A question has been added to the poll 'testname' (1). Question id is 3")))
    }

    def testDeleteQuestion(): Unit = {
      testFewCommands("Add and delete question", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1"),
        TestCase("/begin 1", "Work began with poll 1"),
        TestCase("/add_question (good question) (open)",
          "A question has been added to the poll 'testname' (1). Question id is 1"),
        TestCase("/delete_question 1", "A question with id 1 has been deleted from the poll 'testname' (1)")))
      testFewCommands("Try delete uncreated question", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1"),
        TestCase("/begin 1", "Work began with poll 1"),
        TestCase("/delete_question 1", "A question with id 1 has been deleted from the poll 'testname' (1)")))
    }

    def testAnswer(): Unit = {
      testFewCommands("Answer to not started poll", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1"),
        TestCase("/begin 1", "Work began with poll 1"),
        TestCase("/add_question (good question) (open)",
          "A question has been added to the poll 'testname' (1). Question id is 1"),
        TestCase("/answer 1 (my perfect answer)", "Poll is not running")))
      testFewCommands("Answer to started poll", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1"),
        TestCase("/begin 1", "Work began with poll 1"),
        TestCase("/add_question (good question) (open)",
          "A question has been added to the poll 'testname' (1). Question id is 1"),
        TestCase("/end", "Work with the poll 1 is finished"),
        TestCase("/start_poll 1", "Poll 'testname' (id 1) started"),
        TestCase("/begin 1", "Work began with poll 1"),
        TestCase("/answer 1 (my perfect answer)", "Thank you for answer")))
    }

    def testContexts(): Unit = {
      testFewCommands("One user works with context, second user works without context", List(
        TestCase("/create_poll (testname)", "Poll 'testname' created. Id is 1", 1),
        TestCase("/begin 1", "Work began with poll 1", 1),
        TestCase("/create_poll (testnametwo)", "Poll 'testnametwo' created. Id is 2", 2)))
    }

    testSimpleBeginEnd()
    testView()
    testAddQuestion()
    testDeleteQuestion()
    testAnswer()
    testContexts()
  }

  testCommandsWithoutContext()
  testCommandsWithContext()
}
