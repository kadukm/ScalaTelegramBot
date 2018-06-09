import java.util.Calendar
import java.util.Date

import utils.{Command, NaturalNumbers, PollParams, QuestionParams}
import utils.CommandTypes._

import scala.collection.immutable.Map


case class PollCurator(pollMap: Map[Int, Poll], questionMap: Map[Int, (Int, Question)] = Map(),
                       contexts: Map[Long, Int] = Map(), gen: NaturalNumbers = new NaturalNumbers()) {

  def tryHandleCmd(maybeCmd: Option[Command], userID: Long): (PollCurator, String) = {
    maybeCmd match {
      case Some(cmd) => handleCmd(cmd, userID)
      case None => (this, "Cant't recognize the command")
    }
  }

  private def handleCmd(cmd: Command, userID: Long): (PollCurator, String) = {
    val updCurator = refreshPolls(this, Calendar.getInstance().getTime)
    cmd.cmdType match {
      case List_ => updCurator.list()
      case DeletePoll => updCurator.deletePoll(cmd.pollID.get, userID)
      case StartPoll => updCurator.startPoll(cmd.pollID.get, userID)
      case StopPoll => updCurator.stopPoll(cmd.pollID.get, userID)
      case Result => updCurator.result(cmd.pollID.get)
      case CreatePoll => updCurator.addPoll(userID, cmd.pollParams.get)
      case Begin => updCurator.begin(cmd.pollID.get, userID)
      case End => updCurator.end(userID)
      case View => updCurator.view(userID)
      case AddQuestion => updCurator.addQuestion(cmd.questionParams.get, userID)
      case DeleteQuestion => updCurator.deleteQuestion(cmd.questionID.get, userID)
      case Answer => updCurator.answerToQuestion(cmd.questionID.get, cmd.answer.get, userID)
    }
  }

  private def refreshPolls(curator: PollCurator, curTime: Date): PollCurator = {
    val newMap = curator.pollMap.foldLeft(Map[Int, Poll]())((updatedMap, pair) => {
      val id = pair._1
      val poll = pair._2
      if (poll.startDate.isDefined && poll.endDate.isDefined) {
        if (poll.startDate.get.before(curTime) && poll.endDate.get.after(curTime))
          updatedMap + (id -> poll.startPoll)
        else if (poll.startDate.get.after(curTime) || poll.endDate.get.before(curTime))
          updatedMap + (id -> poll.stopPoll)
        else
          updatedMap
      }
      else if (poll.startDate.isDefined && poll.startDate.get.before(curTime))
        updatedMap + (id -> poll.startPoll)
      else if (poll.endDate.isDefined && poll.endDate.get.before(curTime))
        updatedMap + (id -> poll.stopPoll)
      else
        updatedMap + (id -> poll)
    })
    curator.copy(pollMap = newMap, gen = gen)
  }

  def addPoll(userID: Long, pollParams: PollParams): (PollCurator, String) = {
    val (name, anonymity, afterstop, startDate, endDate) = pollParams.unpack
    val poll = Poll(gen.getNextId, name, anonymity, afterstop, startDate, endDate, ownerID = userID)
    (this.copy(pollMap + (poll.id -> poll), gen = gen), s"Poll '${poll.name}' created. Id is ${poll.id}")
  }

  def list(): (PollCurator, String) = {
    if (pollMap.isEmpty)
      (this, "No polls")
    else
      (this, pollMap.map(data => s"id: ${data._2.id}; name: ${data._2.name}").mkString("\n"))
  }

  def deletePoll(pollID: Int, userID: Long): (PollCurator, String) = {
    pollMap.get(pollID) match {
      case Some(p) =>
        if (p.ownerID == userID)
          if (contexts.getOrElse(userID, -1) == pollID)
            (this.copy(pollMap - pollID, contexts = contexts - userID, gen = gen), s"Poll $pollID was deleted so the context mode was interrupted ")
          else
            (this.copy(pollMap - pollID, gen = gen), s"Poll $pollID was deleted")
        else
          (this, "You are not poll owner")
      case None => (this, "Poll is undefined")
    }
  }

  def startPoll(pollID: Int, userID: Long): (PollCurator, String) = {
    pollMap.get(pollID) match {
      case Some(p) =>
        if (p.startDate.isEmpty) {
          if (p.ownerID == userID) {
            val startedPoll = p.startPoll
            (this.copy(pollMap - pollID + (startedPoll.id -> startedPoll), gen = gen), s"Poll '${startedPoll.name}' (id $pollID) started")
          }
          else
            (this, "You are not poll owner")
        }
        else (this, "Start date is defined, you can't start this poll")
      case None => (this, "Poll is undefined")
    }
  }

  def stopPoll(pollID: Int, userID: Long): (PollCurator, String) = {
    pollMap.get(pollID) match {
      case Some(p) =>
        if (p.endDate.isEmpty) {
          if (p.ownerID == userID) {
            val stoppedPoll = p.stopPoll
            (this.copy(pollMap - pollID + (stoppedPoll.id -> stoppedPoll), gen = gen), s"Poll '${stoppedPoll.name}' (id $pollID) stopped")
          }
          else
            (this, "You are not poll owner")
        }
        else (this, "End date is defined, u can't stop this poll")
      case None => (this, "Poll is undefined")
    }
  }

  def result(id: Int): (PollCurator, String) = {
    pollMap.get(id) match {
      case Some(p) => (PollCurator(pollMap), p.toString())
      case None => (this, "Poll is undefined")
    }
  }

  def begin(id: Int, userID: Long): (PollCurator, String) = {
    if (pollMap.isDefinedAt(id))
      (this.copy(contexts = contexts + (userID -> id), gen = gen), s"Work began with poll $id")
    else (this, s"Can't find poll with id $id")
  }

  def end(userID: Long): (PollCurator, String) = {
    if (contexts.isDefinedAt(userID)) {
      val currentContext = contexts(userID)
      (this.copy(contexts = contexts - userID, gen = gen), s"Work with the poll $currentContext is finished")
    }
    else (this, s"Context mode not enabled")
  }

  def view(userID: Long): (PollCurator, String) = {
    val r = contexts.getOrElse(userID, -1)
    if (r != -1)
      pollMap.get(r)
        .map(p => (this, p.toString))
        .getOrElse(this, "Current poll does not exist")
    else (this, s"Context mode not enabled")
  }

  def addQuestion(questionParams: QuestionParams, userID: Long): (PollCurator, String) = {
    val (text, qType, answers, options) = questionParams.unpack
    val contextID = contexts.getOrElse(userID, -1)
    pollMap.get(contextID) match {
      case Some(poll) =>
        if (poll.ownerID == userID) {
          if (poll.isRunning)
            (this, "Sorry, poll is running")
          else {
            val qId = poll.getNextNumber
            val question = Question(text, qType, answers, poll.anonymity.getOrElse(false), options.toVector)
            val updatedPoll = poll.addQuestion(question, qId)
            (this.copy(pollMap + (contextID -> updatedPoll), gen = gen),
              s"A question has been added to the poll '${updatedPoll.name}' ($contextID). Question id is $qId")
          }
        }
        else
          (this, "You are not poll owner")
      case None => (this, "Context mode not enabled")
    }
  }

  def deleteQuestion(id: Int, userID: Long): (PollCurator, String) = {
    val contextId = contexts.getOrElse(userID, -1)
    pollMap.get(contextId) match {
      case Some(p) =>
        if (p.ownerID == userID) {
          if (p.isRunning)
            (this, "Sorry, poll is running")
          else {
            val updatedPoll = p.deleteQuestion(id)
            (this.copy(pollMap + (contextId -> updatedPoll), gen = gen),
              s"A question with id $id has been deleted from the poll '${updatedPoll.name}' ($contextId)")
          }
        }
        else
          (this, "You are not poll owner")
      case None => (this, "Context mode not enabled")
    }
  }

  def answerToQuestion(id: Int, answ: String, userID: Long): (PollCurator, String) = {
    val contextId = contexts.getOrElse(userID, -1)
    pollMap.get(contextId) match {
      case Some(p) =>
        if (p.isRunning) {
          p.questions.get(id) match {
            case Some(q) =>
              val res = q.answer(answ, userID)
              (this.copy(pollMap - contextId + (contextId -> p.addQuestion(res._1, id)), gen = gen), res._2)
            case None => (this, "Question is undefined")
          }
        }
        else (this, "Poll is not running")
      case None => (this, "Context mode not enabled")
    }
  }
}