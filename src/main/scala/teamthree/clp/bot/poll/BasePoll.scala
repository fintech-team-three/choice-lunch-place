package teamthree.clp.bot.poll

import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import io.circe.generic.auto._
import io.circe.syntax._
import teamthree.clp.bot.{ApplyPoll, NUser, Storages, Vote}

import scala.collection.mutable

abstract class BasePoll(val author: NUser, val storages: Storages) {
  type Action = (Long, String) => Seq[SendMessage]

  private var stage = 0

  private val stages = mutable.MutableList[Action]()
  private val participants = mutable.Map.empty[NUser, Boolean]

  //TODO Понять что делать с этим
  protected var allowUpdateStage = true

  protected def onCancelPoll(): Seq[SendMessage]

  protected def onNextStage(action: Action): Unit = {
    stages += action
  }

  protected def addParticipants(msg: String): Seq[SendMessage] = {
    val users = "@[A-Za-z_]+".r.findAllIn(msg).toList

    if (users.isEmpty)
      SendMessage(author.id, "Введите хотя бы одно имя пользователя") :: Nil
    else {
      users.flatMap { u =>
        storages.userStorage.find(u) match {
          case Some(user: NUser) =>
            if (user.pollAuthor == NUser.NOT_IN_POLL) {
              addParticipant(user)
              None
            } else {
              Some(SendMessage(author.id, s"Пользователь $u уже участвует в каком либо опросе"))
            }
          case None => Some(SendMessage(author.id, s"У пользователя $u нет диалога с ботом"))
        }
      }
    }
  }

  protected def addParticipant(user: NUser): Unit = {
    participants += user -> false
    storages.userStorage.map(user.id) { u => u.copy(pollAuthor = author.id) }
  }

  def cancelPoll(): Seq[SendMessage] = {
    val messages = onCancelPoll()
    participants.keys.foreach { user =>
      storages.userStorage.map(user.id) { u => u.copy(pollAuthor = NUser.NOT_IN_POLL) }
    }

    messages
  }

  def nextStage(from: Long, message: String): Seq[SendMessage] = {
    //TODO Проверка на окончание опроса
    val messages = stages(stage)(from, message)

    if (allowUpdateStage)
      stage += 1
    else
      allowUpdateStage = true

    messages
  }

  def sendToParticipants(fun: NUser => SendMessage): Seq[SendMessage] = {
    participants.keys
      .map { user => fun(user) }
      .toSeq
  }

  def sendPoll(): SendMessage = {
    val markup = InlineKeyboardMarkup.singleRow(Seq(
      InlineKeyboardButton.callbackData("Отправить", ApplyPoll(author.id, sendPoll = true).asJson.spaces2),
      InlineKeyboardButton.callbackData("Отменить", ApplyPoll(author.id, sendPoll = false).asJson.spaces2)))

    SendMessage(author.id, "Отправить опрос?", replyMarkup = Some(markup))
  }

  def vote(from: Long, element: String, vote: Vote): Seq[SendMessage] = {
    val messages = participants.filter(u => u._1.id == from).map {
      case (u: NUser, isVote: Boolean) =>
        if (isVote)
          SendMessage(u.id, "Вы уже голосовали")
        else {
          vote.vote(element)
          participants(u) = true
          SendMessage(u.id, "Ваш голос принят")
        }
    }.toSeq

    if (participants.count(p => p._2) == participants.size)
      messages ++ sendToParticipants { p => SendMessage(p.id, "Выбрано:" + vote.max) }
    else
      messages
  }
}
