package teamthree.clp.bot.poll

import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import io.circe.generic.auto._
import io.circe.syntax._
import teamthree.clp.bot._

import scala.collection.mutable

/**
  * Базовый класс опроса
  *
  * @param author автор опроса
  * @param userStorage
  */
abstract class BasePoll(val author: BotUser, val userStorage: InMemoryUserBotStorage) {
  type Action = InputMessage => StageResult

  private[BasePoll] case class StageResult(messages: Seq[SendMessage], update: Boolean)

  private var stage = 0

  private val stages = mutable.MutableList[Action]()

  private val participants = mutable.Map.empty[BotUser, Boolean]

  /**
    * Сообщения отправляемые при отмене опроса
    *
    * @return Список отправлемых сообщений
    */
  protected def onCancelPoll(): Seq[SendMessage]

  /**
    * Сообщения отправляемые при завершении опроса
    *
    * @return Список отправлемых сообщений
    */
  protected def onEndPoll(): Seq[SendMessage]

  /**
    * Добавляет следующий этап опроса
    */
  protected def onStage(action: Action): Unit = {
    stages += action
  }

  /**
    * Добавлеяет участников опроса
    *
    * @param msg Сообщение со списком логинов пользователей
    * @return Список сообщений отправлемых автору опроса
    */
  def addParticipants(msg: String): Seq[SendMessage] = {
    val users = "@[A-Za-z_]+".r.findAllIn(msg).toList

    if (users.isEmpty)
      SendMessage(author.id, "Введите хотя бы одно имя пользователя") :: Nil
    else {
      users.flatMap { u =>
        userStorage.find { user => user.username == u } match {
          case Some(user: BotUser) =>
            if (user.pollAuthor == BotUser.NOT_IN_POLL) {
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

  def addParticipant(user: BotUser): Unit = {
    participants += user -> false
    userStorage.map(user.id) { u => u.copy(pollAuthor = author.id) }
  }

  /**
    * Отмена опроса
    *
    * @return Список отправлемых сообщений
    */
  def cancelPoll(): Seq[SendMessage] = {
    val messages = onCancelPoll()
    participants.keys.foreach { user =>
      userStorage.map(user.id) { u => u.copy(pollAuthor = BotUser.NOT_IN_POLL) }
    }

    messages
  }

  def nextStage(message: InputMessage): Seq[SendMessage] = {
    val stageResult = stages(stage)(message)

    if (stageResult.update) stage += 1

    if (stages.size >= stage)
      stageResult.messages ++ onEndPoll()
    else
      stageResult.messages
  }

  protected def sendToParticipants(fun: BotUser => SendMessage): Seq[SendMessage] = {
    participants.keys
      .map { user => fun(user) }
      .toSeq
  }

  protected def sendPoll(): SendMessage = {
    val markup = InlineKeyboardMarkup.singleRow(Seq(
      InlineKeyboardButton.callbackData("Отправить", ApplyPoll(author.id, sendPoll = true).asJson.spaces2),
      InlineKeyboardButton.callbackData("Отменить", ApplyPoll(author.id, sendPoll = false).asJson.spaces2)))

    SendMessage(author.id, "Отправить опрос?", replyMarkup = Some(markup))
  }

  def vote(voter: BotUser, element: String, vote: Vote): Seq[SendMessage] = {
    val message =
      if (participants(voter))
        SendMessage(voter.id, "Вы уже голосовали")
      else {
        vote.vote(element)
        participants(voter) = true
        SendMessage(voter.id, "Ваш голос принят")
      }

    if (participants.count(p => p._2) == participants.size)
      message +: sendToParticipants { p => SendMessage(p.id, "Выбрано:" + vote.max) }
    else
      message :: Nil
  }

  def next(messages: Seq[SendMessage]): StageResult = {
    StageResult(messages, update = true)
  }

  def keep(messages: Seq[SendMessage]): StageResult = {
    StageResult(messages, update = false)
  }
}
