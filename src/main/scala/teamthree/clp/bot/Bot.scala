package teamthree.clp.bot

import com.bot4s.telegram.Implicits._
import com.bot4s.telegram.api._
import com.bot4s.telegram.api.declarative.{Commands, Declarative}
import com.bot4s.telegram.methods.{ParseMode, SendMessage}
import com.bot4s.telegram.models._
import teamthree.clp.bot.poll.{BasePoll, CuisinePoll, SimplePoll}

import scala.io.Source

case class ApplyPoll(authorId: Long, sendPoll: Boolean)

case class PollItem(authorId: Long, value: String)

case class InputMessage(from: BotUser, text: String)

case class BotUser(id: Long, username: String, pollAuthor: Long = BotUser.NOT_IN_POLL)

object BotUser {
  val NOT_IN_POLL: Long = -1L
}

trait Storages {
  val userStorage = InMemeoryUserBotStorage()
}


object BotMessages {
  val PLEASE_SEND_START_COMMAND = "Пожалуйста отправьте команду /start для инициализации бота."

  val PLEASE_ADD_USERNAME = "Для корректной работы пожалуйста добавьте имя пользователя(username)"

  val ONLY_ONE_POLL_AT_TIME = "Одновременно допускается только один опрос."

  val START_AND_HELP: String =
    s"""Выбор места для обеда.
       |
             |/start - list commands
       |
             |/newpoll - новый опрос
       |
             |/newsimplepoll - новый опрос
       |
             |/status - статус опроса
       |
             |@Bot args - Inline mode
          """.stripMargin
}

trait CLPBot extends GlobalExecutionContext
  with Declarative
  with Commands
  with Storages {

  import declarative.when

  lazy val token: String = scala.util.Properties.envOrNone("BOT_TOKEN")
    .getOrElse(Source.fromFile("bot.token").getLines().mkString)

  private val pollStorage = InMemoryStorage[Long, BasePoll]()

  onCommand('start) { implicit msg =>
    msg.from.flatMap { u => u.username } match {
      case Some(username) =>
        userStorage.put(BotUser(msg.source, "@" + username))
        reply(BotMessages.START_AND_HELP, parseMode = ParseMode.Markdown)
      case None =>
        reply(BotMessages.PLEASE_ADD_USERNAME)
    }
  }

  onCommand('newpoll) { msg =>
    //TODO: add error checking
    createPoll(msg.source, msg.text.get) { from => CuisinePoll(from, this) }
  }

  onCommand('newsimplepoll) { msg =>
    //TODO: add error checking
    createPoll(msg.source, msg.text.get) { from => SimplePoll(from, this) }
  }

  onCommand('cancel) { implicit msg =>
    pollStorage.find(msg.source) match {
      case Some(poll) =>
        sendMessages(poll.cancelPoll())
        pollStorage.remove(msg.source)
      case None => reply("Сначала создайте опрос")
    }
  }

  when(onMessage, notACommand) { msg =>
    //TODO: add error checking
    userStorage.find(msg.source) match {
      case Some(user) =>
        updatePoll(InputMessage(user, msg.text.get))
      case None =>
    }
  }

  override def receiveCallbackQuery(callbackQuery: CallbackQuery): Unit = {
    //TODO: add error checking
    userStorage.find(callbackQuery.from.id) match {
      case Some(user) =>
        updatePoll(InputMessage(user, callbackQuery.data.get))
      case None =>
    }
  }

  def createPoll(from: Long, message: String)(creator: BotUser => BasePoll): Unit = {
    userStorage.find(from) match {
      case Some(user) =>
        if (user.pollAuthor != BotUser.NOT_IN_POLL)
          request(SendMessage(from, BotMessages.ONLY_ONE_POLL_AT_TIME))
        else {
          val poll = creator(user)

          pollStorage.put(user.id, poll)

          userStorage.map(user.id) { u => u.copy(pollAuthor = u.id) }

          sendMessages(poll.nextStage(InputMessage(user, message)))
        }

      case None => request(SendMessage(from, BotMessages.PLEASE_SEND_START_COMMAND))
    }
  }

  def updatePoll(message: InputMessage): Unit = {
    pollStorage.find(message.from.pollAuthor) match {
      case Some(poll: BasePoll) =>
        sendMessages(poll.nextStage(message))
      case None => request(SendMessage(message.from.id, BotMessages.START_AND_HELP))
    }
  }

  def sendMessages(messages: Seq[SendMessage]): Unit = {
    messages.foreach { sm => request(sm) }
  }

  def notACommand(msg: Message): Boolean =
    msg.text.exists(m => !m.startsWith("/"))
}
