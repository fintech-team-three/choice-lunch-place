package teamthree.clp.bot

import com.bot4s.telegram.Implicits._
import com.bot4s.telegram.api._
import com.bot4s.telegram.api.declarative.{Commands, Declarative}
import com.bot4s.telegram.methods.{ParseMode, SendMessage}
import com.bot4s.telegram.models.{InlineKeyboardButton, _}
import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

import scala.collection.mutable
import scala.io.Source

case class ApplyPoll(authorId: Long, sendPoll: Boolean)

case class PollItem(authorId: Long, value: String)

case class NUser(id: Long, username: String, pollAuthor: Long = NUser.NOT_IN_POLL)

object NUser {
  val NOT_IN_POLL: Long = -1L
}

trait Storages {
  val userStorage = NInMemoryStorage()
}


object Messages {
  val PLEASE_SEND_START_COMMAND = "Пожалуйста отправьте команду /start для инициализации бота."
}

trait NBot extends GlobalExecutionContext
  with Declarative
  with Commands
  with Storages {

  import declarative.when

  lazy val token: String = scala.util.Properties.envOrNone("BOT_TOKEN")
    .getOrElse(Source.fromFile("bot.token").getLines().mkString)

  private val pollBuilderStorage = InMemoryStorage[User, PollBuilder]()

  private val pollStorage = InMemoryStorage[Long, BasePoll]()

  onCommand('start) { implicit msg =>
    msg.from.flatMap { u => u.username } match {
      case Some(username) =>
        userStorage.put(NUser(msg.source, "@" + username))

        reply(
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
          """.stripMargin,
          parseMode = ParseMode.Markdown)

      case None =>
        reply("Для корректной работы пожалуйста добавьте имя пользователя(username)")
    }
  }

  onCommand('newpoll) { implicit msg =>
    userStorage.find(msg.source) match {
      case Some(user) =>

        if (user.pollAuthor == NUser.NOT_IN_POLL)
          reply("Одновременно допускается только один опрос.")
        else {
          val poll = CuisinePoll(this, user)
          pollStorage.put(user.id, poll)
          userStorage.map(user.id) { u => u.copy(pollAuthor = u.id) }

          sendMessages(poll.nextStage(msg.source, msg.text.get))
        }
      case None =>
    }
  }

  onCommand('newsimplepoll) { implicit msg =>
    userStorage.find(msg.source) match {
      case Some(user) =>

        if (user.pollAuthor == NUser.NOT_IN_POLL)
          reply("Одновременно допускается только один опрос.")
        else {
          val poll = SimplePoll(this, user)
          pollStorage.put(user.id, poll)
          userStorage.map(user.id) { u => u.copy(pollAuthor = u.id) }

          sendMessages(poll.nextStage(msg.source, msg.text.get))
        }
      case None =>
    }
  }

  def sendMessages(messages: Seq[SendMessage]): Unit = {
    messages.foreach { sm => request(sm) }
  }

  def notACommand(msg: Message): Boolean =
    msg.text.exists(m => !m.startsWith("/"))

  when(onMessage, notACommand) { implicit msg =>
    userStorage.find(msg.source) match {
      case Some(user) =>
        pollStorage.find(user.pollAuthor) match {
          case Some(npoll) =>
            sendMessages(npoll.nextStage(msg.source, msg.text.get))
          case None => ()
        }
      case None => ()
    }
  }

  override def receiveCallbackQuery(callbackQuery: CallbackQuery): Unit = {
    userStorage.find(callbackQuery.from.id.toLong) match {
      case Some(user) =>
        pollStorage.find(user.pollAuthor) match {
          case Some(npoll) =>
            sendMessages(npoll.nextStage(callbackQuery.from.id.toLong, callbackQuery.data.get))
          case None => ()
        }
      case None => ()
    }
  }
}

trait CLPBot extends GlobalExecutionContext
  with Declarative
  with Commands {

  lazy val token: String = scala.util.Properties.envOrNone("BOT_TOKEN")
    .getOrElse(Source.fromFile("bot.token").getLines().mkString)

  private val userStorage = InMemoryStorage[String, Long]()
  private val pollStorage = InMemoryStorage[Long, Poll]()
  private val pollBuilderStorage = InMemoryStorage[Long, PollBuilder]()

  private val stages = mutable.MutableList[(PollBuilder, Message) => PollBuilder]()

  stages += {
    (b: PollBuilder, message: Message) =>

      if (message.text.isEmpty) {
        request(SendMessage(message.source, "Добавте хотя бы однин логин"))
        b
      }
      else {
        val users = "@[A-Za-z_]+".r.findAllIn(message.text.get)
          .toList
          .map(u => userStorage.find(u))
          .flatMap {
            case Some(value) => Some(value)
            case None => reply(s"У пользователя нет диалога с ботом")(message); None
          }

        if (users.isEmpty) {
          reply("Введите хотя бы одно имя пользователя:")(message)
          b
        }
        else {
          val result = b.copy(stage = b.stage + 1, participants = users)
          reply("Введите название кафе:")(message)
          result
        }
      }
  }

  stages += { (b: PollBuilder, message: Message) =>

    val result = b.copy(stage = b.stage + 1, places = message.text.get.split(',').toList)

    val m = InlineKeyboardMarkup.singleRow(Seq(
      makeInlineButton("Отправить", ApplyPoll(message.source, sendPoll = true).asJson.spaces2),
      makeInlineButton("Отменить", ApplyPoll(message.source, sendPoll = false).asJson.spaces2)
    ))

    request(SendMessage(message.source, "Отправить опрос?", replyMarkup = m))
    result
  }

  def makeInlineButton[T](text: String, obj: String): InlineKeyboardButton = {
    InlineKeyboardButton.callbackData(text, obj)
  }

  def updateStage(message: Message) {
    pollBuilderStorage.findAndUpdate(message.source) { pollBuilder =>
      stages(pollBuilder.stage)(pollBuilder, message)
    }
  }

  def sendPoll(poll: Poll): Unit = {

    val buttons = poll.places.keys
      .map { place => makeInlineButton(place, PollItem(poll.authorId, place).asJson.spaces2) }
      .toSeq

    val m = InlineKeyboardMarkup.singleColumn(buttons)

    for {
      p <- poll.participants.keys
      message <- SendMessage(p, s"${poll.author} приглашает вас на ${poll.name} в одно из этих мест:", replyMarkup = m)
    } yield request(message)

  }

  onCommand('start) { implicit msg =>

    msg.from.flatMap { u => u.username } match {
      case Some(username) =>
        userStorage.put("@" + username, msg.source)
      case None =>
        reply("Для корректной работы пожалуйста добавьте имя пользователя(username)")
    }

    reply(
      s"""Выбор места для обеда.
         |
             |/start - list commands
         |
             |/newpoll [pollname]- новый опрос
         |
             |/status [pollname] - статус опроса
         |
             |@Bot args - Inline mode
          """.stripMargin,
      parseMode = ParseMode.Markdown)
  }

  onCommand('newpoll) { implicit msg =>
    withArgs {
      case args if args.length == 1 =>
        using(_.from.flatMap(u => u.username)) { username =>
          pollBuilderStorage.put(msg.source, PollBuilder(msg.source, username, args.head))
          reply("Введите логины тех с кем вы хотите пойти:")
        }
      case _ =>
        reply("недопустимый аргумент: название опроса")
    }
  }

  onCommand('endpoll) { implicit msg =>
    withArgs {
      case args if args.length == 1 =>

        pollStorage.find(msg.source) match {
          case Some(poll) =>
            val replyString = "В результате голосования выбрано: " + Poll.getResult(poll)

            reply(replyString)

            for {
              p <- poll.participants.keys
              message <- SendMessage(p, replyString)
            } yield request(message)

          case None => reply("Сначала создайте опрос")
        }

      case _ =>
        reply("недопустимый аргумент: название опроса")
    }
  }

  onCommand('cancel) { implicit msg =>
    pollBuilderStorage.remove(msg.source)
  }

  onCommand('status) { implicit msg =>
    withArgs {
      case args if args.length == 1 =>

        pollStorage.find(msg.source) match {
          case Some(value) =>
            reply("Результаты \"" + value.name + "\" :\n" + value.places.map(p => p._1 + ": " + p._2).mkString("\n"))

          case None => reply("Сначала создайте опрос")
        }

      case _ =>
        reply("недопустимый аргумент: название опроса")
    }
  }

  def notACommand(msg: Message): Boolean =
    msg.text.exists(m => !m.startsWith("/"))

  declarative.when(onMessage, notACommand) {
    implicit msg =>
      updateStage(msg)
  }

  /*
   Получение ответов от кнопок
    */
  override def receiveCallbackQuery(callbackQuery: CallbackQuery): Unit = {

    val json = callbackQuery.data.flatMap(parse).getOrElse(Json.Null)

    json match {
      case Right(value: ApplyPoll) =>
        if (value.sendPoll) {
          pollBuilderStorage.find(value.authorId) match {
            case Some(builder) =>
              val poll = Poll.makePoll(builder)
              pollStorage.put(value.authorId, poll)
              pollBuilderStorage.remove(value.authorId)
              sendPoll(poll)
            case None => ()
          }
        }
      case Left(_) => ()
    }

    json match {
      case Right(value: PollItem) =>
        pollStorage.findAndUpdate(value.authorId) { poll =>
          request(SendMessage(callbackQuery.from.id.toLong, "Ваш голос засчитан"))
          Poll.vote(poll, value, callbackQuery.from.id.toLong)
        }
      case Left(_) => ()
    }
  }
}
