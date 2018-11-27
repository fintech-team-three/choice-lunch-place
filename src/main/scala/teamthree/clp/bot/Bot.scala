package teamthree.clp.bot

import com.bot4s.telegram.Implicits._
import com.bot4s.telegram.api.declarative.{Commands, Declarative}
import com.bot4s.telegram.api.{Polling, RequestHandler, TelegramBot, declarative}
import com.bot4s.telegram.clients.SttpClient
import com.bot4s.telegram.methods.{ParseMode, SendMessage}
import com.bot4s.telegram.models._
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp.okhttp.OkHttpFutureBackend
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization.{read, write}
import slogging.{LogLevel, LoggerConfig, PrintLoggerFactory}

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Try


case class ApplyPoll(authorId: Long, sendSurvey: Boolean)

case class PollItem(authorId: Long, item: String)

case class Poll(authorId: Long, participants: Map[Long, Boolean], places: Map[String, Int])

object Poll {

  def vote(poll: Poll, item: PollItem, voter: Long): Poll = {

    if (poll.participants(voter))
      poll
    else {
      poll.copy(participants = poll.participants + (voter -> true),
        places = poll.places + (item.item -> (poll.places(item.item) + 1)))
    }
  }
}

case class PollBuilder(stage: Int = 0, time: String = "", users: List[Long] = List.empty, places: List[String] = List.empty)

abstract class BaseBot(val token: String) extends TelegramBot {
  LoggerConfig.factory = PrintLoggerFactory()
  LoggerConfig.level = LogLevel.TRACE

  implicit val backend: SttpBackend[Future, Nothing] = OkHttpFutureBackend()
  override val client: RequestHandler = new SttpClient(token)

  val userStorage: Storage[String, Long] = InMemoryStorage[String, Long]()
  val pollStorage: Storage[Long, Poll] = InMemoryStorage[Long, Poll]()
  val pollBuilderStorage: Storage[Long, PollBuilder] = InMemoryStorage[Long, PollBuilder]()

  val stages: mutable.MutableList[(PollBuilder, Message) => PollBuilder] = mutable.MutableList[(PollBuilder, Message) => PollBuilder]()
}

class CLPBot(token: String) extends BaseBot(token)
  with Polling
  with Declarative
  with Commands {

  implicit val formats: DefaultFormats.type = DefaultFormats

  //  stages += {
  //    (b: PollBuilder, message: Message) =>
  //      message.text match {
  //        case Some(value) =>
  //          val result = b.copy(stage = b.stage + 1, time = value)
  //          request(SendMessage(message.source, "Введите логины тех с кем вы хотите пойти:"))
  //          result
  //        case None => b
  //      }
  //  }

  def makeInlineButton[T <: AnyRef](text: String, obj: T): InlineKeyboardButton = {
    InlineKeyboardButton.callbackData(text, write[T](obj))
  }

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
            case None => reply(s"У пользователя нет диаалога с ботом")(message); None
          }

        val result = b.copy(stage = b.stage + 1, users = users)
        request(SendMessage(message.source, "Введите название кафе:"))
        result
      }
  }

  stages += { (b: PollBuilder, message: Message) =>

    val result = b.copy(b.stage + 1, places = message.text.get.split(',').toList)

    val m = InlineKeyboardMarkup.singleRow(Seq(
      makeInlineButton("Отправить", ApplyPoll(message.source, sendSurvey = true)),
      makeInlineButton("Отменить", ApplyPoll(message.source, sendSurvey = false))
    ))

    request(SendMessage(message.source, "Отправить опрос", replyMarkup = m))
    result
  }

  def updateStage(message: Message) {
    pollBuilderStorage.findAndUpdate(message.source) { pollBuilder =>
      stages(pollBuilder.stage)(pollBuilder, message)
    }
  }

  def sendSurvey(survey: Poll): Unit = {

    val buttons = survey.places.keys
      .map { place => makeInlineButton(place, PollItem(survey.authorId, place)) }
      .toSeq

    val m = InlineKeyboardMarkup.singleColumn(buttons)

    for {
      u <- survey.participants.keys
      m <- SendMessage(u, "" + "приглашает вас в одно из этих мест:", replyMarkup = m)
    } yield request(m)

  }

  onCommand('newpoll) { implicit msg =>
    pollBuilderStorage.put(msg.from.get.id, PollBuilder())
    reply("Введите логины тех с кем вы хотите пойти:")
  }

  onCommand('cancel) { implicit msg =>
    pollBuilderStorage.remove(msg.source)
  }

  onCommand('status) { implicit msg =>

    pollStorage.find(msg.source) match {
      case Some(value) =>
        reply("Результаты:\n" + value.places.map(p => p._1 + ": " + p._2).mkString("\n"))

      case None => reply("Сначала создайте опрос")
    }

  }

  def notACommand(msg: Message): Boolean =
    msg.text.exists(m => !m.startsWith("/"))

  declarative.when(onMessage, notACommand) {
    implicit msg =>

      updateStage(msg)
  }

  onCommand('start) { implicit msg =>

    msg.from.flatMap { u => u.username } match {
      case Some(username) =>
        userStorage.put("@" + username, msg.source)
      case None =>
        reply("Для корректной работы пожалуйста добавьте имя пользователя(username)")
    }

    reply(
      s"""Generates ${"Let me \uD83C\uDDECoogle that for you!".italic} links.
         |
             |/start - list commands
         |
             |/newpoll - новый опрос
         |
             |/status - статус опроса
         |
             |@Bot args - Inline mode
          """.stripMargin,
      parseMode = ParseMode.Markdown)
  }

  override def receiveCallbackQuery(callbackQuery: CallbackQuery): Unit = {

    Try {
      callbackQuery.data.map(read[ApplyPoll])
    }.getOrElse(None) match {
      case Some(value) =>
        if (value.sendSurvey) {
          val form = pollBuilderStorage.find(value.authorId)

          if (form.isDefined) {

            val surv = Poll(value.authorId, Map(form.get.users.map { u => u -> false }: _*),
              Map(form.get.places.map { u => u -> 0 }: _*))

            pollStorage.put(value.authorId, surv)
            sendSurvey(surv)
          }
        }
      case None => ()
    }

    Try {
      callbackQuery.data.map(read[PollItem])
    }.getOrElse(None) match {
      case Some(value) => ()
        pollStorage.findAndUpdate(value.authorId) { poll =>
          Poll.vote(poll, value, callbackQuery.from.id.toLong)
        }
      case None => ()
    }
  }
}

