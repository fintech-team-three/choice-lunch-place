package teamthree.clp.bot

import com.bot4s.telegram.Implicits._
import com.bot4s.telegram.api._
import com.bot4s.telegram.api.declarative.{Commands, Declarative}
import com.bot4s.telegram.methods.{ParseMode, SendMessage}
import com.bot4s.telegram.models._
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization.{read, write}

import scala.collection.mutable
import scala.io.Source
import scala.util.Try

case class ApplyPoll(authorId: Long, sendPoll: Boolean)

case class PollItem(authorId: Long, value: String)

case class Poll(authorId: Long,
                author: String,
                name: String,
                participants: Map[Long, Boolean],
                places: Map[String, Int])

object Poll {

  def vote(poll: Poll, pollItem: PollItem, voter: Long): Poll = {

    if (poll.participants(voter))
      poll
    else {
      poll.copy(participants = poll.participants + (voter -> true),
        places = poll.places + (pollItem.value -> (poll.places(pollItem.value) + 1)))
    }
  }

  def getResult(poll: Poll): String = {
    poll.places.maxBy(_._2)._1
  }
}

case class PollBuilder(name: String,
                       stage: Int = 0,
                       time: String = "",
                       users: List[Long] = List.empty,
                       places: List[String] = List.empty)

trait CLPBot extends GlobalExecutionContext
  with Declarative
  with Commands {

  lazy val token: String = scala.util.Properties.envOrNone("BOT_TOKEN")
    .getOrElse(Source.fromFile("bot.token").getLines().mkString)

  implicit val formats: DefaultFormats.type = DefaultFormats

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
            case None => reply(s"У пользователя нет диаалога с ботом")(message); None
          }

        val result = b.copy(stage = b.stage + 1, users = users)
        request(SendMessage(message.source, "Введите название кафе:"))
        result
      }
  }

  stages += { (b: PollBuilder, message: Message) =>

    val result = b.copy(stage = b.stage + 1, places = message.text.get.split(',').toList)

    val m = InlineKeyboardMarkup.singleRow(Seq(
      makeInlineButton("Отправить", ApplyPoll(message.source, sendPoll = true)),
      makeInlineButton("Отменить", ApplyPoll(message.source, sendPoll = false))
    ))

    request(SendMessage(message.source, "Отправить опрос?", replyMarkup = m))
    result
  }

  def makeInlineButton[T <: AnyRef](text: String, obj: T): InlineKeyboardButton = {
    InlineKeyboardButton.callbackData(text, write[T](obj))
  }

  def updateStage(message: Message) {
    pollBuilderStorage.findAndUpdate(message.source) { pollBuilder =>
      stages(pollBuilder.stage)(pollBuilder, message)
    }
  }

  def sendPoll(poll: Poll): Unit = {

    val buttons = poll.places.keys
      .map { place => makeInlineButton(place, PollItem(poll.authorId, place)) }
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
      s"""Generates ${"Let me \uD83C\uDDECoogle that for you!".italic} links.
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
        pollBuilderStorage.put(msg.source, PollBuilder(args.head))
      case _ =>
        reply("недопустимый аргумент: название опроса")
    }

    reply("Введите логины тех с кем вы хотите пойти:")
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

    Try {
      callbackQuery.data.map(read[ApplyPoll])
    }.getOrElse(None) match {
      case Some(value) =>
        if (value.sendPoll) {
          val form = pollBuilderStorage.find(value.authorId)

          if (form.isDefined) {
            val poll = Poll(value.authorId, callbackQuery.from.username.get, form.get.name,
              Map(form.get.users.map { u => u -> false }: _*),
              Map(form.get.places.map { u => u -> 0 }: _*))

            pollStorage.put(value.authorId, poll)
            sendPoll(poll)
          }
        }
      case None => ()
    }

    Try {
      callbackQuery.data.map(read[PollItem])
    }.getOrElse(None) match {
      case Some(value) => ()
        pollStorage.findAndUpdate(value.authorId) { poll =>
          request(SendMessage(callbackQuery.from.id.toLong, "Ваш голос засчитан"))
          Poll.vote(poll, value, callbackQuery.from.id.toLong)
        }
      case None => ()
    }
  }
}
