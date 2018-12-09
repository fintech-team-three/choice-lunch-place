package teamthree.clp.bot.poll

import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import teamthree.clp.bot._

case class ChoicePollPoll(a: BotUser,
                          us: InMemoryUserBotStorage,
                          pollStorage: InMemoryStorage[Long, BasePoll]) extends BasePoll(a, us) {
  /**
    * Сообщения отправляемые при отмене опроса
    *
    * @return Список отправлемых сообщений
    */
  override protected def onCancelPoll(): Seq[SendMessage] = Seq.empty

  /**
    * Сообщения отправляемые при завершении опроса
    *
    * @return Список отправлемых сообщений
    */
  override protected def onEndPoll(): Seq[SendMessage] = Seq.empty

  private val pollMap = Map(
    "Простой выбор" -> SimplePoll,
    "Рядом с вами" -> CuisineLocationPoll,
    "В городе" -> CuisineCityPoll)

  onStage { _ =>
    next { () =>

      val buttons = pollMap.keys.zipWithIndex
        .map { place => InlineKeyboardButton.callbackData(place._1, PollItem(author.id, place._2.toString).asJson.spaces2) }
        .toSeq

      val markup = InlineKeyboardMarkup.singleColumn(buttons)

      SendMessage(author.id, s"выберете тип опроса", replyMarkup = Some(markup)) :: Nil
    }
  }

  onStage { message =>
    next { () =>
      parse(message.text)
        .getOrElse(Json.Null)
        .as[PollItem] match {
        case Right(item: PollItem) =>

          endPoll()

          val values = pollMap.values.toSeq
          val poll = values(item.value.toInt)(author, userStorage)

          pollStorage.put(author.id, poll)
          userStorage.map(author.id) { u => u.copy(pollAuthor = u.id) }

          poll.nextStage(InputMessage(author, ""))

        case Left(_) => Seq.empty
      }
    }
  }
}
