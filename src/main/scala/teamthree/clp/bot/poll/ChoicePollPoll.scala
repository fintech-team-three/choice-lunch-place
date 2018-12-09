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
    """
      | Простой выбор
      |
      | Вы приглашаете людей и предлогаете им пойти
      | в одно из предложенных вами место
    """.stripMargin -> SimplePoll,
    """
      | Рядом с вами
      |
      | Вы приглашаете людей и предлогаете им выбрать
      | предпочитаемую кухню и проголосовать за одно из предложенных кафе
      | рядом с вами
    """.stripMargin -> CuisineLocationPoll,
    """
      | В городе
      |
      | Вы приглашаете людей и предлогаете им выбрать
      | предпочитаемую кухню и проголосовать за одно из предложенных кафе
      | в городе
    """.stripMargin -> CuisineLocationPoll)

  onStage { _ =>
    next { () =>

      val buttons = pollMap.keys
        .map { place => InlineKeyboardButton.callbackData(place, PollItem(author.id, place).asJson.spaces2) }
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

          val poll = pollMap(item.value)(author, userStorage)
          pollStorage.put(author.id, poll)
          userStorage.map(author.id) { u => u.copy(pollAuthor = u.id) }

          poll.nextStage(InputMessage(author, ""))

        case Left(_) => Seq.empty
      }
    }
  }
}
