package teamthree.clp.bot.poll

import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import teamthree.clp.bot._

case class SimplePoll(a: NUser, s: Storages) extends BasePoll(a, s) {

  private var placeVote: Vote = Vote()

  override def onCancelPoll(): Seq[SendMessage] = {
    sendToParticipants { p => SendMessage(p.id, s"${author.username} отменил встречу") }
  }

  onNextStage { (_: Long, _: String) =>
    SendMessage(author.id, "Введите логины тех с кем вы хотите пойти:") :: Nil
  }

  onNextStage { (_: Long, msg: String) =>
    addParticipants(msg) :+ SendMessage(author.id, "Введите название кафе:")
  }

  onNextStage { (_: Long, msg: String) =>

    placeVote = Vote(msg.split(','))

    sendPoll() :: Nil
  }

  onNextStage { (_: Long, msg: String) =>

    val json = parse(msg).getOrElse(Json.Null)

    json.as[ApplyPoll] match {
      case Right(value: ApplyPoll) =>
        if (value.sendPoll) {

          val buttons = placeVote.elements()
            .map { place => InlineKeyboardButton.callbackData(place, PollItem(author.id, place).asJson.spaces2) }
            .toSeq

          val markup = InlineKeyboardMarkup.singleColumn(buttons)

          val toParticipants = sendToParticipants { u => SendMessage(u.id, s"${author.username} приглашает вас в кафе, выберете предпочитаемое кафе", replyMarkup = Some(markup)) }

          val toAuthor = SendMessage(author.id, s"выберете предпочитаемое кафе", replyMarkup = Some(markup))

          toAuthor +: toParticipants
        }
        else
          SendMessage(author.id, "Error") :: Nil
      case Left(_) => SendMessage(author.id, "Error") :: Nil
    }
  }

  onNextStage { (from: Long, msg: String) =>

    val json = parse(msg).getOrElse(Json.Null)

    json.as[PollItem] match {
      case Right(value: PollItem) =>
        allowUpdateStage = false
        vote(from, value.value, placeVote)
      case Left(_) => SendMessage(author.id, "Error") :: Nil
    }
  }
}
