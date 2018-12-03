package teamthree.clp.bot

import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup, ReplyMarkup}
import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser.parse
import io.circe.syntax._

import scala.collection.mutable

case class SimplePoll(storages: Storages, a: NUser) extends BasePoll(a) {

  private val participants = mutable.MutableList.empty[NUser]

  private var placeVote: Vote = Vote()

  onNextStage { (_: Long, _: String) =>
    SendMessage(author.id, "Введите логины тех с кем вы хотите пойти:") :: Nil
  }

  onNextStage { (_: Long, msg: String) =>
    val users = "@[A-Za-z_]+".r.findAllIn(msg).toList

    if (users.isEmpty)
      SendMessage(author.id, "Введите хотя бы одно имя пользователя")

    val messages = users.flatMap { u =>
      storages.userStorage.find(u) match {
        case Some(user: NUser) => participants += user; None
        case None => Some(SendMessage(author.id, s"У пользователя $u нет диалога с ботом"))
      }
    }

    SendMessage(author.id, "Введите название кафе:") +: messages
  }

  onNextStage { (_: Long, msg: String) =>

    placeVote = Vote(msg.split(','))

    val markup: ReplyMarkup = InlineKeyboardMarkup.singleRow(Seq(
      InlineKeyboardButton.callbackData("Отправить", ApplyPoll(author.id, sendPoll = true).asJson.spaces2),
      InlineKeyboardButton.callbackData("Отменить", ApplyPoll(author.id, sendPoll = false).asJson.spaces2)))

    SendMessage(author.id, "Отправить опрос?", replyMarkup = Some(markup)) :: Nil
  }

  onNextStage { (_: Long, msg: String) =>

    val json = parse(msg).getOrElse(Json.Null)

    json.as[ApplyPoll] match {
      case Right(value: ApplyPoll) =>
        if (value.sendPoll) {

          val toParticipants = sendToParticipants { u => SendMessage(u.id, s"${author.username} приглашает вас в кафе, выберете предпочитаемое кафе") }

          val toAuthor = SendMessage(author.id, s"выберете предпочитаемое кафе")

          toAuthor +: toParticipants
        }
        else
          SendMessage(author.id, "Error") :: Nil
      case Left(_) => SendMessage(author.id, "Error") :: Nil
    }
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

          val toParticipants = {
            for {
              p <- participants
            } yield SendMessage(p.id, s"${author.username} приглашает вас пойти с ним в одно из этих мест:", replyMarkup = Some(markup))
          }.toList

          val toAuthor = SendMessage(author.id, s"напишите предпочитаемую кухню")

          toAuthor :: toParticipants
        }
        else {
          SendMessage(author.id, "Попробуйте еще раз ") :: Nil
        }
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

  //  onNextStage { (from: Long, msg: String) =>
  //    vote(from, msg, cuisineVote)
  //  }
}
