package teamthree.clp.bot

import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser.parse
import io.circe.syntax._

case class CuisinePoll(storages: Storages, a: NUser) extends BasePoll(a) {

  private val cuisineVote: Vote = Vote()

  onNextStage { (from: Long, msg: String) =>
    SendMessage(author.id, "Введите логины тех с кем вы хотите пойти:") :: Nil
  }

  onNextStage { (_: Long, msg: String) =>
    val users = "@[A-Za-z_]+".r.findAllIn(msg).toList

    if (users.isEmpty)
      SendMessage(author.id, "Введите хотя бы одно имя пользователя")

    val messages = users.flatMap { u =>
      storages.userStorage.find(u) match {
        case Some(user: NUser) =>
          if (user.pollAuthor == NUser.NOT_IN_POLL) {
            storages.userStorage.map(user.id) { u => u.copy(pollAuthor = author.id) }
            addParticipant(user)
            None
          } else {
            Some(SendMessage(author.id, s"Пользователь $u уже участвует в каком либо опросе"))
          }
        case None => Some(SendMessage(author.id, s"У пользователя $u нет диалога с ботом"))
      }
    }

    val markup = InlineKeyboardMarkup.singleRow(Seq(
      InlineKeyboardButton.callbackData("Отправить", ApplyPoll(author.id, sendPoll = true).asJson.spaces2),
      InlineKeyboardButton.callbackData("Отменить", ApplyPoll(author.id, sendPoll = false).asJson.spaces2)))

    SendMessage(author.id, "Отправить опрос?", replyMarkup = Some(markup)) :: messages
  }

  onNextStage { (_: Long, msg: String) =>
    val json = parse(msg).getOrElse(Json.Null)

    json.as[ApplyPoll] match {
      case Right(value: ApplyPoll) =>
        if (value.sendPoll) {

          val toParticipants = sendToParticipants { u => SendMessage(u.id, s"${author.username} приглашает вас в кафе, напишите предпочитаемую кухню") }
          val toAuthor = SendMessage(author.id, s"напишите предпочитаемую кухню")

          toAuthor +: toParticipants
        }
        else
          SendMessage(author.id, "Попробуйте еще раз ") :: Nil
      case Left(_) => SendMessage(author.id, "Error") :: Nil
    }
  }

  onNextStage { (from: Long, msg: String) =>
    allowUpdateStage = false
    vote(from, msg, cuisineVote)

  }
}
