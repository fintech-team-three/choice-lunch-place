package teamthree.clp.bot.poll

import com.bot4s.telegram.methods.SendMessage
import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser._
import teamthree.clp.bot._

case class CuisinePoll(a: BotUser, s: Storages) extends BasePoll(a, s) {

  private val cuisineVote: Vote = Vote()

  override def onCancelPoll(): Seq[SendMessage] = {
    sendToParticipants { p => SendMessage(p.id, s"${author.username} отменил встречу") }
  }

  onNextStage { message =>
    SendMessage(author.id, "Введите логины тех с кем вы хотите пойти:") :: Nil
  }

  onNextStage { message =>
    addParticipants(message.text) :+ sendPoll()
  }

  onNextStage { message =>
    val json = parse(message.text).getOrElse(Json.Null)

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

  onNextStage { message =>
    allowUpdateStage = false
    vote(message.from, message.text, cuisineVote)
  }

}
