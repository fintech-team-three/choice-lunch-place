package teamthree.clp.bot.poll

import com.bot4s.telegram.methods.SendMessage
import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser.parse
import teamthree.clp.bot._

case class CuisinePoll(a: BotUser, s: InMemeoryUserBotStorage) extends BasePoll(a, s) {

  private val cuisineVote: Vote = Vote()

  override def onCancelPoll(): Seq[SendMessage] = {
    sendToParticipants { p => SendMessage(p.id, s"${author.username} отменил встречу") }
  }

  override protected def onEndPoll(): Seq[SendMessage] = {
    sendToParticipants { p => SendMessage(p.id, s"Спасибо за участие в опросе") }
  }

  onStage { _ =>
    next(
      SendMessage(author.id, "Введите логины тех с кем вы хотите пойти:") :: Nil
    )
  }

  onStage { message =>
    next(
      addParticipants(message.text) :+ sendPoll()
    )
  }

  onStage { message =>
    next(
      parse(message.text)
        .getOrElse(Json.Null)
        .as[ApplyPoll] match {
        case Right(value: ApplyPoll) =>
          if (value.sendPoll) {

            val toParticipants = sendToParticipants {
              u => SendMessage(u.id, s"${author.username} приглашает вас в кафе, напишите предпочитаемую кухню")
            }

            SendMessage(author.id, s"напишите предпочитаемую кухню") +: toParticipants
          }
          else
            Seq.empty
        case Left(_) => Seq.empty
      }
    )
  }

  onStage { message =>
    keep(
      vote(message.from, message.text, cuisineVote)
    )
  }


}
