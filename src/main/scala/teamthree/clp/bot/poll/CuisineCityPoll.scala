package teamthree.clp.bot.poll

import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import teamthree.clp.bot._

case class CuisineCityPoll(a: BotUser, s: InMemoryUserBotStorage) extends BasePoll(a, s) {

  private var cuisineVote: Vote = Vote(Seq.empty)
  private var placeVote: Vote = Vote(Seq.empty)
  private var city = ""

  override def onCancelPoll(): Seq[SendMessage] = {
    sendToParticipants { p => SendMessage(p.id, s"${author.username} отменил встречу") }
  }

  override protected def onEndPoll(): Seq[SendMessage] = {
    sendToParticipants { p => SendMessage(p.id, "Спасибо за участие в опросе") } :+
      SendMessage(author.id, "Спасибо за участие в опросе")
  }

  onStage { _ =>
    next { () =>
      SendMessage(author.id, "Отправьте город в котором вы хотите встретиться") :: Nil
    }
  }

  onStage { message =>
    next { () =>
      city = message.text
      SendMessage(author.id, "Введите логины тех с кем вы хотите пойти:") :: Nil
    }
  }

  onStage { message =>
    next { () =>
      addParticipants(message.text) :+ sendPoll()
    }
  }

  onStage { message =>
    next { () =>
      parse(message.text)
        .getOrElse(Json.Null)
        .as[ApplyPoll] match {
        case Right(value: ApplyPoll) =>
          if (value.sendPoll) {

            cuisineVote = Vote(participants :+ author)

            val toParticipants = sendToParticipants {
              u => SendMessage(u.id, s"${author.username} приглашает вас в кафе, напишите предпочитаемую кухню")
            }

            SendMessage(author.id, s"напишите предпочитаемую кухню") +: toParticipants
          }
          else
            Seq.empty
        case Left(_) => Seq.empty
      }
    }
  }

  onStage { message =>
    cuisineVote = cuisineVote.vote(message.from, message.text)
    if (cuisineVote.isVoteEnd) {
      next { () =>

        /** *************************************************/
        //places
        val places = "Кафе 1" :: "Кафе 2" :: "Кафе 3" :: "Кафе 4" :: "Кафе 5" ::
          "Кафе 6" :: "Кафе 7" :: "Кафе 8" :: "Кафе 9" :: "Кафе 10" :: Nil

        /** ************************************************/

        placeVote = Vote(participants :+ author, places)

        val buttons = places
          .map { place => InlineKeyboardButton.callbackData(place, PollItem(author.id, place).asJson.spaces2) }

        val markup = InlineKeyboardMarkup.singleColumn(buttons)

        val toParticipants = sendToParticipants { p =>
          SendMessage(p.id, "В результате голосования выбрано:" + cuisineVote.max)
        } ++ sendToParticipants { u =>
          SendMessage(u.id, "Выберете предпочитаемое кафе", replyMarkup = Some(markup))
        }

        val toAuthor = SendMessage(author.id, s"выберете предпочитаемое кафе", replyMarkup = Some(markup)) ::
          SendMessage(author.id, "В результате голосования выбрано:" + cuisineVote.max) :: Nil

        SendMessage(message.from.id, "Ваш голос принят") :: Nil ++ toParticipants ++ toAuthor
      }
    } else {
      keep { () =>
        SendMessage(message.from.id, "Ваш голос принят") :: Nil
      }
    }
  }

  onStage {
    message =>
      parse(message.text)
        .getOrElse(Json.Null)
        .as[PollItem] match {
        case Right(item: PollItem) =>
          placeVote = placeVote.vote(message.from, item.value)

          if (placeVote.isVoteEnd) {
            next { () =>
              SendMessage(message.from.id, "Ваш голос принят") +:
                SendMessage(author.id, "В результате голосования выбрано:" + placeVote.max) +:
                sendToParticipants {
                  p => SendMessage(p.id, "В результате голосования выбрано:" + placeVote.max)
                }
            }
          } else {
            keep { () => SendMessage(message.from.id, "Ваш голос принят") :: Nil }
          }

        case Left(_) => keep { () => Seq.empty }
      }
  }
}
