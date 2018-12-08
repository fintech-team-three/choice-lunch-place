package teamthree.clp.bot

import scala.collection.mutable

/**
  * Отправить опрос или нет
  *
  * @param authorId автор опроса
  * @param sendPoll подтверждение отправки
  */
case class ApplyPoll(authorId: Long, sendPoll: Boolean)

/**
  *
  * @param authorId
  * @param value
  */
case class PollItem(authorId: Long, value: String)

/**
  * Пользователь бота
  *
  * @param id         id беседы пользователя с ботом
  * @param username   имя пользователя
  * @param pollAuthor находится ли пользователь в опросе,
  *                   если да то равно [[BotUser.id]] автора,
  *                   если нет то [[teamthree.clp.bot.BotUser.NOT_IN_POLL]]
  */
case class BotUser(id: Long, username: String, pollAuthor: Long = BotUser.NOT_IN_POLL)

object BotUser {
  val NOT_IN_POLL: Long = -1L
}

/**
  * Сообщение от пользователя
  *
  * @param from пользователь от которого отправлено сообщение
  * @param text текст сообщения
  */
case class InputMessage(from: BotUser, text: String)

/**
  * Простой опрос
  *
  * @param el список пунктов опроса
  */
case class Vote private(private val participants: Map[BotUser, Boolean],
                        private val items: mutable.Map[String, Int]) {

  /**
    *
    * @return список пунктов опроса
    */
  def elements(): Seq[String] = {
    items.keys.toSeq
  }

  /**
    * Голосование за один из пунктов опроса
    *
    * @param voter   голосующий
    * @param element пункт за который голосуют
    * @return обновленый опрос
    */
  def vote(voter: BotUser, element: String): Vote = {
    if (participants(voter))
      this
    else {
      Vote(participants + (voter -> true),
        items + (element -> (items.getOrElse(element, 0) + 1)))
    }
  }

  /**
    * Все ли проголосовали
    * @return да или нет
    */
  def isVoteEnd: Boolean = {
    participants.values.groupBy(identity).size == participants.size
  }

  /**
    * @return Пункт опроса с максимальным количеством голосов
    */
  def max: String = {
    items.maxBy(_._2)._1
  }
}

object Vote {
  def apply(participants: Seq[BotUser], elements: Seq[String] = Seq.empty): Vote =
    new Vote(Map(participants.map(p => p -> false): _*),
      mutable.Map(elements.map(e => e -> 0): _*))
}
