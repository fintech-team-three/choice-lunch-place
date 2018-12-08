package teamthree.clp.bot

import scala.collection.mutable

/**
  * Отправить опрос или нет
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
  * @param id id беседы пользователя с ботом
  * @param username имя пользователя
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
  * @param from пользователь от которого отправлено сообщение
  * @param text текст сообщения
  */
case class InputMessage(from: BotUser, text: String)

/**
  * Простой опрос
  *
  * @param el список пунктов опроса
  */
case class Vote(private val el: Seq[String] = Seq.empty) {

  private val items = mutable.Map(el.map(e => e -> 0): _*)

  /**
    *
    * @return список пунктов опроса
    */
  def elements(): Seq[String] ={
    items.keys.toSeq
  }

  /**
    * Голосование за один из пунктов опроса
    *
    * @param element пунккт опроса, если отсутствует, то будет добавлен в опрос
    */
  def vote(element: String): Unit = {
    if (items.contains(element))
      items(element) += 1
    else
      items += element -> 1
  }

  /**
    * @return Пункт опроса с максимальным количеством голосов
    */
  def max: String = {
    items.maxBy(_._2)._1
  }
}
