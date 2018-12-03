package teamthree.clp.bot

import com.bot4s.telegram.methods.SendMessage

import scala.collection.mutable

abstract class BasePoll(val author: NUser) {
  type Action = (Long, String) => Seq[SendMessage]

  private var stage = 0

  private val stages = mutable.MutableList[Action]()

  protected var allowUpdateStage = true

  private val participants = mutable.Map.empty[NUser, Boolean]


  def nextStage(from: Long, message: String): Seq[SendMessage] = {
    val messages = stages(stage)(from, message)

    if (allowUpdateStage)
      stage += 1
    else
      allowUpdateStage = true

    messages
  }

  def onNextStage(action: Action): Unit = {
    stages += action
  }

  def addParticipant(user: NUser): Unit = {
    participants += user -> false
  }

  def sendToParticipants(fun: NUser => SendMessage): Seq[SendMessage] = {
    participants.keys
      .map { user => fun(user) }
      .toSeq
  }

  def vote(from: Long, element: String, vote: Vote): Seq[SendMessage] = {
    val messages = participants.filter(u => u._1.id == from).map {
      case (u: NUser, isVote: Boolean) =>
        if (isVote)
          SendMessage(u.id, "Вы уже голосовали")
        else {
          vote.vote(element)
          SendMessage(u.id, "Ваш голос принят")
        }
    }.toSeq

    if (participants.count(p => p._2) == participants.size)
      sendToParticipants { p => SendMessage(p.id, "Выбрано:" + vote.max) } ++: messages
    else
      messages
  }
}


case class Vote(private val items: mutable.Map[String, Int] = mutable.Map.empty[String, Int]) {

  def elements(): Iterable[String] = {
    items.keys
  }

  def vote(element: String): Unit = {
    if (items.contains(element))
      items(element) += 1
    else
      items += element -> 0
  }

  def max: String = {
    items.maxBy(_._2)._1
  }
}

object Vote {
  def apply(data: Seq[String]): Vote = new Vote(mutable.Map(data.map(e => e -> 0): _*))
}