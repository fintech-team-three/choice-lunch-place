package teamthree.clp.bot

case class Poll(authorId: Long,
                author: String,
                name: String,
                participants: Map[Long, Boolean],
                places: Map[String, Int])

object Poll {

  def vote(poll: Poll, pollItem: PollItem, voter: Long): Poll = {

    if (poll.participants(voter))
      poll
    else {
      poll.copy(participants = poll.participants + (voter -> true),
        places = poll.places + (pollItem.value -> (poll.places(pollItem.value) + 1)))
    }
  }

  def getResult(poll: Poll): String = {
    poll.places.maxBy(_._2)._1
  }

  def makePoll(builder: PollBuilder): Poll = {
    Poll(builder.authorId,
      builder.author,
      builder.name,
      Map(builder.participants.map { u => u -> false }: _*),
      Map(builder.places.map { u => u -> 0 }: _*))
  }
}

case class PollBuilder(authorId: Long,
                       author: String,
                       name: String,
                       stage: Int = 0,
                       time: String = "",
                       participants: List[Long] = List.empty,
                       places: List[String] = List.empty)