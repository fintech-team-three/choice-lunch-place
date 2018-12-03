package teamthree.clp.bot

import scala.collection.mutable

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