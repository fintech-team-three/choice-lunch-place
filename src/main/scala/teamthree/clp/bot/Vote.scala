package teamthree.clp.bot

import scala.collection.mutable


/**
  * Простой опрос
  *
  * @param elements список пунктов опроса
  */
case class Vote(elements: Seq[String] = Seq.empty) {

  private val items = mutable.Map(elements.map(e => e -> 0): _*)

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
    * @return Элемент опроса с максимальным количеством голосов
    */
  def max: String = {
    items.maxBy(_._2)._1
  }
}
