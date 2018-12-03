package teamthree.clp.bot

import scala.collection.mutable

trait Storage[K, V] {
  def put(id: K, poll: V)

  def find(id: K): Option[V]

  def contains(id: K): Boolean

  def remove(id: K)

  def findAndUpdate(id: K)(mapper: V => V): Option[V] = {
    find(id) match {
      case Some(poll) =>
        val newVal = mapper(poll)

        put(id, newVal)

        Some(newVal)
      case None => None
    }
  }
}

case class InMemoryStorage[K, V]() extends Storage[K, V] {

  private var storage: Map[K, V] = Map.empty

  override def put(id: K, poll: V): Unit = {
    storage = storage + (id -> poll)
  }


  override def find(id: K): Option[V] = storage.get(id)

  override def remove(id: K): Unit = {
    storage = storage - id
  }

  override def contains(id: K): Boolean = storage.contains(id)
}

case class NInMemoryStorage() {
  private val users = mutable.MutableList[NUser]()

  def put(user: NUser): Unit = {
    users += user
  }

  def find(id: Long): Option[NUser] = {
    users.find { u => u.id == id }
  }

  def find(username: String): Option[NUser] = {
    users.find { u => u.username == username }
  }

  def map(id: Long)(mapper: NUser => NUser): Unit = {
    users.find { u => u.id == id }.map(mapper)
  }
}
