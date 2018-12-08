package teamthree.clp.bot

trait Storage[K, V] {
  def put(id: K, poll: V)

  def find(id: K): Option[V]

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
}

case class InMemoryUserBotStorage() {
  private var users = Set[BotUser]()

  def put(user: BotUser): Unit = {
    users = users + user
  }

  def find(action: BotUser => Boolean): Option[BotUser] = {
    users.find(action)
  }

  def map(id: Long)(mapper: BotUser => BotUser): Unit = {
    users = users.map { u =>
      if (u.id == id)
        mapper(u)
      else
        u
    }
  }
}
