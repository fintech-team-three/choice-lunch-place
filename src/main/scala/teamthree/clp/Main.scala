package teamthree.clp

import teamthree.clp.bot.CLPBot

import scala.io.Source

object Main extends App {
  val token = scala.util.Properties
    .envOrNone("BOT_TOKEN")
    .getOrElse(Source.fromFile("bot.token").getLines().mkString)

  val bot = new CLPBot(token)
  val eol = bot.run()
}