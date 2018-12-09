package teamthree.clp

import teamthree.clp.bot.WebhookCLPBot

object Main extends App {
  val bot = new WebhookCLPBot()
  val eol = bot.run()
}