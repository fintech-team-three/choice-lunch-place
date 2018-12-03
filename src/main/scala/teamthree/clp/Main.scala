package teamthree.clp

import teamthree.clp.bot.{PollingCLPBot, WebhookCLPBot}

object Main extends App {
  val bot = new WebhookCLPBot()
  val eol = bot.run()
}