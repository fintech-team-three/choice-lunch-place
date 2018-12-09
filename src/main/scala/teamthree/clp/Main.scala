package teamthree.clp

import teamthree.clp.bot.{PollingCLPBot, WebhookCLPBot}

object Main extends App {
  val bot = new PollingCLPBot()
  val eol = bot.run()
}