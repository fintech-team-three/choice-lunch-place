package teamthree.clp

import teamthree.clp.bot.{CLPBot, PollingCLPBot}

object Main extends App {
  val bot = new PollingCLPBot()
  val eol = bot.run()
}