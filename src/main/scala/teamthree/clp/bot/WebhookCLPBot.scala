package teamthree.clp.bot

import com.bot4s.telegram.api._
import com.bot4s.telegram.clients.AkkaHttpClient
import slogging.{LogLevel, LoggerConfig, PrintLoggerFactory}


class WebhookCLPBot extends CLPBot
  with AkkaTelegramBot
  with Webhook {

  LoggerConfig.factory = PrintLoggerFactory()
  LoggerConfig.level = LogLevel.TRACE

  override val webhookUrl: String = scala.util.Properties.envOrNone("WEBHOOK_URL").get
  override val port: Int = scala.util.Properties.envOrNone("PORT").map(_.toInt).get
  override val client: RequestHandler = new AkkaHttpClient(token)
}