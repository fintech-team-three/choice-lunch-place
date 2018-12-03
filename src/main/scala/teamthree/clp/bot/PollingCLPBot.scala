package teamthree.clp.bot

import com.bot4s.telegram.api.{Polling, RequestHandler, TelegramBot}
import com.bot4s.telegram.clients.SttpClient
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp.okhttp.OkHttpFutureBackend
import slogging.{LogLevel, LoggerConfig, PrintLoggerFactory}

import scala.concurrent.Future

class PollingCLPBot extends CLPBot
  with TelegramBot
  with Polling {

  LoggerConfig.factory = PrintLoggerFactory()
  LoggerConfig.level = LogLevel.TRACE

  implicit val backend: SttpBackend[Future, Nothing] = OkHttpFutureBackend()
  override val client: RequestHandler = new SttpClient(token)
}

