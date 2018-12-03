package teamthree.clp.YaPlaceApi

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import io.circe.Json
import scala.concurrent.duration._
import io.circe.parser._
import model.Language.Language
import model.TypeRes.TypeRes
import model.ApiModels._
import model._

import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}

case class ApiPlaceUrl(path: String,
                       apiKey: String,
                       text: String,
                       lang: Language,
                       typeRes: Option[TypeRes] = None,
                       centerSearch: Option[(Double, Double)] = None,
                       sizeAreaSearch: Option[(Double, Double)] = None,
                       searchOutside: Option[Boolean] = None,
                       numResults: Int = 10) {

  def getUri: Uri = {
    val query = Query.newBuilder
    query += ("apikey" -> apiKey)
    query += ("text" -> text)
    query += ("lang" -> lang.toString)
    query += ("results" -> numResults.toString)
    typeRes match {
      case Some(value) => query += ("type" -> value.toString)
      case None =>
    }
    centerSearch match {
      case Some(value) => query += ("ll" -> s"${value._1},${value._2}")
      case None =>
    }
    sizeAreaSearch match {
      case Some(value) => query += ("spn" -> s"${value._1},${value._2}")
      case None =>
    }
    searchOutside match {
      case Some(_) => query += ("rspn" -> "1")
      case None =>
    }
    Uri(path).withQuery(query.result())
  }
}

sealed trait Result[+T]
final case class Success[T](result: T) extends Result[T]
final case class Error[T](message: String) extends Result[T]

//https://tech.yandex.ru/maps/doc/geosearch/concepts/request-docpage/
class PlacesApi(apiKey: String) {
  private val searchPath = "https://search-maps.yandex.ru/v1/"

  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  private def getCafesByUrl(url: ApiPlaceUrl): Future[Result[List[Cafe]]] = {
    makeRequest(url).flatMap {
        case response@HttpResponse(StatusCodes.OK, headers, entity, _) =>
          entity.dataBytes.runFold(ByteString(""))(_ ++ _).flatMap { body =>
            Future {
              val jsn = parse(body.utf8String).getOrElse(Json.Null)
              println("Got response, body: " + body.utf8String)
              jsn.as[List[Cafe]] match {
                case Left(err) => Error(err.toString())
                case Right(value) => Success(value)
              }
            }
          }
        case response@HttpResponse(code, _, _, _) =>
          println("Request failed, response code: " + code)
          Future(Error("Request failed, response code: " + code))
      }
  }
  private def makeRequest(url: ApiPlaceUrl): Future[HttpResponse] =
    Http().singleRequest(HttpRequest(uri = url.getUri))

  def searchCafeInArea(cuisine: String, coordinates: (Double, Double), sizeArea: (Double, Double),
                       lang: Language = Language.ru_RU, numEntries: Int = 10): Future[Result[List[Cafe]]] =
    getCafesByUrl(ApiPlaceUrl(searchPath, apiKey, cuisine, lang,
      Some(TypeRes.biz), Some(coordinates), Some(sizeArea), numResults = numEntries))

  def searchCafeInCity(cuisine: String, city: String, lang: Language = Language.ru_RU,
                       numEntries: Int = 10): Future[Result[List[Cafe]]] =
    getCafesByUrl(ApiPlaceUrl(searchPath, apiKey, s"$cuisine $city", lang,
      Some(TypeRes.biz), numResults = numEntries))

  def searchCafeByCoords(cuisine: String, coordinates: (Double, Double),
                         lang: Language = Language.ru_RU, numEntries: Int = 10): Future[Result[List[Cafe]]] =
    getCafesByUrl(ApiPlaceUrl(searchPath, apiKey, cuisine, lang, Some(TypeRes.biz),
      Some(coordinates), numResults = numEntries))
}

object PlaceApiExample extends App {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global
  val apiKey = scala.util.Properties.envOrNone("YANDEX_TOKEN")
  val placesApi = apiKey match {
    case Some(key) =>
      new PlacesApi(key)
    case None =>
      println("Fatal error: could not get token for Yandex Places API.")
      sys.exit()
  }

  val res1 = placesApi.searchCafeByCoords("кавказская кухня", (37.0, 55.43644829))
  val res2 = placesApi.searchCafeInCity("французская кухня", "Екатеринбург")
  val res3 = placesApi.searchCafeInArea("шашлык", (40, 60), (2, 3))
  println(Await.result(res1, 10 seconds))
  println(Await.result(res2, 10 seconds))
  println(Await.result(res3, 10 seconds))
}
