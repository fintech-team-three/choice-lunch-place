package teamthree.clp.storage


import teamthree.clp.YaPlaceApi._
import teamthree.clp.YaPlaceApi.model.ApiModels.Cafe

import scala.concurrent.duration._
import scala.concurrent._
import scala.util.Try

case class WebRes[A](run: PlacesApi => A) {
  def map[B](f: A => B): WebRes[B] =
    WebRes(src => f(run(src)))

  def flatMap[B](f: A => WebRes[B]): WebRes[B] =
    WebRes(src => f(run(src)).run(src))

  def execute(key:String): Try[A] = Try {
    val p = new PlacesApi(key)
    println("File was opened")
    val result = run(p)
    println("File was closed")
    result
  }
}



object WebRes {
  def getCafeInCity(food: String, city:String): WebRes[List[Cafe]] = WebRes { p =>
    implicit val ec: ExecutionContextExecutor = ExecutionContext.global
    val res = p.searchCafeInCity(food, city)
    val data = Await.ready(res, 3 seconds)
    data match {
      case d:List[Cafe] => d
    }
  }

  def getCafeByCoords(food: String, coords:(Double, Double)): WebRes[List[Cafe]] = WebRes { p =>
    implicit val ec: ExecutionContextExecutor = ExecutionContext.global
    val res = p.searchCafeByCoords(food, coords)
    val data = Await.ready(res, 3 seconds)
    data match {
      case d:List[Cafe] => d
    }
  }

  def getCafeInArea(food: String, coords:(Double, Double), sizeArea: (Double, Double)): WebRes[List[Cafe]] = WebRes { p =>
    implicit val ec: ExecutionContextExecutor = ExecutionContext.global
    val res = p.searchCafeInArea(food, coords, sizeArea)
    val data = Await.ready(res, 3 seconds)
    data match {
      case d:List[Cafe] => d
    }
  }

}

object WebResExample extends App {

  var program = for {
    res  <- WebRes.getCafeInCity("кавказская кухня", "Екатеринбург")
  } yield res

  var out = program.execute("a952f905-e84c-4549-a35c-5f273adbc857")
  println(out)
  println("done!")
}

