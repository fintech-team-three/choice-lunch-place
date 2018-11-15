package teamthree.clp.storage

import java.net.URL

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Success

class FileResSpec extends FlatSpec with Matchers {
  import FileRes._

  val filePath: URL = getClass.getResource("/places.txt")

  "getAllFoods" should "return list of all foods in the file on given path" in {
    val result = getAllFoods.execute(filePath)
    result should be(Success(List("Russian", "Italian")))
  }

  "getPlaces" should "return list of places for given food" in {
    val result = getPlaces("Russian").execute(filePath)
    result should be(Success(List("Кафе1", "Ресторан1")))
  }

  "map and flatMap" should "work correctly in for-comprehension" in {
    val testProgram: FileRes[List[String]] =
      for {
        allFoods <- getAllFoods
        places <- getPlaces(allFoods.last)
      } yield places

    val result = testProgram.execute(filePath)
    result should be(Success(List("Ресторан 2", "Кафе 2", "Ресторан-кафе")))
  }
}
