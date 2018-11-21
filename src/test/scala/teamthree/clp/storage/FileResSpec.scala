package teamthree.clp.storage

import java.net.URL

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.TryValues._
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

  val testProgram: FileRes[List[String]] =
    for {
      allFoods <- getAllFoods
      places <- getPlaces(allFoods.last)
    } yield places

  "map and flatMap" should "work correctly in for-comprehension" in {
    val result = testProgram.execute(filePath)
    result should be(Success(List("Ресторан 2", "Кафе 2", "Ресторан-кафе")))
  }

  "Any FileRes" should "return Failure with NullPointerException if received URL invalid" in {
    val result = testProgram.execute(getClass.getResource("/non-existing-file.txt"))
    result.failure.exception shouldBe a [java.lang.NullPointerException]
  }
}