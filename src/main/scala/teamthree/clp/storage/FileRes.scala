package teamthree.clp.storage

import java.net.URL

import scala.io.Source
import scala.util.Try

case class FileRes[A](run: List[String] => A) {
  def map[B](f: A => B): FileRes[B] = FileRes(src => f(run(src)))

  def flatMap[B](f: A => FileRes[B]): FileRes[B] =
    FileRes(src => f(run(src)).run(src))

  def execute(url: URL): Try[A] = Try {
    println("File opening...")
    val bufferedSource = Source.fromURL(url)
    val result = run(bufferedSource.getLines.toList)
    bufferedSource.close()
    println("File close")
    result
  }
}

object FileRes {
  def getPlaces(food: String): FileRes[List[String]] = FileRes { lines =>
    lines.find(_.contains(food)) match {
      case Some(line) =>
        val separatorIndex = line.indexOf(':')
        line.substring(separatorIndex + 1).split('|').toList
      case None =>
        Nil
    }
  }

  def getAllFoods: FileRes[List[String]] = FileRes { lines =>
    for {
      line <- lines
      separatorIndex = line.indexOf(':')
    } yield line.substring(0, separatorIndex)
  }
}