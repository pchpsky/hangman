package hangman

import cats._
import cats.instances._
import cats.data._
import cats.syntax._
import cats.implicits._
import cats.effect._

import scala.io.Source
import scala.util.Random

case class Dictionary(words: List[String]) {
  def isEmpty: Boolean = words.isEmpty

  def length: Int = words.length

  def apply(n: Int): String = words(n)

  def shuffle[F[_]: Sync]: F[Dictionary] =
    Sync[F].delay { Dictionary(Random.shuffle(words)) }
}

object Dictionary {
  def fromResource[F[_]: Sync](resourceName: String): F[Either[AppError, Dictionary]] = {
    readLinesFromResource(resourceName)
      .map(Dictionary(_).asRight[AppError])
      .handleErrorWith(_ => Sync[F].pure(ResourceNotFound(resourceName).asLeft))
  }

  private def readLinesFromResource[F[_]](resourceName: String)(implicit F: Sync[F]): F[List[String]] = {
    Resource.fromAutoCloseable(F.delay { Source.fromResource(resourceName) })
      .use(bufferedSource => F.delay { bufferedSource.getLines().toList })
  }
}
