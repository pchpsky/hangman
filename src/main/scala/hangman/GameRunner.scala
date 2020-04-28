package hangman

import cats._
import cats.instances._
import cats.data._
import cats.syntax._
import fs2._
import cats.implicits._
import cats.effect._
import hangman.Game.{Result, Won}

case class GameRunner(dictionary: Dictionary) {
  def run[F[_]: Sync]: F[Result] = {
    def gameStream(words: List[String]): Stream[F, Result] = {
      Stream
        .emits(words)
        .covary[F]
        .map(Game.create)
        .map(Game.run[F])
        .map(_.flatMap(res => Sync[F].delay{ println(res.show) }.as(res)))
        .flatMap[F, Result](Stream.eval)
        .takeThrough(_ == Won)
    }

    for {
      shuffled <- dictionary.shuffle
      result <- gameStream(shuffled.words).compile.last
    } yield result.get
  }
}

object GameRunner {
  def create(dictionary: Dictionary): Either[AppError, GameRunner] =
    Either.cond(
      !dictionary.isEmpty,
      GameRunner(dictionary),
      EmptyDictionary
    )
}
