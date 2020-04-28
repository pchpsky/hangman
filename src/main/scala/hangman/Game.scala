package hangman

import cats._
import cats.instances._
import cats.data._
import cats.syntax._
import fs2._
import cats.implicits._
import cats.effect._
import hangman.Game.{Lost, Result, Won}

import scala.io.StdIn

case class Game(word: List[Char], result: List[Option[Char]], guesses: List[Char] = List()) {
  def show: String = {
    val showResult = result.map(_.getOrElse('_')).mkString
    s"$showResult\nAttempts left: ${7 - guesses.size}\n"
  }

  def guess(char: Char): (Game, Option[Result]) = {
    if (!char.isLetter || result.exists(_.contains(char)) || guesses.contains(char)) {
      (this, None)
    } else if (!word.contains(char)) {
      (Game(word, result, char :: guesses), Option.when(guesses.size == 6)(Lost))
    } else {
      val res = word.zip(result).fmap { case (c, optionC) => optionC.orElse(Option.when(c == char)(c)) }
      (Game(word, res, guesses), Option.when(res.sequence.isDefined)(Won))
    }
  }

  def attempt[F[_]: Sync]: F[(Game, Option[Result])] = {
    for {
      _ <- Sync[F].delay { println(show) }
      c <- Sync[F].delay { StdIn.readChar() }
    } yield guess(c)
  }
}

object Game {
  sealed trait Result {
    def show: String
  }
  final object Won extends Result {
    def show = "You won!"
  }
  final object Lost extends Result {
    def show = "You lost!"
  }

  def create(word: String): Game = {
    Game(word.toList, List.fill(word.length)(Option.empty[Char]))
  }

  def run[F[_]: Sync](game: Game): F[Result] = {
    Stream
      .iterateEval((game, Option.empty[Result]))(_._1.attempt)
      .takeThrough(_._2.isEmpty).compile.last.map(_.get._2.get)
  }
}
