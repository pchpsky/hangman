package hangman

import java.io._

import cats._
import cats.instances._
import cats.data._
import cats.syntax._
import cats.implicits._
import cats.effect._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val gameRunner: EitherT[IO, AppError, GameRunner] = for {
      dictionary <- EitherT(Dictionary.fromResource[IO]("dictionary.txt"))
      runner <- EitherT(IO(GameRunner.create(dictionary)))
    } yield runner

    gameRunner.value.flatMap {
      case Left(error) => IO(println(error.message)).as(ExitCode.Error)
      case Right(gameRunner) => gameRunner.run[IO].as(ExitCode.Success)
    }
  }
}
