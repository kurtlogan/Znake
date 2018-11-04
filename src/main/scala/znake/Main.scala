package znake

import java.io.IOException

import scalaz.zio._
import scalaz.zio.console._

import scala.util.Random
import scala.concurrent.duration._

object Main extends App {

  private val boardSize: Int = 10

  case class Loc(x: Int, y: Int)
  case class State(name: String, snake: List[Loc], points: Int, apple: Loc) {
    def moveSnake: State =
      State(name, snake.map(loc => loc.copy(x = loc.x + 1)), points, apple)

    val hasEatenSelf: Boolean = snake match {
      case x :: xs => xs.contains(x)
      case _       => false
    }

    val isOutOfBounds: Boolean = snake.headOption.foldLeft(false) {
      (acc, loc) =>
        loc.x < 0 || loc.x >= boardSize ||
        loc.y < 0 || loc.y >= boardSize
    }

    val isDead: Boolean = isOutOfBounds || hasEatenSelf
  }

  def nextInt(max: Int): IO[Nothing, Int] =
    IO.sync(Random.nextInt(max))

  val randomLoc: IO[Nothing, Loc] =
    for {
      x <- nextInt(boardSize)
      y <- nextInt(boardSize)
    } yield Loc(x, y)

  val getName: IO[IOException, String] =
    putStrLn("Please tell us your name") *> getStrLn

  val welcomeMsg: IO[IOException, Unit] =
    putStrLn("Press any key to start...") *> getStrLn *> IO.now(())

  def appleLoc(snake: List[Loc]): IO[Nothing, Loc] =
    for {
      loc <- randomLoc
      loc <- if (snake.contains(loc)) appleLoc(snake)
             else IO.now(loc)
    } yield loc

  val znakeGame: IO[IOException, Unit] =
    for {
      _     <- putStrLn("Welcome to Znake!")
      name  <- getName
      _     <- welcomeMsg
      snake = List(Loc(5, 5), Loc(4, 5), Loc(3, 5))
      apple <- appleLoc(snake)
      state = State(name, snake, 0, apple)
      _     <- renderGame(state)
      _     <- gameLoop(state)
    } yield ()

  def gameLoop(state: State): IO[IOException, Unit] =
    for {
      state <- IO.now(state.moveSnake)
      _     <- renderGame(state)
      _     <- IO.sleep(200.milliseconds)
      loop  <-
        if (state.isDead) putStrLn("You are dead!") *> IO.now(false)
        else IO.now(true)
      _ <-
        if(loop) gameLoop(state)
        else IO.now(())
    } yield ()

  def renderGame(state: State): IO[IOException, Unit] = {
    // |-------|
    // |  @    |
    // |    x  |
    // |   xx  |
    // |-------|

    val border = s"|${(1 to boardSize).map(_ => "-").mkString}|"
    val board = for {
      y   <- 0 until boardSize
    } yield {
      for {
        x   <- 0 until boardSize
        loc = Loc(x, y)
      } yield {
        if (state.snake.contains(loc)) "x"
        else if (state.apple == loc) "@"
        else " "
      }
    }

    for {
      _ <- putStrLn("\033\143")
      _ <- putStrLn(border)
      _ <- board.foldLeft[IO[IOException, Unit]](
                 IO.now(()))(
                 (acc, x) => acc *> putStrLn(s"|${x.mkString}|"))
      _ <- putStrLn(border)
    } yield ()
  }

  override def run(args: List[String]): IO[Nothing, Main.ExitStatus] =
    znakeGame.redeemPure(
      _ => ExitStatus.ExitNow(1),
      _ => ExitStatus.ExitNow(0)
    )
}
