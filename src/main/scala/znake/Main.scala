package znake

import java.io.IOException

import jline.console.ConsoleReader
import jline.internal.NonBlockingInputStream
import scalaz.zio._
import scalaz.zio.console._
import znake.KeyListeners.Keys
import znake.KeyListeners.Keys._

import scala.util.Random
import scala.concurrent.duration._

object KeyListeners {

  sealed trait Keys

  object Keys {
    case object Up extends Keys
    case object Down extends Keys
    case object Right extends Keys
    case object Left extends Keys
  }

  val consoleReaderIO: IO[Exception, ConsoleReader] =
    IO.syncException(new ConsoleReader)

  def inputStreamIO(reader: ConsoleReader): IO[Exception, NonBlockingInputStream] =
    IO.syncException(new NonBlockingInputStream(reader.getInput, true))

  val streamIO: IO[Exception, NonBlockingInputStream] =
  consoleReaderIO.flatMap(inputStreamIO)

  def updateKeys(stream: NonBlockingInputStream, onKey: Keys => IO[Nothing, Unit]): IO[Exception, Unit] =
    for {
      nbis <- IO.now(stream)
      r    <- IO.now(nbis.read(10))
      _    <- r.toChar match {
        case 'A' => onKey(Keys.Up)
        case 'B' => onKey(Keys.Down)
        case 'C' => onKey(Keys.Right)
        case 'D' => onKey(Keys.Left)
        case _   => IO.unit
      }
    } yield ()

  def listen(onKey: Keys => IO[Nothing, Unit]): IO[Exception, Unit] =
    streamIO.flatMap(r => updateKeys(r, onKey).repeat(Schedule.spaced(25 millis)).void)
}


object Main extends App {

  type Snake = List[Loc]

  private val boardSize: Int = 20
  private val startSnake: Snake = List(Loc(5, 5), Loc(4, 5), Loc(3, 5))
  private val startInput: Keys = Right

  case class Loc(x: Int, y: Int)

  case class State(name: String, snake: Snake, points: Int, justFed: Boolean, apple: Loc) {
    def moveSnake(dir: Keys): State = {
      val tail = if (justFed) snake else snake.init

      val updated = snake match {
        case x :: _ =>
          (dir match {
            case Left  => x.copy(x = x.x - 1)
            case Right => x.copy(x = x.x + 1)
            case Up    => x.copy(y = x.y - 1)
            case Down  => x.copy(y = x.y + 1)
          }) :: tail
        case x => x
      }

      State(name, updated, points, false, apple)
    }

    val hasEatenSelf: Boolean = snake match {
      case x :: xs => xs.contains(x)
      case _       => false
    }

    val isOutOfBounds: Boolean = snake.headOption.foldLeft(false) {
      (_, loc) =>
        loc.x < 0 || loc.x >= boardSize ||
        loc.y < 0 || loc.y >= boardSize
    }

    val isDead: Boolean = isOutOfBounds || hasEatenSelf

    val hasEatenApple: Boolean = snake.headOption.contains(apple)
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
    putStrLn("Press any key to start...") *> getStrLn *> IO.unit

  def appleLoc(snake: Snake): IO[Nothing, Loc] =
    for {
      loc <- randomLoc
      loc <- if (snake.contains(loc)) appleLoc(snake)
      else IO.now(loc)
    } yield loc

  def onKey(ref: Ref[Keys]): Keys => IO[Nothing, Unit] =
    nextKey =>
      for {
        curr <- ref.get
        _    <- (nextKey, curr) match {
          case (Keys.Up | Keys.Down, Keys.Up | Keys.Down) |
               (Keys.Left | Keys.Right, Keys.Left | Keys.Right) => IO.unit
          case _ => ref.set(nextKey)
        }
      } yield ()

  val znakeGame: IO[IOException, Unit] =
    for {
      _     <- putStrLn("Welcome to Znake!")
      name  <- getName
      _     <- welcomeMsg
      snake  = startSnake
      apple <- appleLoc(snake)
      state  = State(name, snake, 0, false, apple)
      _     <- renderGame(state)
      input <- Ref(startInput)
      _     <- KeyListeners.listen(onKey(input)).supervised.fork
      _     <- gameLoop(state, input)
    } yield ()

  def gameLoop(state: State, input: Ref[Keys]): IO[IOException, Unit] =
    for {
      state <- nextState(state, input)
      _     <- renderGame(state)
      _     <- IO.sleep(200.milliseconds)
      _     <-
        if (state.isDead) putStrLn("You are dead!") *> IO.unit
        else gameLoop(state, input)
    } yield ()

  def nextState(state: State, input: Ref[Keys]): IO[IOException, State] =
    for {
      dir   <- input.get
      state <- IO.now(state.moveSnake(dir))
      state <- if (state.hasEatenApple) eatApple(state)
               else IO.now(state)
    } yield state

  def eatApple(state: State): IO[IOException, State] =
    for {
      apple <- appleLoc(state.snake)
      points = state.points + 1
      state <- IO.now(state.copy(points = points, justFed = true, apple = apple))
    } yield state

  def renderGame(state: State): IO[IOException, Unit] = {
    // Kurt   10
    // |-------|
    // |  @    |
    // |    x  |
    // |   xx  |
    // |-------|

    val screenSize = boardSize + 2
    val spaces = (screenSize - (state.name.length + state.points.toString.length)).max(2)
    val points = state.name + (" " * spaces) + state.points
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
      _ <- putStrLn(points)
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
