package io.aecor.liberator.tests

import cats.data._
import cats.free.Free
import cats.implicits._
import cats.{ Applicative, Eval, InjectK, Monad, ~> }
import io.aecor.liberator.Term
import io.aecor.liberator.data.ProductKK
import io.aecor.liberator.macros._
import io.aecor.liberator.syntax._

import scala.io.StdIn

@term
@free
@algebra
@functorK
@reifyInvocations
trait KeyValueStore[K, V, F[_]] {
  def setValue(key: K, value: V): F[Unit]

  def getValue(key: K): F[Option[V]]
}
object KeyValueStore

@term
@free
@algebra
@functorK
trait Logging[F[_]] {
  def debug(value: String): F[Unit]

  def info(value: String): F[Unit]
}
object Logging

@term
@free
@algebra
@functorK
trait UserInteraction[F[_]] {
  def readLn(prompt: String): F[String]
  def writeLn(s: String): F[Unit]
}
object UserInteraction

@term
@free
@algebra
@functorK
trait FileIO[F[_]] {
  def appendLine(filePath: String, line: String): F[Unit]
}
object FileIO

object StateFileIO extends FileIO[State[Map[String, Vector[String]], ?]] {
  override def appendLine(filePath: String,
                          line: String): State[Map[String, Vector[String]], Unit] =
    State.modify { state =>
      state.updated(filePath, state.getOrElse(filePath, Vector.empty) :+ line)
    }
}

class StateKeyValueStore[K, V] extends KeyValueStore[K, V, State[Map[K, V], ?]] {
  override def setValue(key: K, value: V): State[Map[K, V], Unit] =
    StateT.modify[Eval, Map[K, V]](_.updated(key, value))

  override def getValue(key: K): State[Map[K, V], Option[V]] =
    StateT.inspect(_.get(key))
}

object StateKeyValueStore {
  def apply[K, V]: StateKeyValueStore[K, V] = new StateKeyValueStore[K, V]
}

class ConsoleLogging[F[_]: Applicative] extends Logging[F] {
  override def debug(value: String): F[Unit] =
    Applicative[F].pure(print(s"DEBUG: $value\n"))

  override def info(value: String): F[Unit] =
    Applicative[F].pure(print(s"INFO: $value\n"))
}

object ConsoleLogging {
  def apply[F[_]: Applicative]: Logging[F] = new ConsoleLogging[F]
}

class FileLogging[F[_]: FileIO](fileName: String) extends Logging[F] {
  override def debug(value: String): F[Unit] = FileIO[F].appendLine(fileName, s"DEBUG: $value\n")

  override def info(value: String): F[Unit] = FileIO[F].appendLine(fileName, s"INFO: $value\n")
}

object FileLogging {
  def apply[F[_]: FileIO](fileName: String): Logging[F] = new FileLogging(fileName)
  def term(fileName: String): Logging[Term[FileIO, ?]] = new FileLogging[Term[FileIO, ?]](fileName)
}

class ConsoleUserInteraction[F[_]: Applicative] extends UserInteraction[F] {
  override def readLn(prompt: String): F[String] = Applicative[F].pure(StdIn.readLine(prompt))

  override def writeLn(s: String): F[Unit] = Applicative[F].pure(println(s))
}

object ConsoleUserInteraction {
  def apply[F[_]: Applicative]: UserInteraction[F] = new ConsoleUserInteraction[F]
}

object TestApp {

  def main(args: Array[String]): Unit = {
    def setAndGetPreviousValue[K, V, F[_]: Monad: KeyValueStore[K, V, ?[_]]: Logging](
      key: K,
      value: V
    ): F[Option[V]] =
      for {
        previous <- KeyValueStore[K, V, F].getValue(key)
        _ <- Logging[F].info(s"Was $key = $previous")
        _ <- Logging[F].debug(s"Setting $key to $value")
        _ <- KeyValueStore[K, V, F].setValue(key, value)
      } yield previous

    def program[F[_]: Monad: Logging: UserInteraction: KeyValueStore[String, String, ?[_]]]
      : F[Unit] =
      for {
        key <- UserInteraction[F].readLn("Enter key: ")
        value <- UserInteraction[F].readLn("Enter value: ")
        previous <- setAndGetPreviousValue[String, String, F](key, value)
        message = previous
          .map(s => s"Previous value was $s")
          .getOrElse("Previous value was not set")
        _ <- UserInteraction[F].writeLn(message)
        exit <- UserInteraction[F].readLn("Exit? (y/n): ").map(_ == "y")
        _ <- if (exit) {
              ().pure[F]
            } else {
              program[F]
            }
      } yield ()

    case class AppState(keyValueStoreState: Map[String, String],
                        fileIOState: Map[String, Vector[String]])

    type AppF[A] = State[AppState, A]

    def fileIO: FileIO[AppF] = StateFileIO.mapK(
      λ[State[Map[String, Vector[String]], ?] ~> AppF](
        _.transformS(_.fileIOState, (s, x) => s.copy(fileIOState = x))
      )
    )

    def keyValueStore: KeyValueStore[String, String, AppF] =
      StateKeyValueStore[String, String].mapK(
        λ[State[Map[String, String], ?] ~> AppF](
          _.transformS(_.keyValueStoreState, (s, x) => s.copy(keyValueStoreState = x))
        )
      )

    def termProgram =
      program[Term[ProductKK[KeyValueStore[String, String, ?[_]],
                             ProductKK[Logging, UserInteraction, ?[_]],
                             ?[_]], ?]]

    type OpsCoproduct[A] =
      EitherK[KeyValueStore.KeyValueStoreOp[String, String, ?],
              EitherK[Logging.LoggingOp, UserInteraction.UserInteractionOp, ?],
              A]

    implicit val userInteractionInjectK: InjectK[UserInteraction.UserInteractionOp, OpsCoproduct] =
      InjectK.catsRightInjectKInstance(InjectK.catsRightInjectKInstance)

    def freeProgram =
      program[Free[OpsCoproduct, ?]]

    def interpreters =
      keyValueStore :&:
        FileLogging[Term[FileIO, ?]]("log.dat")
        .transpile(fileIO) :&:
        ConsoleUserInteraction[AppF]

    def out =
      Eval.later(termProgram(interpreters)).flatMap { _ =>
        Eval.later(freeProgram.foldMap(interpreters.asFunctionK))
      }

    def result = out.value.run(AppState(Map.empty, Map.empty))

    println(result.value)
    ()

  }
}
