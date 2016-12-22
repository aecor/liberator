package io.aecor.liberator.test

import cats.data.{ State, StateT }
import cats.free.{ Free, Inject }
import cats.implicits._
import cats.{ Applicative, Eval, Monad }
import io.aecor.liberator.macros.free
import io.aecor.liberator.{ FreeAlgebra, ProductKK }

import scala.io.StdIn

@free
trait KeyValueStore[F[_]] {
  def setValue(key: String, value: String): F[Unit]

  def getValue(key: String): F[Option[String]]
}

@free
trait Logging[F[_]] {
  def debug(value: String): F[Unit]

  def info(value: String): F[Unit]
}

@free
trait UserInteraction[F[_]] {
  def readLn(prompt: String): F[String]

  def writeLn(s: String): F[Unit]
}

object StateKeyValueStore extends KeyValueStore[State[Map[String, String], ?]] {
  override def setValue(key: String, value: String): State[Map[String, String], Unit] =
    StateT.modify[Eval, Map[String, String]](_.updated(key, value))

  override def getValue(key: String): State[Map[String, String], Option[String]] =
    StateT.inspect(_.get(key))
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

class ConsoleUserInteraction[F[_]: Applicative] extends UserInteraction[F] {
  override def readLn(prompt: String): F[String] = Applicative[F].pure(StdIn.readLine(prompt))

  override def writeLn(s: String): F[Unit] = Applicative[F].pure(println(s))
}

object ConsoleUserInteraction {
  def apply[F[_]: Applicative]: UserInteraction[F] = new ConsoleUserInteraction[F]
}

object App {

  def main(args: Array[String]): Unit = {

    def setAndGetPreviousValue[F[_]: Monad: KeyValueStore: Logging](
      key: String,
      value: String
    ): F[Option[String]] =
      for {
        previous <- KeyValueStore[F].getValue(key)
        _ <- Logging[F].info(s"Was $key = $previous")
        _ <- Logging[F].debug(s"Setting $key to $value")
        _ <- KeyValueStore[F].setValue(key, value)
      } yield previous

    def program[F[_]: Monad: KeyValueStore: Logging: UserInteraction]: F[Unit] =
      for {
        key <- UserInteraction[F].readLn("Enter key: ")
        value <- UserInteraction[F].readLn("Enter value: ")
        previous <- setAndGetPreviousValue[F](key, value)
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

    val freeAlgebra =
      FreeAlgebra[ProductKK[KeyValueStore, ProductKK[Logging, UserInteraction, ?[_]], ?[_]]]

    implicit class FAOps[F[_[_]], A[_]](fa: F[A]) {
      def and[G[_[_]]](ga: G[A]): ProductKK[F, G, A] = ProductKK(fa, ga)
    }

    val k = freeAlgebra(
      StateKeyValueStore and
        (ConsoleLogging[State[Map[String, String], ?]]
          and ConsoleUserInteraction[State[Map[String, String], ?]])
    )

    implicit val userInteractionInject: Inject[UserInteraction.UserInteractionFree,
                                               freeAlgebra.Out] =
      Inject.catsFreeRightInjectInstance(
        Inject.catsFreeRightInjectInstance(Inject.catsFreeReflexiveInjectInstance)
      )

    def freeProgram: Free[freeAlgebra.Out, Unit] = program[Free[freeAlgebra.Out, ?]]

    val out =
      freeProgram.foldMap(k)

    val result = out.run(Map.empty)

    println(result.value)

    ()

  }
}
