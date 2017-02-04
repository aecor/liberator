package io.aecor.liberator.tests

import cats.data.{ State, StateT }
import cats.free.{ Free, Inject }
import cats.implicits._
import cats.{ Applicative, Eval, Monad }
import io.aecor.liberator.macros.free
import io.aecor.liberator.{ FreeAlgebra, ProductKK, Term }

import scala.io.StdIn

@free
trait KeyValueStore[K, V, F[_]] {
  def setValue(key: K, value: V): F[Unit]
  def getValue(key: K): F[Option[V]]
}

object KeyValueStore {
  object terms {
    type KeyValueStoreApplied[K, V] = {
      type T[F[_]] = KeyValueStore[K, V, F]
    }

    def setValue[K, V](key: K, value: V): Term[KeyValueStoreApplied[K, V]#T, Unit] =
      new Term[KeyValueStoreApplied[K, V]#T, Unit] {
        override def apply[F[_]](alg: KeyValueStore[K, V, F])(implicit F: Monad[F]): F[Unit] =
          alg.setValue(key, value)
      }
  }
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

    def program[F[_]: Monad: KeyValueStore[String, String, ?[_]]: Logging: UserInteraction]
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

    val freeAlgebra =
      FreeAlgebra[ProductKK[KeyValueStore[String, String, ?[_]],
                            ProductKK[Logging, UserInteraction, ?[_]],
                            ?[_]]]

    val k = freeAlgebra.toFunctionK(
      ProductKK(
        StateKeyValueStore[String, String],
        ProductKK(
          ConsoleLogging[State[Map[String, String], ?]],
          ConsoleUserInteraction[State[Map[String, String], ?]]
        )
      )
    )

    implicit val userInteractionInject: Inject[UserInteraction.UserInteractionOp, freeAlgebra.Out] =
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
