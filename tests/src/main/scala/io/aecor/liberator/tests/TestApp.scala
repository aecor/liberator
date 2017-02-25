package io.aecor.liberator.tests

import cats.data._
import cats.free.{ Free, Inject }
import cats.implicits._
import cats.{ Applicative, Eval, Monad, ~> }
import io.aecor.liberator.Term
import io.aecor.liberator.data.ProductKK
import io.aecor.liberator.macros.{ algebra, free }
import io.aecor.liberator.syntax._
import io.aecor.liberator.tests.Eff.{ FxSelector, Ops }
import io.aecor.liberator.tests.Fx.Unified
import shapeless.ops.hlist.Selector
import shapeless.{ ::, HList, HNil }

import scala.io.StdIn

@free
trait KeyValueStore[K, V, F[_]] {
  def setValue(key: K, value: V): F[Unit]

  def getValue(key: K): F[Option[V]]
}

@algebra("foo", "bar")
trait Food[F[_]] {
  def create(foo: String, bar: String): F[Unit]
  def get(foo: String, bar: String): F[Option[String]]
  def foo: Int = 1
}

@free
trait Logging[F[_]] {
  def debug(value: String): F[Unit]

  def info(value: String): F[Unit]
}

trait UserInteraction[F[_]] {
  def readLn(prompt: String): F[String]

  def writeLn(s: String): F[Unit]
}

object UserInteraction {
  def apply[F[_]](implicit instance: UserInteraction[F]): UserInteraction[F] = instance

  sealed abstract class UserInteractionOp[A] extends Product with Serializable

  object UserInteractionOp {

    final case class ReadLn(prompt: String) extends UserInteractionOp[String]

    final case class WriteLn(s: String) extends UserInteractionOp[Unit]

  }

  type AppliedUserInteractionOp43099 = { type Out[A] = UserInteractionOp[A] }

  def fromFunctionK[F[_]](
    f: _root_.cats.arrow.FunctionK[UserInteractionOp, F]
  ): UserInteraction[F] = new UserInteraction[F] {
    def readLn(prompt: String): F[String] = f(UserInteractionOp.ReadLn(prompt))

    def writeLn(s: String): F[Unit] = f(UserInteractionOp.WriteLn(s))
  }

  def toFunctionK[F[_]](
    ops: UserInteraction[F]
  ): _root_.cats.arrow.FunctionK[UserInteractionOp, F] =
    new _root_.cats.arrow.FunctionK[UserInteractionOp, F] {
      def apply[A](op: UserInteractionOp[A]): F[A] = op match {
        case UserInteractionOp.ReadLn(prompt) => ops.readLn(prompt)
        case UserInteractionOp.WriteLn(s) => ops.writeLn(s)
      }
    }

  type FreeHelper43097[F[_]] = { type Out[A] = _root_.cats.free.Free[F, A] }

  implicit def freeInstance[F[_]](
    implicit inject: _root_.cats.free.Inject[UserInteractionOp, F]
  ): UserInteraction[FreeHelper43097[F]#Out] =
    fromFunctionK(new _root_.cats.arrow.FunctionK[UserInteractionOp, FreeHelper43097[F]#Out] {
      def apply[A](op: UserInteractionOp[A]): _root_.cats.free.Free[F, A] =
        _root_.cats.free.Free.inject(op)
    })

  type TermHelper43098[M[_[_]]] = { type Out[A] = _root_.io.aecor.liberator.Term[M, A] }

  implicit def termInstance[M[_[_]]](
    implicit extract: _root_.io.aecor.liberator.Extract[M, UserInteraction]
  ): UserInteraction[TermHelper43098[M]#Out] =
    fromFunctionK(new _root_.cats.arrow.FunctionK[UserInteractionOp, TermHelper43098[M]#Out] {
      def apply[A](op: UserInteractionOp[A]): _root_.io.aecor.liberator.Term[M, A] =
        new Term[M, A] {
          override def apply[F[_]](alg: M[F])(implicit F: Monad[F]): F[A] =
            toFunctionK(extract(alg))(op)
        }
    })

  type AppliedUserInteraction43100 = { type Out[F[_]] = UserInteraction[F] }

  implicit def liberatorAlgebra: io.aecor.liberator.Algebra.Aux[UserInteraction, UserInteractionOp] =
    new io.aecor.liberator.Algebra[UserInteraction] {
      type Out[A] = UserInteractionOp[A]

      override def toFunctionK[F[_]](
        of: UserInteraction[F]
      ): _root_.cats.arrow.FunctionK[UserInteractionOp, F] = UserInteraction.toFunctionK(of)

      override def fromFunctionK[F[_]](
        f: _root_.cats.arrow.FunctionK[UserInteractionOp, F]
      ): UserInteraction[F] = UserInteraction.fromFunctionK(f)
    }
}

@free
trait FileIO[F[_]] {
  def appendLine(filePath: String, line: String): F[Unit]
}

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

sealed abstract class Fx

sealed trait :*:[H[_[_]], +T <: Fx] extends Fx

sealed trait Just[F[_[_]]] extends Fx

object Fx {
  trait Unified[Fxs <: Fx] {
    type P[A[_]]
  }
  object Unified extends LowerPriorityUnifiedInstances {
    def apply[Fxs <: Fx](implicit u: Unified[Fxs]): Aux[Fxs, u.P] = u
    type Aux[Fxs <: Fx, P0[_[_]]] = Unified[Fxs] {
      type P[A[_]] = P0[A]
    }
    implicit def single[F[_[_]]]: Unified.Aux[Just[F], F] = new Unified[Just[F]] {
      type P[A[_]] = F[A]
    }
  }
  trait LowerPriorityUnifiedInstances {
    implicit def productKK[F[_[_]], T <: Fx, TP[_[_]]](
      implicit ev: Unified.Aux[T, TP]
    ): Unified.Aux[F :*: T, ProductKK[F, TP, ?[_]]] = new Unified[F :*: T] {
      type P[A[_]] = ProductKK[F, TP, A]
    }
  }
}

sealed trait Eff[Fxs <: Fx, F[_]] {
  type Out <: HList
  def instances: Out

  def apply[M[_[_]]](implicit selector: Selector[Out, M[F]]): M[F] = selector(instances)
}

sealed trait EffC[Fxs <: Fx, F[_]] {
  def apply[M[_[_]]](implicit selector: FxSelector[Fxs, M]): M[F] = selector.apply(this)
}
case class JustEff[M[_[_]], F[_]](mf: M[F]) extends EffC[Just[M], F]
case class ConsEff[M[_[_]], F[_], T <: Fx](h: M[F], t: EffC[T, F]) extends EffC[M :*: T, F]

object Eff {

  trait FxSelector[L <: Fx, M[_[_]]] {
    def apply[F[_]](effC: EffC[L, F]): M[F]
  }

  object FxSelector {

    implicit def select[M[_[_]]]: FxSelector[Just[M], M] =
      new FxSelector[Just[M], M] {
        override def apply[F[_]](effC: EffC[Just[M], F]): M[F] = effC match {
          case JustEff(mf) =>
            mf
          case other =>
            ???
        }
      }

    implicit def recurseT[M[_[_]], T <: Fx, N[_[_]]](
      implicit st: FxSelector[T, N]
    ): FxSelector[M :*: T, N] =
      new FxSelector[M :*: T, N] {
        override def apply[F[_]](effC: EffC[:*:[M, T], F]): N[F] = effC match {
          case ConsEff(h, t) =>
            st(t)
          case other => ???
        }
      }
    implicit def recurseH[M[_[_]], T <: Fx]: FxSelector[M :*: T, M] =
      new FxSelector[M :*: T, M] {
        override def apply[F[_]](effC: EffC[:*:[M, T], F]): M[F] = effC match {
          case ConsEff(h, t) =>
            h
          case other => ???
        }
      }
  }

  def apply[F <: Fx, A[_]](implicit instance: Eff[F, A]): Aux[F, A, instance.Out] = instance
  trait MkTerm[Fxs <: Fx] {
    def apply[U[_[_]]]()(implicit U: Unified.Aux[Fxs, U],
                         instance: Eff[Fxs, Term[U, ?]]): Aux[Fxs, Term[U, ?], instance.Out] =
      instance
  }
  def mkTerm[Fxs <: Fx] = new MkTerm[Fxs] {}

  trait Ops[F[_], H <: HList] {
    def instances: H
    def apply[M[_[_]]](implicit selector: Selector[H, M[F]]): M[F] = selector(instances)
  }

  def term[Fxs <: Fx, U[_[_]], Out](
    implicit U: Unified.Aux[Fxs, U],
    instance: Eff[Fxs, Term[U, ?]]
  ): Ops[Term[U, ?], instance.Out] =
    new Ops[Term[U, ?], instance.Out] {
      override def instances: instance.Out = instance.instances
    }

  type Aux[F <: Fx, A[_], Out0 <: HList] = Eff[F, A] {
    type Out = Out0
  }

  implicit def fxJustEff[M[_[_]], F[_]](implicit fa: M[F]): Aux[Just[M], F, M[F] :: HNil] =
    new Eff[Just[M], F] {
      override type Out = M[F] :: HNil
      override def instances: Out = fa :: HNil
    }

  implicit def fxCons[F[_], M[_[_]], HE, T <: Fx, TE <: HList](
    implicit HA: M[F],
    T: Aux[T, F, TE]
  ): Aux[M :*: T, F, M[F] :: TE] = new Eff[M :*: T, F] {
    override type Out = M[F] :: TE
    override def instances: Out = HA :: T.instances
  }

}

@algebra
trait Foo[S, F[_]] {
  def bar(a: S): F[String]
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

    val fileIO: FileIO[AppF] = StateFileIO.mapK(
      λ[State[Map[String, Vector[String]], ?] ~> AppF](
        _.transformS(_.fileIOState, (s, x) => s.copy(fileIOState = x))
      )
    )

    val keyValueStore: KeyValueStore[String, String, AppF] =
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
      Coproduct[KeyValueStore.KeyValueStoreOp[String, String, ?],
                Coproduct[Logging.LoggingOp, UserInteraction.UserInteractionOp, ?],
                A]

    implicit val userInteractionInject: Inject[UserInteraction.UserInteractionOp, OpsCoproduct] =
      Inject.catsFreeRightInjectInstance(
        Inject.catsFreeRightInjectInstance(Inject.catsFreeReflexiveInjectInstance)
      )

    type Effects = Logging :*: Just[UserInteraction]

    def p[F[_]: Monad](ops: Ops[F, Logging[F] :: UserInteraction[F] :: HNil]): F[Unit] =
      for {
        _ <- ops[UserInteraction].writeLn("")
        x <- ops[Logging].debug("dsad")
      } yield ()

    def p2[F[_]: Monad](implicit ops: EffC[Effects, F]): F[Unit] =
      for {
        _ <- ops[UserInteraction].writeLn("")
        x <- ops[Logging].debug("dsad")
      } yield ()

    def freeProgram =
      program[Free[OpsCoproduct, ?]]

    val interpreters = keyValueStore :&:
        FileLogging[Term[FileIO, ?]]("log.dat")
          .transpile(fileIO) :&:
          ConsoleUserInteraction[AppF]

    val out =
      termProgram(interpreters).flatMap { _ =>
        freeProgram.foldMap(interpreters.asFunctionK)
      }

    val result = out.run(AppState(Map.empty, Map.empty))

    println(result.value)

    ()

  }
}
