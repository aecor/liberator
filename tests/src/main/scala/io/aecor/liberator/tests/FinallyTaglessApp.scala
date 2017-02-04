package io.aecor.liberator.tests

import cats.data.{ EitherT, State }
import cats.implicits._
import cats.{ Monad, ~> }
import io.aecor.liberator.macros.free
import io.aecor.liberator.syntax._
import io.aecor.liberator.tests.FooServiceComponent1.TermF
import io.aecor.liberator.{ ProductKK, ProjectK, Term }

@free
trait Api[F[_]] {
  def doThing(aThing: String, params: Map[String, String]): F[Either[String, String]]
}

@free
trait FooService[F[_]] {
  def doFoo(params: Map[String, String]): F[String]
}

object FooService {
  implicit def instanceTerm[M[_[_]]](
    implicit pr: ProjectK[M, FooService]
  ): FooService[TermF[M]#T] =
    new FooService[TermF[M]#T] {
      override def doFoo(params: Map[String, String]): Term[M, String] =
        new Term[M, String] {
          override def apply[F[_]](alg: M[F])(implicit F: Monad[F]): F[String] =
            pr.prj(alg).doFoo(params)
        }
    }
}

@free
trait FooServiceComponent1[F[_]] {
  def doFooStep1(params: Map[String, String]): F[String]
}

object FooServiceComponent1 {
  type TermF[M[_[_]]] = {
    type T[A] = Term[M, A]
  }
  implicit def instanceTerm[M[_[_]]](
    implicit pr: ProjectK[M, FooServiceComponent1]
  ): FooServiceComponent1[TermF[M]#T] =
    new FooServiceComponent1[TermF[M]#T] {
      override def doFooStep1(params: Map[String, String]): Term[M, String] =
        new Term[M, String] {
          override def apply[F[_]](alg: M[F])(implicit F: Monad[F]): F[String] =
            pr.prj(alg).doFooStep1(params)
        }
    }
}

@free
trait FooServiceComponent2[F[_]] {
  def doFooStep2(params: Map[String, String]): F[String]
}

object FooServiceComponent2 {
  implicit def instanceTerm[M[_[_]]](
    implicit pr: ProjectK[M, FooServiceComponent2]
  ): FooServiceComponent2[TermF[M]#T] =
    new FooServiceComponent2[TermF[M]#T] {
      override def doFooStep2(params: Map[String, String]): Term[M, String] =
        new Term[M, String] {
          override def apply[F[_]](alg: M[F])(implicit F: Monad[F]): F[String] =
            pr.prj(alg).doFooStep2(params)
        }
    }
}

@free
trait BarService[F[_]] {
  def doBar(params: Map[String, String]): F[String]
}

object BarService {
  implicit def instanceTerm[M[_[_]]](
    implicit pr: ProjectK[M, BarService]
  ): BarService[TermF[M]#T] =
    new BarService[TermF[M]#T] {
      override def doBar(params: Map[String, String]): Term[M, String] =
        new Term[M, String] {
          override def apply[F[_]](alg: M[F])(implicit F: Monad[F]): F[String] =
            pr.prj(alg).doBar(params)
        }
    }
}

@free
trait FileIO[F[_]] {
  def appendLine(filePath: String, line: String): F[Unit]
}

object FileIO {
  implicit def instanceTerm[M[_[_]]](implicit pr: ProjectK[M, FileIO]): FileIO[TermF[M]#T] =
    new FileIO[TermF[M]#T] {
      override def appendLine(filePath: String, line: String): Term[M, Unit] =
        new Term[M, Unit] {
          override def apply[F[_]](alg: M[F])(implicit F: Monad[F]): F[Unit] =
            pr.prj(alg).appendLine(filePath, line)
        }
    }
}

class DefaultApi[F[_]: Monad: FooService: BarService] extends Api[F] {

  override def doThing(aThing: String, params: Map[String, String]): F[Either[String, String]] =
    aThing match {
      case "foo" =>
        FooService[F].doFoo(params).map(_.asRight[String])
      case "bar" =>
        BarService[F].doBar(params).map(_.asRight[String])
      case other =>
        "Unknown command".asLeft[String].pure[F]
    }
}

object DefaultApi {
  def terms: Api[Term[ProductKK[FooService, BarService, ?[_]], ?]] = new DefaultApi
}

class DefaultFooService[F[_]: Monad: FooServiceComponent1: FooServiceComponent2]
    extends FooService[F] {
  override def doFoo(params: Map[String, String]): F[String] =
    for {
      r1 <- FooServiceComponent1[F].doFooStep1(params)
      r2 <- FooServiceComponent2[F].doFooStep2(params)
    } yield s"$r1 && $r2"
}

object DefaultFooService {
  def terms: FooService[Term[ProductKK[FooServiceComponent1, FooServiceComponent2, ?[_]], ?]] =
    new DefaultFooService[Term[ProductKK[FooServiceComponent1, FooServiceComponent2, ?[_]], ?]]
}

class DefaultFooServiceComponent1[F[_]: Monad: KeyValueStore[String, String, ?[_]]]
    extends FooServiceComponent1[F] {
  override def doFooStep1(params: Map[String, String]): F[String] =
    params.toVector
      .traverse {
        case (key, value) =>
          KeyValueStore[String, String, F].setValue(key, value)
      }
      .map { x =>
        s"Inserted ${x.length} elements"
      }
}

object DefaultFooServiceComponent1 {
  def terms: FooServiceComponent1[Term[KeyValueStore[String, String, ?[_]], ?]] =
    new DefaultFooServiceComponent1[Term[KeyValueStore[String, String, ?[_]], ?]]
}

class DefaultFooServiceComponent2[F[_]: Monad: FileIO] extends FooServiceComponent2[F] {
  override def doFooStep2(params: Map[String, String]): F[String] =
    params.toVector
      .traverse {
        case (key, value) =>
          FileIO[F].appendLine("fooServiceComponent2.dat", s"$key -> $value")
      }
      .map(x => s"Appended ${x.length} lines")
}

object DefaultFooServiceComponent2 {
  def terms: FooServiceComponent2[Term[FileIO, ?]] =
    new DefaultFooServiceComponent2[Term[FileIO, ?]]
}

object StateFileIO extends FileIO[State[Map[String, Vector[String]], ?]] {
  override def appendLine(filePath: String,
                          line: String): State[Map[String, Vector[String]], Unit] =
    State.modify { state =>
      state.updated(filePath, state.getOrElse(filePath, Vector.empty) :+ line)
    }
}

class DefaultBarService[F[_]: FileIO: Monad] extends BarService[F] {
  override def doBar(params: Map[String, String]): F[String] =
    params.toVector
      .traverse {
        case (key, value) =>
          FileIO[F].appendLine("barService.dat", s"$key; $value")
      }
      .map(x => s"Appended ${x.length} lines")
}

object DefaultBarService {
  def terms: BarService[Term[FileIO, ?]] =
    new DefaultBarService[Term[FileIO, ?]]
}

object FinallyTaglessApp extends App {

  case class AppState(keyValueStoreState: Map[String, String],
                      fileIOState: Map[String, Vector[String]])

  type F[A] = State[AppState, A]

  val fileIO: FileIO[F] = StateFileIO.mapK(
    λ[State[Map[String, Vector[String]], ?] ~> F](
      _.transformS(_.fileIOState, (s, x) => s.copy(fileIOState = x))
    )
  )

  val keyValueStore: KeyValueStore[String, String, F] =
    StateKeyValueStore[String, String].mapK(
      λ[State[Map[String, String], ?] ~> F](
        _.transformS(_.keyValueStoreState, (s, x) => s.copy(keyValueStoreState = x))
      )
    )

  val api: Api[F] =
    DefaultApi.terms.transpile(
      ProductKK(
        DefaultFooService.terms.transpile(
          ProductKK(
            DefaultFooServiceComponent1.terms.transpile(keyValueStore),
            DefaultFooServiceComponent2.terms.transpile(fileIO)
          )
        ),
        DefaultBarService.terms.transpile(fileIO)
      )
    )

  val out = EitherT(api.doThing("foo", Map("b" -> "1", "a" -> "2")))
    .flatMapF { string =>
      api.doThing("bar", Map("out" -> string))
    }
    .flatMapF { x =>
      api.doThing(x, Map.empty)
    }
    .value
    .run(AppState(Map.empty, Map.empty))
    .value

  println(out)
}
