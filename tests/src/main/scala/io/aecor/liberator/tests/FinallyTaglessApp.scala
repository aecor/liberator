package io.aecor.liberator.tests

import cats.data.{ EitherT, State }
import cats.implicits._
import cats.{ Monad, ~> }
import io.aecor.liberator.macros.free
import io.aecor.liberator.syntax._
import io.aecor.liberator.{ ProductKK, Term }

@free
trait Api[F[_]] {
  def doThing(aThing: String, params: Map[String, String]): F[Either[String, String]]
}

@free
trait FooService[F[_]] {
  def doFoo(params: Map[String, String]): F[String]
}

object FooService {
  object terms {
    def doFoo(params: Map[String, String]): Term[FooService, String] =
      new Term[FooService, String] {
        override def apply[F[_]](alg: FooService[F])(implicit F: Monad[F]): F[String] =
          alg.doFoo(params)
      }
  }

}

@free
trait FooServiceComponent1[F[_]] {
  def doFooStep1(params: Map[String, String]): F[String]
}

object FooServiceComponent1 {
  object terms {
    def doFooStep1(params: Map[String, String]): Term[FooServiceComponent1, String] =
      new Term[FooServiceComponent1, String] {
        override def apply[F[_]](alg: FooServiceComponent1[F])(implicit F: Monad[F]): F[String] =
          alg.doFooStep1(params)
      }
  }
}

@free
trait FooServiceComponent2[F[_]] {
  def doFooStep2(params: Map[String, String]): F[String]
}

object FooServiceComponent2 {
  object terms {
    def doFooStep2(params: Map[String, String]): Term[FooServiceComponent2, String] =
      new Term[FooServiceComponent2, String] {
        override def apply[F[_]](alg: FooServiceComponent2[F])(implicit F: Monad[F]): F[String] =
          alg.doFooStep2(params)
      }
  }
}

@free
trait BarService[F[_]] {
  def doBar(params: Map[String, String]): F[String]
}

object BarService {
  object terms {
    def doBar(params: Map[String, String]): Term[BarService, String] =
      new Term[BarService, String] {
        override def apply[F[_]](alg: BarService[F])(implicit F: Monad[F]): F[String] =
          alg.doBar(params)
      }
  }
}

@free
trait FileIO[F[_]] {
  def appendLine(filePath: String, line: String): F[Unit]
}

object FileIO {
  object terms {
    def appendLine(filePath: String, line: String): Term[FileIO, Unit] =
      new Term[FileIO, Unit] {
        override def apply[F[_]](alg: FileIO[F])(implicit F: Monad[F]): F[Unit] =
          alg.appendLine(filePath, line)
      }
  }
}

object ApiInTermsOfFooServiceAndBarService
    extends Api[Term[ProductKK[FooService, BarService, ?[_]], ?]] {
  import BarService.terms._
  import FooService.terms._
  override def doThing(
    aThing: String,
    params: Map[String, String]
  ): Term[ProductKK[FooService, BarService, ?[_]], Either[String, String]] =
    aThing match {
      case "foo" =>
        doFoo(params).map(_.asRight[String]).project
      case "bar" =>
        doBar(params).map(_.asRight[String]).project
      case other =>
        Term.pure("Unknown command".asLeft)
    }
}

object FooServiceInTermsOfComponent1AndComponent2
    extends FooService[Term[ProductKK[FooServiceComponent1, FooServiceComponent2, ?[_]], ?]] {
  import FooServiceComponent1.terms._
  import FooServiceComponent2.terms._
  override def doFoo(
    params: Map[String, String]
  ): Term[ProductKK[FooServiceComponent1, FooServiceComponent2, ?[_]], String] =
    for {
      r1 <- doFooStep1(params).project[ProductKK[FooServiceComponent1, FooServiceComponent2, ?[_]]]
      r2 <- doFooStep2(params).project[ProductKK[FooServiceComponent1, FooServiceComponent2, ?[_]]]
    } yield s"$r1 && $r2"

}

object FooServiceComponent1InTermsOfKeyValueStore
    extends FooServiceComponent1[Term[KeyValueStore[String, String, ?[_]], ?]] {
  override def doFooStep1(
    params: Map[String, String]
  ): Term[KeyValueStore[String, String, ?[_]], String] =
    params.toVector
      .traverse {
        case (key, value) =>
          KeyValueStore.terms.setValue(key, value)
      }
      .map { x =>
        s"Inserted ${x.length} elements"
      }

}

object FooServiceComponent2InTermsOfFileIO extends FooServiceComponent2[Term[FileIO, ?]] {
  import FileIO.terms._
  override def doFooStep2(params: Map[String, String]): Term[FileIO, String] =
    params.toVector
      .traverse {
        case (key, value) =>
          appendLine("fooServiceComponent2.dat", s"$key -> $value")
      }
      .map(x => s"Appended ${x.length} lines")
}

object StateFileIO extends FileIO[State[Map[String, Vector[String]], ?]] {
  override def appendLine(filePath: String,
                          line: String): State[Map[String, Vector[String]], Unit] =
    State.modify { state =>
      state.updated(filePath, state.getOrElse(filePath, Vector.empty) :+ line)
    }
}

object BarServiceInTermsOfFileIO extends BarService[Term[FileIO, ?]] {
  import FileIO.terms._
  override def doBar(params: Map[String, String]): Term[FileIO, String] =
    params.toVector
      .traverse {
        case (key, value) =>
          appendLine("barService.dat", s"$key; $value")
      }
      .map(x => s"Appended ${x.length} lines")
}

object FinallyTaglessApp extends App {

  case class AppState(keyValueStoreState: Map[String, String],
                      fileIOState: Map[String, Vector[String]])

  type F[A] = State[AppState, A]

  val fileIOF: FileIO[F] = StateFileIO.mapK(
    Lambda[State[Map[String, Vector[String]], ?] ~> F](
      _.transformS(_.fileIOState, (s, x) => s.copy(fileIOState = x))
    )
  )

  val keyValueStoreF: KeyValueStore[String, String, F] =
    StateKeyValueStore[String, String].mapK(
      Lambda[State[Map[String, String], ?] ~> F](
        _.transformS(_.keyValueStoreState, (s, x) => s.copy(keyValueStoreState = x))
      )
    )

  val component1: FooServiceComponent1[F] =
    FooServiceComponent1InTermsOfKeyValueStore.transpile(keyValueStoreF)

  val component2: FooServiceComponent2[F] =
    FooServiceComponent2InTermsOfFileIO.transpile(fileIOF)

  val fooService: FooService[F] =
    FooServiceInTermsOfComponent1AndComponent2.transpile(ProductKK(component1, component2))

  val barService: BarService[F] =
    BarServiceInTermsOfFileIO.transpile(fileIOF)

  val api: Api[F] =
    ApiInTermsOfFooServiceAndBarService.transpile(ProductKK(fooService, barService))

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
