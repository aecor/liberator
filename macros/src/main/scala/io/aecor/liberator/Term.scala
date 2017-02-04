package io.aecor.liberator

import cats.{ Monad, ~> }

trait Term[Alg[_[_]], A] { outer =>
  def apply[F[_]](alg: Alg[F])(implicit F: Monad[F]): F[A]

  final def flatMap[B](f: A => Term[Alg, B]): Term[Alg, B] = new Term[Alg, B] {
    override def apply[F[_]](alg: Alg[F])(implicit F: Monad[F]): F[B] =
      F.flatMap(outer(alg)) { a =>
        f(a)(alg)
      }
  }

  final def map[B](f: A => B): Term[Alg, B] = new Term[Alg, B] {
    override def apply[F[_]](alg: Alg[F])(implicit F: Monad[F]): F[B] =
      F.map(outer(alg))(f)
  }

  final def contramapK[G[_[_]]](f: FunctionKK[G, Alg]): Term[G, A] = new Term[G, A] {
    override def apply[F[_]](alg: G[F])(implicit F: Monad[F]): F[A] =
      outer(f(alg))
  }
  def project[G[_[_]]](implicit projectK: ProjectK[G, Alg]): Term[G, A] =
    new Term[G, A] {
      override def apply[F[_]](alg: G[F])(implicit F: Monad[F]): F[A] =
        outer(projectK.prj(alg))
    }

}

object Term extends TermInstances {
  def pure[Alg[_[_]], A](a: A): Term[Alg, A] = new Term[Alg, A] {
    override def apply[F[_]](alg: Alg[F])(implicit F: Monad[F]): F[A] = F.pure(a)
  }
  def project[M[_[_]], L[_[_]]]: TermProjectPartiallyApplied[M, L] =
    new TermProjectPartiallyApplied

  final class TermProjectPartiallyApplied[M[_[_]], L[_[_]]] private[liberator] {
    def apply[A](fa: Term[L, A])(implicit I: ProjectK[M, L]): Term[M, A] =
      new Term[M, A] {
        override def apply[F[_]](alg: M[F])(implicit F: Monad[F]): F[A] =
          fa(I.prj(alg))
      }
  }
  def transpile[M[_[_]], N[_[_]], F[_]: Monad](mtn: M[Term[N, ?]],
                                               nf: N[F])(implicit ev: FreeAlgebra[M]): M[F] =
    ev.mapK(mtn)(Lambda[Term[N, ?] ~> F](_(nf)))

}

final class TermSyntaxIdOps[M[_[_]], N[_[_]]](val self: M[Term[N, ?]]) extends AnyVal {
  def transpile[F[_]: Monad](nf: N[F])(implicit ev: FreeAlgebra[M]): M[F] =
    Term.transpile(self, nf)
}

trait TermSyntax {
  implicit def toTermSyntaxIdOps[M[_[_]], N[_[_]]](a: M[Term[N, ?]]): TermSyntaxIdOps[M, N] =
    new TermSyntaxIdOps(a)
}

private[liberator] trait TermInstances {
  implicit def catsMonadInstance[M[_[_]]]: Monad[Term[M, ?]] = new Monad[Term[M, ?]] {
    override def tailRecM[A, B](a: A)(f: (A) => Term[M, Either[A, B]]): Term[M, B] =
      f(a).flatMap {
        case Left(x) => tailRecM(x)(f)
        case Right(b) => pure(b)
      }

    override def flatMap[A, B](fa: Term[M, A])(f: (A) => Term[M, B]): Term[M, B] =
      fa.flatMap(f)

    override def pure[A](x: A): Term[M, A] = Term.pure(x)
  }
}
