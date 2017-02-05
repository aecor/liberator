package io.aecor.liberator

import cats.{ CoflatMap, Monad, ~> }

trait Term[M[_[_]], A] { outer =>
  def apply[F[_]](ops: M[F])(implicit F: Monad[F]): F[A]

  final def flatMap[B](f: A => Term[M, B]): Term[M, B] = new Term[M, B] {
    override def apply[F[_]](ops: M[F])(implicit F: Monad[F]): F[B] =
      F.flatMap(outer(ops)) { a =>
        f(a)(ops)
      }
  }

  final def map[B](f: A => B): Term[M, B] = new Term[M, B] {
    override def apply[F[_]](ops: M[F])(implicit F: Monad[F]): F[B] =
      F.map(outer(ops))(f)
  }

  final def contramapK[G[_[_]]](f: FunctionKK[G, M]): Term[G, A] = new Term[G, A] {
    override def apply[F[_]](ops: G[F])(implicit F: Monad[F]): F[A] =
      outer(f(ops))
  }
}

object Term extends TermInstances {
  def pure[M[_[_]], A](a: A): Term[M, A] = new Term[M, A] {
    override def apply[F[_]](alg: M[F])(implicit F: Monad[F]): F[A] = F.pure(a)
  }

  def transpile[M[_[_]], N[_[_]], F[_]: Monad](mtn: M[Term[N, ?]],
                                               nf: N[F])(implicit ev: Ops[M]): M[F] =
    ev.mapK(mtn)(λ[Term[N, ?] ~> F](_(nf)))

}

final class TermSyntaxIdOps[M[_[_]], N[_[_]]](val self: M[Term[N, ?]]) extends AnyVal {
  def transpile[F[_]: Monad](nf: N[F])(implicit ev: Ops[M]): M[F] =
    Term.transpile(self, nf)
}

trait TermSyntax {
  implicit def toTermSyntaxIdOps[M[_[_]], N[_[_]]](a: M[Term[N, ?]]): TermSyntaxIdOps[M, N] =
    new TermSyntaxIdOps(a)
}

private[liberator] trait TermInstances {
  implicit def catsMonadInstance[M[_[_]]]: Monad[Term[M, ?]] with CoflatMap[Term[M, ?]] =
    new Monad[Term[M, ?]] with CoflatMap[Term[M, ?]] {
      override def tailRecM[A, B](a: A)(f: (A) => Term[M, Either[A, B]]): Term[M, B] =
        f(a).flatMap {
          case Left(x) => tailRecM(x)(f)
          case Right(b) => pure(b)
        }

      override def flatMap[A, B](fa: Term[M, A])(f: (A) => Term[M, B]): Term[M, B] =
        fa.flatMap(f)

      override def pure[A](x: A): Term[M, A] = Term.pure(x)

      override def coflatMap[A, B](fa: Term[M, A])(f: (Term[M, A]) => B): Term[M, B] =
        pure(f(fa))
    }
  implicit def generic[M[_[_]], N[_[_]]](implicit extract: Extract[M, N],
                                         ops: Ops[N]): N[Term[M, ?]] =
    ops.fromFunctionK(new (ops.Out ~> Term[M, ?]) {
      override def apply[A](fa: ops.Out[A]): Term[M, A] =
        new Term[M, A] {
          override def apply[F[_]](alg: M[F])(implicit F: Monad[F]): F[A] =
            ops.toFunctionK(extract(alg))(fa)
        }
    })
}
