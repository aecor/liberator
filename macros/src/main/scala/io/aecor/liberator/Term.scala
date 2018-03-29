package io.aecor.liberator

import cats.{ Apply, CoflatMap, Functor, Group, Monad, ~> }
import io.aecor.liberator.Term.{ Ap, Effect, FlatMap, Pure }

import scala.annotation.tailrec

sealed abstract class Term[M[_[_]], A] { outer =>
  final def apply[F[_]](ops: M[F])(implicit F: Monad[F]): F[A] =
    F.tailRecM(this)(_.step match {
      case Pure(a)            => F.pure(Right(a))
      case FlatMap(c, g)      => F.map(c(ops))(cc => Left(g(cc)))
      case Effect(invocation) => F.map(invocation.invoke(ops))(cc => Right(cc))
      case Ap(fa, fab)        => F.map(F.ap(fab(ops))(fa(ops)))(cc => Right(cc))
    })

  @tailrec
  final def step: Term[M, A] = this match {
    case FlatMap(FlatMap(c, f), g) => c.flatMap(cc => f(cc).flatMap(g)).step
    case FlatMap(Pure(a), f)       => f(a).step
    case x                         => x
  }

  final def flatMap[B](f: A => Term[M, B]): Term[M, B] = Term.FlatMap(this, f)

  final def map[B](f: A => B): Term[M, B] = flatMap(a => Term.pure(f(a)))

  final def contramapK[G[_[_]]](f: FunctionKK[G, M]): Term[G, A] = this match {
    case FlatMap(fa, fm) => fa.contramapK(f).flatMap(a => fm(a).contramapK(f))
    case Pure(a)         => Pure(a)
    case Effect(invocation) =>
      Effect(new Invocation[G, A] {
        override def invoke[F[_]](target: G[F]): F[A] = invocation.invoke(f(target))
      })
    case Ap(fa, fab) => Ap(fa.contramapK(f), fab.contramapK(f))
  }

  final def ap[B](fab: Term[M, (A => B)]): Term[M, B] = Term.Ap(this, fab)
}

object Term extends TermInstances {
  private case class FlatMap[M[_[_]], A, B](fa: Term[M, A], f: A => Term[M, B]) extends Term[M, B]
  private case class Pure[M[_[_]], A](a: A) extends Term[M, A]
  private case class Effect[M[_[_]], A](value: Invocation[M, A]) extends Term[M, A]
  private case class Ap[M[_[_]], A, B](fa: Term[M, A], tb: Term[M, A => B]) extends Term[M, B]

  type Invocation[M[_[_]], A] = io.aecor.liberator.Invocation[M, A]

  def lift[M[_[_]], A](invocation: Invocation[M, A]): Term[M, A] = Effect(invocation)
  def pure[M[_[_]], A](a: A): Term[M, A] = Pure(a)

  def transpile[M[_[_]], N[_[_]], F[_]: Monad](
    mtn: M[Term[N, ?]],
    nf: N[F]
  )(implicit ev: Algebra[M], M: FunctorK[M]): M[F] =
    M.mapK(mtn, λ[Term[N, ?] ~> F](_(nf)))

}

final class TermSyntaxIdOps[M[_[_]], N[_[_]]](val self: M[Term[N, ?]]) extends AnyVal {
  def transpile[F[_]: Monad](nf: N[F])(implicit ev: Algebra[M], functorK: FunctorK[M]): M[F] =
    Term.transpile(self, nf)
}

trait TermSyntax {
  final implicit def toTermSyntaxIdOps[M[_[_]], N[_[_]]](a: M[Term[N, ?]]): TermSyntaxIdOps[M, N] =
    new TermSyntaxIdOps(a)
}

private[liberator] trait TermInstances {
  implicit def catsMonadInstance[M[_[_]]]: Monad[Term[M, ?]] with CoflatMap[Term[M, ?]] =
    new Monad[Term[M, ?]] with CoflatMap[Term[M, ?]] {
      override def tailRecM[A, B](a: A)(f: (A) => Term[M, Either[A, B]]): Term[M, B] =
        f(a).flatMap {
          case Left(x)  => tailRecM(x)(f)
          case Right(b) => pure(b)
        }

      override def ap[A, B](ff: Term[M, (A) => B])(fa: Term[M, A]): Term[M, B] = fa.ap(ff)

      override def flatMap[A, B](fa: Term[M, A])(f: (A) => Term[M, B]): Term[M, B] =
        fa.flatMap(f)

      override def pure[A](x: A): Term[M, A] = Term.pure(x)

      override def coflatMap[A, B](fa: Term[M, A])(f: (Term[M, A]) => B): Term[M, B] =
        pure(f(fa))
    }

  implicit def catsGroupInstanceForTerm[M[_[_]], A](implicit A: Group[A]): Group[Term[M, A]] =
    new Group[Term[M, A]] {
      override def empty: Term[M, A] = Term.pure(A.empty)

      override def combine(x: Term[M, A], y: Term[M, A]): Term[M, A] =
        Apply[Term[M, ?]].map2(x, y)(A.combine)

      override def inverse(a: Term[M, A]): Term[M, A] = a.map(A.inverse)
    }

  implicit def liftGeneric[M[_[_]], N[_[_]]](implicit extract: Extract[M, N],
                                             algebra: Algebra[N]): N[Term[M, ?]] =
    algebra.fromFunctionK(new (algebra.Out ~> Term[M, ?]) {
      final override def apply[A](fa: algebra.Out[A]): Term[M, A] =
        Term.lift(new Invocation[M, A] {
          final override def invoke[F[_]](mf: M[F]): F[A] =
            algebra.toFunctionK(extract(mf))(fa)
        })
    })
}
