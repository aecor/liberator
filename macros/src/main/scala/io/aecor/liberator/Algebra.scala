package io.aecor.liberator

import cats.~>

/**
  *
  * `Algebra.Aux[M, Out]` is an isomorphism between `M[A]` and `Out ~> A`
  *
  */
trait Algebra[M[_[_]]] {
  type Out[_]
  def toFunctionK[F[_]](fa: M[F]): Out ~> F
  def fromFunctionK[F[_]](nat: Out ~> F): M[F]
  final def mapK[F[_], G[_]](fa: M[F])(f: F ~> G): M[G] =
    fromFunctionK(toFunctionK(fa).andThen(f))
}

object Algebra {

  def apply[F[_[_]]](implicit ops: Algebra[F]): Aux[F, ops.Out] = ops

  type Aux[F[_[_]], Out0[_]] = Algebra[F] {
    type Out[A] = Out0[A]
  }

  object ops extends AlgebraSyntax

  trait AlgebraSyntax {
    implicit def toAlgebraOps[F[_[_]], A[_]](fa: F[A]): OpsSyntaxIdOps[F, A] =
      new OpsSyntaxIdOps(fa)
  }

  final class OpsSyntaxIdOps[F[_[_]], A[_]](val self: F[A]) extends AnyVal {
    def mapK[B[_]](f: A ~> B)(implicit algebra: Algebra[F]): F[B] =
      algebra.mapK(self)(f)

    def asFunctionK(implicit algebra: Algebra[F]): algebra.Out ~> A =
      algebra.toFunctionK(self)
  }
}
