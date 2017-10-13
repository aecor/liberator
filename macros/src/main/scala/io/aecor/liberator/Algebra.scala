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
}

object Algebra {

  def apply[F[_[_]]](implicit ops: Algebra[F]): Aux[F, ops.Out] = ops

  type Aux[F[_[_]], Out0[_]] = Algebra[F] {
    type Out[A] = Out0[A]
  }

  object syntax extends AlgebraSyntax

  trait AlgebraSyntax {
    implicit def toAlgebraOps[F[_[_]], A[_]](fa: F[A]): OpsSyntaxIdOps[F, A] =
      new OpsSyntaxIdOps(fa)
  }

  final class OpsSyntaxIdOps[F[_[_]], A[_]](val self: F[A]) extends AnyVal {
    def asFunctionK(implicit algebra: Algebra[F]): algebra.Out ~> A =
      algebra.toFunctionK(self)
  }
}
