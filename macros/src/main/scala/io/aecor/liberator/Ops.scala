package io.aecor.liberator

import cats.data.Coproduct
import cats.~>
import shapeless.Lazy

/**
  *
  * Represents the ability to convert F[A] to Out ~> A and back
  *
  */
trait Ops[F[_[_]]] {
  type Out[_]
  def toFunctionK[A[_]](fa: F[A]): Out ~> A
  def fromFunctionK[A[_]](nat: Out ~> A): F[A]
  def mapK[A[_], B[_]](fa: F[A])(f: A ~> B): F[B] =
    fromFunctionK(toFunctionK(fa).andThen(f))
}

object Ops {

  def apply[F[_[_]]](implicit ops: Ops[F]): Aux[F, ops.Out] = ops

  type Aux[F[_[_]], Out0[_]] = Ops[F] {
    type Out[A] = Out0[A]
  }

  implicit def productKKOpsInstance[F[_[_]], G[_[_]], FOp[_], GOp[_]](
    implicit f: Lazy[Aux[F, FOp]],
    g: Lazy[Aux[G, GOp]]
  ): Aux[ProductKK[F, G, ?[_]], Coproduct[FOp, GOp, ?]] =
    new Ops[ProductKK[F, G, ?[_]]] {
      override type Out[A] = Coproduct[FOp, GOp, A]
      override def toFunctionK[A[_]](of: ProductKK[F, G, A]): Coproduct[FOp, GOp, ?] ~> A =
        λ[Coproduct[FOp, GOp, ?] ~> A](
          _.fold(f.value.toFunctionK(of.fa), g.value.toFunctionK(of.ga))
        )

      override def fromFunctionK[A[_]](fa: Coproduct[FOp, GOp, ?] ~> A): ProductKK[F, G, A] =
        ProductKK(
          f.value.fromFunctionK(λ[FOp ~> A](x => fa(Coproduct.leftc(x)))),
          g.value.fromFunctionK(λ[GOp ~> A](x => fa(Coproduct.rightc(x))))
        )
    }
}

final class OpsSyntaxIdOps[F[_[_]], A[_]](val self: F[A]) extends AnyVal {
  def mapK[B[_]](f: A ~> B)(implicit ops: Ops[F]): F[B] =
    ops.mapK(self)(f)
  def asFunctionK(implicit ops: Ops[F]): ops.Out ~> A =
    ops.toFunctionK(self)
}
trait FreeAlgebraSyntax {
  implicit def toFreeAlgebraSyntaxIdOps[F[_[_]], A[_]](fa: F[A]): OpsSyntaxIdOps[F, A] =
    new OpsSyntaxIdOps(fa)
}
