package io.aecor.liberator

import cats.data.Coproduct
import cats.~>
import shapeless.Lazy

/**
  * Represents the ability to convert F[A] to natural transformation from Out to A
  *
  * Out is called a free algebra of F
  *
  */
trait FreeAlgebra[F[_[_]]] {
  type Out[_]
  def toFunctionK[A[_]](fa: F[A]): Out ~> A
  def fromFunctionK[A[_]](nat: Out ~> A): F[A]
  def mapK[A[_], B[_]](fa: F[A])(f: A ~> B): F[B] =
    fromFunctionK(toFunctionK(fa).andThen(f))
}

object FreeAlgebra {

  def apply[F[_[_]]](implicit freeAlgebra: FreeAlgebra[F]): Aux[F, freeAlgebra.Out] = freeAlgebra

  type Aux[F[_[_]], Out0[_]] = FreeAlgebra[F] {
    type Out[A] = Out0[A]
  }

  implicit def product[F[_[_]], G[_[_]], FOp[_], GOp[_]](
    implicit f: Lazy[Aux[F, FOp]],
    g: Lazy[Aux[G, GOp]]
  ): Aux[ProductKK[F, G, ?[_]], Coproduct[FOp, GOp, ?]] =
    new FreeAlgebra[ProductKK[F, G, ?[_]]] {
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

final class FreeAlgebraSyntaxIdOps[F[_[_]], A[_]](val self: F[A]) extends AnyVal {
  def mapK[B[_]](f: A ~> B)(implicit freeAlgebra: FreeAlgebra[F]): F[B] =
    freeAlgebra.mapK(self)(f)
  def asFunctionK(implicit freeAlgebra: FreeAlgebra[F]): freeAlgebra.Out ~> A =
    freeAlgebra.toFunctionK(self)
}

final class FreeAlgebraSyntaxFunctionKOps[A[_], B[_]](val self: A ~> B) extends AnyVal {
  def asAlg[F[_[_]]](implicit freeAlgebra: FreeAlgebra.Aux[F, A]): F[B] =
    freeAlgebra.fromFunctionK(self)
}

trait FreeAlgebraSyntax {
  implicit def toFreeAlgebraSyntaxIdOps[F[_[_]], A[_]](fa: F[A]): FreeAlgebraSyntaxIdOps[F, A] =
    new FreeAlgebraSyntaxIdOps(fa)

  implicit def toFreeAlgebraSyntaxFunctionKOps[A[_], B[_]](
    self: A ~> B
  ): FreeAlgebraSyntaxFunctionKOps[A, B] =
    new FreeAlgebraSyntaxFunctionKOps(self)
}
