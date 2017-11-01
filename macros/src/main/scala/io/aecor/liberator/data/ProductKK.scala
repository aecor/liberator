package io.aecor.liberator.data

import cats.data.EitherK
import cats.~>
import io.aecor.liberator.Algebra
import io.aecor.liberator.Algebra.Aux
import shapeless.Lazy

final case class ProductKK[M[_[_]], N[_[_]], F[_]](fa: M[F], ga: N[F]) {
  def :&:[L[_[_]]](la: L[F]): ProductKK[L, ProductKK[M, N, ?[_]], F] =
    ProductKK(la, this)
}

object ProductKK {
  implicit def opsInstance[F[_[_]], G[_[_]], FOp[_], GOp[_]](
    implicit f: Lazy[Aux[F, FOp]],
    g: Lazy[Aux[G, GOp]]
  ): Aux[ProductKK[F, G, ?[_]], EitherK[FOp, GOp, ?]] =
    new Algebra[ProductKK[F, G, ?[_]]] {
      override type Out[A] = EitherK[FOp, GOp, A]
      override def toFunctionK[A[_]](of: ProductKK[F, G, A]): EitherK[FOp, GOp, ?] ~> A =
        λ[EitherK[FOp, GOp, ?] ~> A](_.fold(f.value.toFunctionK(of.fa), g.value.toFunctionK(of.ga)))

      override def fromFunctionK[A[_]](fa: EitherK[FOp, GOp, ?] ~> A): ProductKK[F, G, A] =
        ProductKK(
          f.value.fromFunctionK(λ[FOp ~> A](x => fa(EitherK.leftc(x)))),
          g.value.fromFunctionK(λ[GOp ~> A](x => fa(EitherK.rightc(x))))
        )
    }

  class ProductKKIdOps[F[_[_]], A[_]](val self: F[A]) extends AnyVal {
    def :&:[G[_[_]]](ga: G[A]): ProductKK[G, F, A] = ProductKK(ga, self)
  }
  trait ProductKKSyntax {
    @inline final implicit def toProductKKSyntax[F[_[_]], A[_]](self: F[A]): ProductKKIdOps[F, A] =
      new ProductKKIdOps(self)
  }
}
