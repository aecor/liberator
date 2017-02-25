package io.aecor.liberator.data

import cats.data.Coproduct
import cats.~>
import io.aecor.liberator.Algebra
import io.aecor.liberator.Algebra.Aux
import shapeless.Lazy

final case class ProductKK[F[_[_]], G[_[_]], A[_]](fa: F[A], ga: G[A]) {
  def :&:[L[_[_]]](la: L[A]): ProductKK[L, ProductKK[F, G, ?[_]], A] =
    ProductKK(la, this)
}

object ProductKK {
  implicit def opsInstance[F[_[_]], G[_[_]], FOp[_], GOp[_]](
    implicit f: Lazy[Aux[F, FOp]],
    g: Lazy[Aux[G, GOp]]
  ): Aux[ProductKK[F, G, ?[_]], Coproduct[FOp, GOp, ?]] =
    new Algebra[ProductKK[F, G, ?[_]]] {
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
  class ProductKKIdOps[F[_[_]], A[_]](val self: F[A]) extends AnyVal {
    def :&:[G[_[_]]](ga: G[A]): ProductKK[G, F, A] = ProductKK(ga, self)
  }
  trait ProductKKSyntax {
    implicit def toProductKKSyntax[F[_[_]], A[_]](self: F[A]): ProductKKIdOps[F, A] =
      new ProductKKIdOps(self)
  }
}
