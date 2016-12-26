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
  def apply[A[_]](of: F[A]): Out ~> A
}

object FreeAlgebra {

  def apply[F[_[_]]](implicit freeAlgebra: FreeAlgebra[F]): Aux[F, freeAlgebra.Out] = freeAlgebra

  type Aux[F[_[_]], Out0[_]] = FreeAlgebra[F] {
    type Out[A] = Out0[A]
  }

  implicit def product[F[_[_]], G[_[_]], FreeF[_], FreeG[_]](
    implicit f: Lazy[Aux[F, FreeF]],
    g: Lazy[Aux[G, FreeG]]
  ): Aux[ProductKK[F, G, ?[_]], Coproduct[FreeF, FreeG, ?]] =
    new FreeAlgebra[ProductKK[F, G, ?[_]]] {
      override type Out[A] = Coproduct[FreeF, FreeG, A]
      override def apply[A[_]](of: ProductKK[F, G, A]): Coproduct[FreeF, FreeG, ?] ~> A =
        λ[Coproduct[FreeF, FreeG, ?] ~> A](_.fold(f.value(of.fa), g.value(of.ga)))
    }
}
