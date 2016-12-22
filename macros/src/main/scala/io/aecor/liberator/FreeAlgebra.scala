package io.aecor.liberator

import cats.data.Coproduct
import cats.~>
import shapeless.Lazy

/**
  * Represents the ability to convert some O[F] to natural transformation from some Out to F
  *
  * Where Out is called free algebra of O
  *
  * @tparam O
  */
trait FreeAlgebra[O[_[_]]] {
  type Out[_]
  def apply[F[_]](of: O[F]): Out ~> F
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
