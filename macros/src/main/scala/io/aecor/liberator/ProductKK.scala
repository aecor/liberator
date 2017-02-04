package io.aecor.liberator

import cats.data.Coproduct
import cats.~>

final case class ProductKK[F[_[_]], G[_[_]], A[_]](fa: F[A], ga: G[A]) {
  def toFunctionK(implicit faf: FreeAlgebra[F],
                  fag: FreeAlgebra[G]): Coproduct[faf.Out, fag.Out, ?] ~> A =
    new (Coproduct[faf.Out, fag.Out, ?] ~> A) {
      override def apply[X](cop: Coproduct[faf.Out, fag.Out, X]): A[X] =
        cop.fold(faf.toFunctionK(fa), fag.toFunctionK(ga))
    }
}

sealed trait ProjectK[F[_[_]], G[_[_]]] {
  def prj[A[_]](fa: F[A]): G[A]
}

private[liberator] sealed abstract class ProjectKInstances {
  implicit def liberatorReflexiveProjectInstance[F[_[_]]]: ProjectK[F, F] =
    new ProjectK[F, F] {
      override def prj[A[_]](fa: F[A]): F[A] = fa
    }

  implicit def liberatorLeftProjectInstance[F[_[_]], G[_[_]]]: ProjectK[ProductKK[F, G, ?[_]], F] =
    new ProjectK[ProductKK[F, G, ?[_]], F] {
      override def prj[A[_]](prod: ProductKK[F, G, A]): F[A] =
        prod.fa
    }

  implicit def liberatorRightProjectInstance[F[_[_]], G[_[_]], H[_[_]]](
    implicit I: ProjectK[G, F]
  ): ProjectK[ProductKK[H, G, ?[_]], F] =
    new ProjectK[ProductKK[H, G, ?[_]], F] {
      override def prj[A[_]](fa: ProductKK[H, G, A]): F[A] =
        I.prj(fa.ga)
    }
}

object ProjectK extends ProjectKInstances {
  def apply[F[_[_]], G[_[_]]](implicit instance: ProjectK[F, G]): ProjectK[F, G] = instance
}
