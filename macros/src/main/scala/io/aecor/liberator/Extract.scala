package io.aecor.liberator

import io.aecor.liberator.data.ProductKK

sealed trait Extract[M[_[_]], N[_[_]]] {
  def apply[F[_]](fa: M[F]): N[F]
}

object Extract extends ExtractInstances {
  def apply[M[_[_]], N[_[_]]](implicit instance: Extract[M, N]): Extract[M, N] = instance
}

private[liberator] sealed abstract class ExtractInstances {
  implicit def liberatorReflexiveExtractInstance[F[_[_]]]: Extract[F, F] =
    new Extract[F, F] {
      override def apply[A[_]](fa: F[A]): F[A] = fa
    }

  implicit def liberatorLeftExtractInstance[F[_[_]], G[_[_]]]: Extract[ProductKK[F, G, ?[_]], F] =
    new Extract[ProductKK[F, G, ?[_]], F] {
      override def apply[A[_]](prod: ProductKK[F, G, A]): F[A] =
        prod.fa
    }

  implicit def liberatorRightExtractInstance[F[_[_]], G[_[_]], H[_[_]]](
    implicit extract: Extract[G, F]
  ): Extract[ProductKK[H, G, ?[_]], F] =
    new Extract[ProductKK[H, G, ?[_]], F] {
      override def apply[A[_]](prod: ProductKK[H, G, A]): F[A] =
        extract(prod.ga)
    }
}
