package io.aecor.free

final case class ProductKK[F[_[_]], G[_[_]], A[_]](fa: F[A], ga: G[A])
