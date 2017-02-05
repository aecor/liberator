package io.aecor.liberator

import cats.data.Coproduct
import cats.~>

final case class ProductKK[F[_[_]], G[_[_]], A[_]](fa: F[A], ga: G[A])
