package io.aecor.liberator

trait FunctionKK[F[_[_]], G[_[_]]] {
  def apply[A[_]](fa: F[A]): G[A]
}
