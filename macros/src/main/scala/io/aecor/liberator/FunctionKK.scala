package io.aecor.liberator

trait FunctionKK[M[_[_]], N[_[_]]] {
  def apply[F[_]](fa: M[F]): N[F]
}
