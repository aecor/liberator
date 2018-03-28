package io.aecor.liberator

trait Invocation[M[_[_]], A] {
  def invoke[F[_]](target: M[F]): F[A]
}
