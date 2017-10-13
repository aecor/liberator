package io.aecor.liberator

import cats.~>

/**
  * Higher-kinded Functor
  */
trait FunctorK[M[_[_]]] {
  def mapK[F[_], G[_]](mf: M[F], fg: F ~> G): M[G]
}

object FunctorK {
  object syntax extends FunctorKSyntax

  final class OpsSyntaxIdOps[M[_[_]], F[_]](val self: M[F]) extends AnyVal {
    def mapK[G[_]](f: F ~> G)(implicit M: FunctorK[M]): M[G] =
      M.mapK(self, f)
  }

  trait FunctorKSyntax {
    @inline implicit def toFunctorKOps[M[_[_]], F[_]](self: M[F]): OpsSyntaxIdOps[M, F] =
      new OpsSyntaxIdOps(self)
  }
}
