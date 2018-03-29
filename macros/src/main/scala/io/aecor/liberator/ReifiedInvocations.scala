package io.aecor.liberator

import cats.~>

trait ReifiedInvocations[M[_[_]]] extends FunctorK[M] {
  def invocations: M[Invocation[M, ?]]
  def mapInvocations[F[_]](f: Invocation[M, ?] ~> F): M[F] = mapK(invocations, f)
}

object ReifiedInvocations {
  def apply[M[_[_]]](implicit instance: ReifiedInvocations[M]): ReifiedInvocations[M] = instance
}
