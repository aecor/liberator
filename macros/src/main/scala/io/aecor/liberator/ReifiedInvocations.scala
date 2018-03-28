package io.aecor.liberator

trait ReifiedInvocations[M[_[_]]] {
  def invocations: M[Invocation[M, ?]]
}
