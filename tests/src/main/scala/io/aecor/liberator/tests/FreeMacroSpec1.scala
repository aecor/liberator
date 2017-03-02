package io.aecor.liberator.tests

import io.aecor.liberator.macros.FreeMacro
import org.scalatest.{ FlatSpec, Matchers }

import scala.meta._

class FreeMacroSpec1 extends FlatSpec with Matchers {

  "Free macro" should "generate boilerplate" in {

    val defn =
      source"""
        trait KVS[K, V, F[_]] {
            def getValue(key: K): F[Option[V]]
          }
      """.collect {
        case t: Defn.Trait => t
      }.head

    val block = Term.Block(
      """
        |  trait KVS[K, V, F[_]] {
        |    def getValue(key: K): F[Option[V]]
        |  }
        |  object KVS {
        |    def apply[K, V, F[_]](implicit instance: KVS[K, V, F]): KVS[K, V, F] = instance
        |    sealed abstract class KVSFree[K, V, A] extends Product with Serializable
        |    object KVSFree { final case class GetValue[K, V](key: K) extends KVSFree[K, V, Option[V]] }
        |    type AppliedKVSFree2[K, V] = { type Out[A] = KVSFree[K, V, A] }
        |    def fromFunctionK[K, V, F[_]](f: _root_.cats.arrow.FunctionK[AppliedKVSFree2[K, V]#Out, F]): KVS[K, V, F] = new KVS[K, V, F] { def getValue(key: K): F[Option[V]] = f(KVSFree.GetValue(key)) }
        |    def toFunctionK[K, V, F[_]](ops: KVS[K, V, F]): _root_.cats.arrow.FunctionK[AppliedKVSFree2[K, V]#Out, F] = new _root_.cats.arrow.FunctionK[AppliedKVSFree2[K, V]#Out, F] {
        |      def apply[A](op: KVSFree[K, V, A]): F[A] = op match {
        |        case KVSFree.GetValue(key) => ops.getValue(key)
        |      }
        |    }
        |    type FreeHelper1[F[_]] = { type Out[A] = _root_.cats.free.Free[F, A] }
        |    implicit def freeInstance[K, V, F[_]](implicit inject: _root_.cats.free.Inject[AppliedKVSFree2[K, V]#Out, F]): KVS[K, V, FreeHelper1[F]#Out] = fromFunctionK(new _root_.cats.arrow.FunctionK[AppliedKVSFree2[K, V]#Out, FreeHelper1[F]#Out] { def apply[A](op: KVSFree[K, V, A]): _root_.cats.free.Free[F, A] = _root_.cats.free.Free.inject(op) })
        |    type AppliedKVS3[K, V] = { type Out[F[_]] = KVS[K, V, F] }
        |    implicit def liberatorFreeAlgebra[K, V]: io.aecor.liberator.FreeAlgebra.Aux[AppliedKVS3[K, V]#Out, AppliedKVSFree2[K, V]#Out] = new io.aecor.liberator.FreeAlgebra[AppliedKVS3[K, V]#Out] {
        |      type Out[A] = KVSFree[K, V, A]
        |      override def apply[F[_]](of: KVS[K, V, F]): _root_.cats.arrow.FunctionK[AppliedKVSFree2[K, V]#Out, F] = KVS.toFunctionK(of)
        |    }
        |  }
      """.stripMargin.parse[Source].get.stats
    )

    FreeMacro(defn, None).toString() shouldEqual block.toString()
  }

}
